package billingapp

import activitycore.{Window, Session, Sessions}

import billingcore.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*

import org.scalajs.dom

import scala.concurrent.Future
import scala.scalajs.js

/** The Sessions tab: one session of attention across every thread, drawn as thread boxes down a shared time axis, each
  * stretch showing its state and what was said or done in it.
  */
object SessionsView:
  final case class Props(call: RpcCall, openThread: String => Callback, event: RealtimeEventJs | Null)
  /** `draft` is the set of ticked stretch keys; `plans` the server's pricing of the ticked stretches per billing root;
    * `ledgers` the roots' ledgers, read for their shared stretches when a send is blocked on one.
    */
  final case class State(
    screen: Option[SessionScreenJs],
    loading: Boolean,
    error: Option[String],
    draft: Set[String],
    plans: Map[String, PlanJs],
    ledgers: Map[String, LedgerJs],
    busy: Boolean,
    /** The keys of the mark the pointer is over, whose full detail shows in the card at the corner. */
    hover: Option[Vector[String]],
    /** Where the hover card goes, in window pixels: inside the Sessions panel, on the side away from the pointer. */
    hoverAt: HoverAt,
    /** The billing roots whose summaries are being written. */
    writing: Set[String],
    /** The thread whose bill-to picker is open under its box's name. */
    picking: Option[String],
    /** The bb projects and the Zoho catalog the picker offers, read the first time a picker opens. */
    projects: Option[Vector[ProjectInfo]],
    catalog: Option[CatalogResult],
    /** The keys being left out or put back, until the screen that shows the change arrives. */
    leaving: Set[String],
    /** Columns folded or unfolded by hand, by the id of the column's first thread; true is folded. Any other column
      * is folded when it has nothing left to review.
      */
    folding: Map[String, Boolean])

  /** The hover card's horizontal place: its left edge and its width. */
  final case class HoverAt(left: Double, width: Double)

  /** The ticks of each session, kept outside the component so a reload or a visit elsewhere does not lose them. */
  private val savedDrafts = scala.collection.mutable.Map.empty[Double, Set[String]]

  /** What the screen knows of the session's threads: who each is, and which thread's submission each bills through. */
  private final class Threads(val accts: Vector[ThreadAccount]):
    private val byId                       = accts.map(t => t.threadId -> t).toMap
    def account(id: String): Option[ThreadAccount] = byId.get(id)
    def billing(id: String): Billing       = byId.get(id).fold(Billing.Unmarked)(billingOf)
    def bills(id: String): Boolean         = billing(id) == Billing.Bills
    def title(id: String): String          = byId.get(id).fold(id)(_.title)

    /** The topmost ancestor that bills to the same place: the thread whose ledger and submission cover this one. */
    def rootOf(id: String): String =
      byId.get(id) match
        case Some(t) =>
          t.parentThreadId.flatMap(byId.get).filter(p => p.billsTo.isDefined && p.billsTo == t.billsTo) match
            case Some(parent) => rootOf(parent.threadId)
            case None         => id
        case None    => id

  private val ColPx   = 170

  /** The width of a folded column: a strip that keeps the marks' colours and the button that unfolds it. */
  private val FoldedPx = 28

  /** Where the columns sit across the page, after the axis: a folded column is a narrow strip. */
  private final case class Columns(folded: Set[Int]):
    def width(c: Int): Int              = if folded(c) then FoldedPx else ColPx
    def left(c: Int): Int               = AxisPx + (0 until c).map(width).sum
    def spanWidth(c: Int, n: Int): Int  = (c until c + n).map(width).sum

  /** Height of the sticky row naming every column; a box's own sticky name stops just under it. */
  private val HeaderPx = 50
  /** The axis: the signal dots in its left part, the clock times right-aligned in the rest. */
  private val AxisPx  = 92

  /** The signal kinds in their lanes down the axis, left to right, each with its dot's colour and what it means. */
  private val SignalLanes = Vector(
    ("speech", "bg-sky-500", "you spoke to the thread"),
    ("playback", "bg-violet-500", "a reply was read aloud to you"),
    ("message", "bg-emerald-500", "you sent a typed message"),
    ("view", "bg-amber-500", "the thread was on screen while you were active"),
    ("turn", "bg-zinc-400", "the agent ran a turn")
  )

  private def billingOf(t: ThreadAccount): Billing =
    if t.billsTo.isDefined then Billing.Bills
    else if t.resolved.source.isDefined then Billing.NotClientWork
    else Billing.Unmarked

  private def stateOf(s: StretchStateJs): StretchState =
    s.kind match
      case "leftOut"      => StretchState.LeftOut
      case "sent"         => StretchState.Sent(s.submissionId.getOrElse(""))
      case "settled"      => StretchState.Settled(s.submissionId.getOrElse(""))
      case "inZohoByHand" =>
        val t = s.trace.toOption
        StretchState.InZohoByHand(
          t.fold(ZohoTrace.TimerLeftRunning(0L, ""))(t =>
            if t.kind == "timerRan" then ZohoTrace.TimerRan(t.start.toLong, t.end.toLong, t.detail)
            else ZohoTrace.TimerLeftRunning(t.start.toLong, t.detail)
          )
        )
      case _              => StretchState.Unbilled

  /** What a stretch shows without a click: the model's line when one has been written, else the first thing the user
    * said in it, else the fallback line.
    */
  private def textOf(l: LedgerStretchJs): Option[String] =
    val summary = Option(l.summary).map(_.trim).filter(_.nonEmpty)
    val first   = l.messages.find(_.speaker == "User").map(_.text.trim).filter(_.nonEmpty)
    if l.fromModel then summary.orElse(first) else first.orElse(summary)

  private def stateWord(st: StretchState): String =
    st match
      case StretchState.Unbilled        => "unbilled"
      case StretchState.LeftOut         => "left out"
      case StretchState.Sent(_)         => "sent"
      case StretchState.Settled(_)      => "settled"
      case StretchState.InZohoByHand(_) => "in Zoho by hand"

  private def draftableKind(st: StretchState): Boolean =
    st match
      case StretchState.Unbilled | StretchState.InZohoByHand(_) => true
      case _                                                    => false

  final class Backend($ : BackendScope[Props, State]):
    def load(at: Option[Long]): Callback =
      $.props.flatMap { p =>
        $.modState(_.copy(loading = true, error = None)) >>
          Callback.future(
            rpc(p.call, BillingRpc.SessionScreen(at))
              .map(r => arrive(r))
              .recover { case e => $.modState(_.copy(loading = false, leaving = Set.empty, error = Some(errorMessage(e)))) }
          )
      }

    private def threadsOf(screen: SessionScreenJs) = new Threads(accounts(screen.threads))

    /** A new screen: the ticks come back from where this session left them, else nothing is ticked. */
    private def arrive(r: SessionScreenJs): Callback =
      val th        = threadsOf(r)
      val draftable = r.stretches.toVector.filter(l => th.bills(l.threadId) && draftableKind(stateOf(l.state))).map(_.key).toSet
      val saved     = if r.current >= 0 then savedDrafts.get(r.sessions(r.current).start) else None
      $.modState(
        _.copy(screen = Some(r), loading = false, leaving = Set.empty, writing = r.summarizing.toSet, draft = saved.fold(Set.empty[String])(_.intersect(draftable))),
        price
      )

    private def keysByRoot(screen: SessionScreenJs, draft: Set[String]): Map[String, Vector[String]] =
      val th = threadsOf(screen)
      screen.stretches.toVector.filter(l => draft(l.key)).groupMap(l => th.rootOf(l.threadId))(_.key)

    /** Asks the server to price what is ticked, one request per billing root. */
    def price: Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        s.screen.fold(Callback.empty) { screen =>
          if screen.current >= 0 then savedDrafts(screen.sessions(screen.current).start) = s.draft
          val byRoot = keysByRoot(screen, s.draft)
          $.modState(_.copy(plans = Map.empty)) >>
            Callback.sequence(byRoot.toVector.map { (root, keys) =>
              Callback.future(
                rpc(p.call, BillingRpc.Draft(root, subthreads = true, DraftPick.Keys(keys.toSet)))
                  .map(plan => $.modState(st => if st.draft == s.draft then st.copy(plans = st.plans + (root -> plan)) else st))
                  .recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
              )
            }) >> loadBlockLedgers(byRoot.keys.toVector)
        }
      }

    /** The ledgers of roots whose send is blocked, for the shared stretches awaiting a decision. */
    private def loadBlockLedgers(roots: Vector[String]): Callback =
      $.props.flatMap(p =>
        Callback.sequence(roots.map(root =>
          Callback.future(
            rpc(p.call, BillingRpc.Ledger(root, subthreads = true))
              .map(l => $.modState(st => st.copy(ledgers = st.ledgers + (root -> l))))
              .recover { case _ => Callback.empty }
          )
        ))
      )

    /** Starts the existing summary run for the ticked stretches of a billing root that have no line yet. */
    private def writeSummaries(root: String): Callback =
      $.state.flatMap(s =>
        s.screen.fold(Callback.empty)(screen => summarize(root, keysByRoot(screen, s.draft).getOrElse(root, Vector.empty)))
      )

    /** The unbilled stretches of the session's billing threads that have no model summary, by billing root: what can be
      * summarized before anything is ticked.
      */
    private def unsummarized(screen: SessionScreenJs): Map[String, Vector[String]] =
      val th = threadsOf(screen)
      screen.stretches.toVector
        .filter(l => th.bills(l.threadId) && l.state.kind == "unbilled" && !l.fromModel)
        .groupMap(l => th.rootOf(l.threadId))(_.key)

    private def summarizeSession: Callback =
      $.state.flatMap(s =>
        s.screen.fold(Callback.empty)(screen =>
          Callback.sequence(unsummarized(screen).map((root, keys) => summarize(root, keys)))
        )
      )

    /** Starts the existing summary run for some stretches of a billing root. The run takes a while; `BillingEvent` says
      * when it ended (see `onEvent`).
      */
    private def summarize(root: String, keys: Vector[String]): Callback =
      $.props.flatMap { p =>
        $.modState(st => st.copy(writing = st.writing + root, error = None)) >>
          Callback.future(
            rpc(p.call, BillingRpc.Summarize(root, subthreads = true, DraftPick.Keys(keys.toSet)))
              .map(_ => Callback.empty)
              .recover { case e => $.modState(st => st.copy(writing = st.writing - root, error = Some(errorMessage(e)))) }
          )
      }

    def onEvent(event: BillingEvent): Callback =
      event match
        case BillingEvent.SummariesWritten(id)      => $.modState(st => st.copy(writing = st.writing - id)) >> reloadCurrent
        case BillingEvent.SummariesFailed(id, why)  =>
          $.modState(st => st.copy(writing = st.writing - id, error = Some(s"Summaries not written: $why")))
        case BillingEvent.Changed                   => reloadCurrent

    /** Opens or closes a thread's bill-to picker, reading the projects and catalog it offers the first time. */
    private def togglePicker(id: String): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        val open = $.modState(_.copy(picking = Option.when(!s.picking.contains(id))(id)))
        if s.projects.nonEmpty then open
        else
          open >> Callback.future(
            rpc(p.call, BillingRpc.Overview(js.Date.now().toLong)).flatMap(o =>
              (if o.zoho.configured then rpc(p.call, BillingRpc.Catalog(refresh = false)).map(Some(_)) else Future.successful(None))
                .map(c => $.modState(_.copy(projects = Some(o.projects.toVector), catalog = c)))
            ).recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
          )
      }

    private def setHover(keys: Option[Vector[String]]): Callback =
      $.modState(s => if s.hover == keys then s else s.copy(hover = keys))

    /** The card is fixed to the window but placed inside this panel, which may share the window with others. */
    private def startHover(keys: Vector[String], e: ReactMouseEvent): Callback =
      val panel = Option(e.currentTarget.asInstanceOf[dom.Element].closest("[data-sessions-panel]"))
        .map(_.getBoundingClientRect())
      val (from, to) = panel.fold((0.0, dom.window.innerWidth.toDouble))(r => (r.left, r.right))
      val width      = math.min(480.0, to - from - 32)
      val at         =
        if e.clientX > (from + to) / 2 then HoverAt(from + 16, width) else HoverAt(to - 16 - width, width)
      $.modState(s => if s.hover.contains(keys) && s.hoverAt == at then s else s.copy(hover = Some(keys), hoverAt = at))

    /** Ticks or unticks, then prices: the price is asked once the new ticks are in the state, never before. */
    private def tick(keys: Vector[String]): Callback =
      $.modState(
        s =>
          val on = keys.forall(s.draft)
          s.copy(draft = if on then s.draft -- keys else s.draft ++ keys),
        price
      )

    private def leaveOut(keys: Vector[String], out: Boolean): Callback =
      $.props.flatMap(p =>
        $.modState(s => s.copy(draft = if out then s.draft -- keys else s.draft ++ keys, busy = true, leaving = s.leaving ++ keys), saveDraft) >>
          Callback.future(
            keys.foldLeft(Future.unit)((done, key) =>
              done.flatMap(_ => rpc(p.call, BillingRpc.SetExcluded(key, out)).map(_ => ()))
            ).map(_ => $.modState(_.copy(busy = false)) >> reloadCurrent)
              .recover { case e => $.modState(st => st.copy(busy = false, leaving = st.leaving -- keys, error = Some(errorMessage(e)))) }
          )
      )

    /** Keeps the ticks of the open session where the next reload will find them. */
    private def saveDraft: Callback =
      $.state.map(s =>
        s.screen.filter(_.current >= 0).foreach(sc => savedDrafts(sc.sessions(sc.current).start) = s.draft)
      )

    def reloadCurrent: Callback =
      $.state.flatMap(s =>
        load(s.screen.filter(_.current >= 0).map(sc => sc.sessions(sc.current).start.toLong))
      )

    private def decide(root: String)(key: BlockKey, allocation: Allocation): Callback =
      $.props.flatMap(p =>
        Callback.future(
          rpc(p.call, BillingRpc.Decide(root, subthreads = true, key, allocation)).map(_ => reloadCurrent)
            .recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
        )
      )

    private def undecide(key: BlockKey): Callback =
      $.props.flatMap(p =>
        Callback.future(
          rpc(p.call, BillingRpc.Undecide(key)).map(_ => reloadCurrent)
            .recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
        )
      )

    private def send(root: String, billed: Boolean): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        (s.screen, s.plans.get(root)) match
          case (Some(screen), Some(plan)) =>
            val keys     = keysByRoot(screen, s.draft).getOrElse(root, Vector.empty)
            val title    = threadsOf(screen).title(root)
            val question =
              if billed then
                s"Send ${hoursMinutes(plan.totalMs)} for \"$title\" as ${plan.entries.length} Zoho entries (${plan.entries.map(e => Words.dayLabel(e.day)).mkString(", ")})? The ${keys.length} ticked stretches count as billed from then on."
              else s"Mark the ${keys.length} ticked stretches of \"$title\" (${hoursMinutes(plan.totalMs)}) as settled without billing?"
            if !dom.window.confirm(question) then Callback.empty
            else
              $.modState(_.copy(busy = true)) >>
                Callback.future(
                  rpc(p.call, BillingRpc.Submit(root, subthreads = true, DraftPick.Keys(keys.toSet), billed))
                    .map(_ => $.modState(st => st.copy(busy = false, draft = st.draft -- keys)) >> reloadCurrent)
                    .recover { case e => $.modState(_.copy(busy = false, error = Some(errorMessage(e)))) }
                )
          case _                          => Callback.empty
      }

    private def stateClass(st: StretchState, ticked: Boolean): String =
      val base = st match
        case StretchState.Unbilled        => "border-blue-500 bg-blue-200/70 dark:bg-blue-500/30"
        case StretchState.LeftOut         => "border-border bg-muted text-muted-foreground line-through"
        case StretchState.Sent(_)         => "border-green-500 bg-green-200/70 dark:bg-green-500/30"
        case StretchState.Settled(_)      => "border-border bg-muted text-muted-foreground"
        case StretchState.InZohoByHand(_) => "border-amber-500 bg-amber-200/70 dark:bg-amber-500/30"
      if ticked then base + " ring-2 ring-primary" else base

    /** One swatch drawn like a real mark, and what it means or does. */
    private def sample(
      label: String,
      state: StretchState,
      ticked: Boolean,
      caption: String,
      dotted: Boolean = false): VdomNode =
      <.div(
        ^.key       := label,
        ^.className := "flex items-center gap-2",
        <.div(
          ^.className := s"w-40 shrink-0 rounded border px-1 py-0.5 text-[10px] font-medium leading-tight ${stateClass(state, ticked)}${if dotted then " border-dotted" else ""}",
          label
        ),
        <.span(caption)
      )

    private def legend: VdomNode =
      <.div(
        ^.className := "grid gap-x-6 gap-y-1 text-xs text-muted-foreground sm:grid-cols-2 lg:grid-cols-3",
        SignalLanes.map((kind, colour, meaning) =>
          <.div(
            ^.key       := s"signal-$kind",
            ^.className := "flex items-center gap-2",
            <.span(^.className := s"inline-block h-[7px] w-[7px] shrink-0 rounded-full $colour"),
            <.span(s"Dot down the time axis: $meaning ($kind).")
          )
        ).toVdomArray,
        sample("☑ 5m 22:28 · unbilled", StretchState.Unbilled, ticked = true, "Click to untick. Goes into the next send."),
        sample("☐ 5m 22:28 · unbilled", StretchState.Unbilled, ticked = false, "Click to tick it."),
        sample("3 short · 1m · unbilled", StretchState.Unbilled, ticked = true, "Several tiny stretches of one thread as one mark. One click ticks or unticks all of them.", dotted = true),
        <.div(
          ^.key       := "not-billable",
          ^.className := "flex items-center gap-2",
          <.div(
            ^.className := "w-40 shrink-0 rounded border border-border bg-background px-1 py-0.5 text-[10px] font-medium leading-tight text-muted-foreground",
            "5m 22:28 · not billable"
          ),
          <.span("Not billable: a thread with no client. Shown so you can see what you did; nothing to tick.")
        ),
        sample("5m 22:28 · left out", StretchState.LeftOut, ticked = false, "Out of every send. \"put back\" undoes it."),
        sample("5m 22:28 · sent", StretchState.Sent(""), ticked = false, "Already in Zoho."),
        sample("5m 22:28 · settled", StretchState.Settled(""), ticked = false, "Marked done without billing."),
        sample("5m 22:28 · in Zoho by hand", StretchState.InZohoByHand(ZohoTrace.TimerLeftRunning(0L, "")), ticked = false, "Entered in Zoho yourself. Click to include it anyway."),
        <.div(
          ^.className := "flex items-center gap-2",
          <.div(^.className := "h-5 w-40 shrink-0 rounded border border-dashed border-amber-500/70 bg-amber-500/5"),
          <.span("A thread not yet marked as client work or not. Nothing to tick.")
        ),
        <.div(
          ^.className := "flex items-center gap-2",
          <.div(^.className := "h-5 w-40 shrink-0 rounded border border-blue-500/70 bg-blue-500/5"),
          <.span("A client thread's box. Its title is on its first stretch; click the title to open it.")
        )
      )

    private def markView(
      s: State,
      th: Threads,
      tz: String,
      pm: PlacedMark,
      cols: Columns,
      column: Int,
      title: Option[String]): VdomNode =
      val folded   = cols.folded(column)
      val members  = pm.mark.members
      val keys     = members.map(_.window.key)
      val canTick  = th.bills(members.head.window.threadId) && members.forall(m => draftableKind(m.state))
      val ticked   = canTick && keys.forall(s.draft)
      val canOut   = th.bills(members.head.window.threadId) &&
        (members.forall(m => draftableKind(m.state)) || members.forall(_.state == StretchState.LeftOut))
      val isOut    = members.forall(_.state == StretchState.LeftOut)
      val height   = math.max(pm.yBottom - pm.yTop - 2, 14.0)
      val parts    = members.length
      val tiny     = members.forall(_.durationMs < SessionView.TinyMs)
      // Time on a thread that does not bill is shown, never decided: hollow, and "not billable" in place of "unbilled".
      val hollow = !th.bills(members.head.window.threadId) && members.forall(_.state == StretchState.Unbilled)
      def word(st: StretchState): String = if hollow then "not billable" else stateWord(st)
      def look(st: StretchState): String =
        if hollow then "border-border/60 bg-background font-normal text-muted-foreground/60" else stateClass(st, ticked)
      val (cls, head, body) = pm.mark match
        case Mark.One(st)  =>
          (
            look(st.state),
            s"${hoursMinutes(st.durationMs.toDouble)} ${clock(st.window.start.toDouble, tz)} · ${word(st.state)}",
            st.text.getOrElse("")
          )
        case Mark.Cluster(f, _) =>
          val states = members.map(m => word(m.state)).distinct
          (
            look(f.state) + " border-dotted",
            s"$parts ${if tiny then "short" else "parts"} · ${hoursMinutes(pm.mark.durationMs.toDouble)} ${clock(f.window.start.toDouble, tz)}" +
              (if states.length > 1 then " · mixed" else s" · ${states.head}"),
            members.flatMap(_.text).headOption.getOrElse("")
          )
      <.div(
        ^.key       := keys.head,
        ^.onMouseEnter ==> ((e: ReactMouseEvent) => startHover(keys, e)),
        ^.onMouseLeave --> setHover(None),
        ^.className := s"absolute overflow-hidden rounded border px-1 text-[10px] leading-tight $cls " +
          (if canTick then "cursor-pointer " else "") + (if keys.exists(s.leaving) then "animate-pulse opacity-60" else ""),
        ^.style     := js.Dynamic.literal(
          "left"   -> (cols.left(column) + 2),
          "top"    -> pm.yTop,
          "width"  -> (cols.width(column) - 8),
          "height" -> height
        ),
        ^.onClick --> (if canTick && !s.busy then tick(keys) else Callback.empty),
        title.map(_ => <.div(^.className := "h-[14px]")),
        <.div(
          ^.className := (if hollow then "flex items-center gap-1 whitespace-nowrap" else "flex items-center gap-1 font-medium whitespace-nowrap"),
          if canTick then <.span(if ticked then "☑" else "☐") else EmptyVdom,
          <.span(^.className := "truncate", head)
        ).when(!folded),
        <.div(^.className := (if hollow then "break-words text-muted-foreground/60" else "break-words text-muted-foreground"), body.take(160)).when(!folded && body.nonEmpty && height > 44),
        <.button(
          ^.`type`    := "button",
          // A generous target, so a click meant for it does not land on the mark and tick it instead.
          ^.className := "absolute right-0 bottom-0 z-10 rounded border border-border bg-background px-2 py-1 text-[10px] leading-none hover:bg-muted",
          ^.disabled  := s.busy,
          ^.onClick ==> ((e: ReactMouseEvent) => e.stopPropagationCB >> leaveOut(keys, out = !isOut)),
          if keys.exists(s.leaving) then (if isOut then "putting back…" else "leaving out…")
          else if isOut then "put back"
          else "leave out"
        ).when(!folded && canOut && height > 26)
      )

    /** Where a thread bills, in a word; a click opens the picker that changes it. */
    private def billingChip(th: Threads, id: String, billing: Billing): VdomNode =
      <.button(
        ^.`type`    := "button",
        ^.title     := th.account(id).fold("")(a => Words.billsTo(a.resolved)) + " (click to change)",
        ^.className := "shrink-0 rounded border border-border px-1 font-normal hover:bg-muted " + (billing match
          case Billing.Bills         => "text-blue-600 dark:text-blue-400"
          case Billing.NotClientWork => "text-muted-foreground"
          case Billing.Unmarked      => "border-amber-500/70 text-amber-600 dark:text-amber-400"),
        ^.onClick --> togglePicker(id),
        billing match
          case Billing.Bills         => th.account(id).flatMap(_.billsTo).fold("bills")(_.customerName)
          case Billing.NotClientWork => "not client"
          case Billing.Unmarked      => "unset"
      )

    private def boxView(pr: Props, cols: Columns, p: PlacedBox): Vector[VdomNode] =
      val id      = p.box.info.id
      val billing = p.box.info.billing
      val cls     = billing match
        case Billing.Bills         => "border-blue-500/70 bg-blue-500/5"
        case Billing.NotClientWork => "border-border bg-muted/40"
        case Billing.Unmarked      => "border-dashed border-amber-500/70 bg-amber-500/5"
      // The thread's name heads its box and stays at the top of the screen while the box is scrolled through.
      val header  = <.button(
        ^.`type`    := "button",
        ^.title     := (p.box.info.title + (billing match
          case Billing.Bills         => ""
          case Billing.NotClientWork => " (not client work)"
          case Billing.Unmarked      => " (never marked as client work or not)")),
        ^.className := "sticky top-0 z-30 block truncate rounded-sm bg-background px-1 text-left text-[10px] font-semibold leading-[14px] hover:underline",
        ^.style     := js.Dynamic.literal("width" -> (cols.width(p.column) - 10), "top" -> HeaderPx),
        ^.onClick --> pr.openThread(id),
        p.box.info.title
      )
      val frame   = <.div(
        ^.key       := s"box-${p.box.info.id}",
        ^.className := s"absolute rounded border $cls",
        ^.style     := js.Dynamic.literal(
          "left"   -> cols.left(p.column),
          "top"    -> p.yTop,
          "width"  -> (cols.spanWidth(p.column, p.span) - 4),
          "height" -> math.max(p.yBottom - p.yTop, 16.0)
        ),
        header
      )
      Vector[VdomNode](frame) ++ p.children.flatMap(boxView(pr, cols, _))

    /** What a column's threads still need, most urgent first. */
    private enum ColumnNeed(val headerClass: String, val words: String):
      case Settle   extends ColumnNeed("border-t-4 border-amber-500", "Has time already in Zoho by hand to settle.")
      case Decide   extends ColumnNeed("border-t-4 border-dashed border-amber-500", "Has a thread never marked as client work or not.")
      case Bill     extends ColumnNeed("border-t-4 border-blue-500", "Has unbilled time.")
      case Nothing  extends ColumnNeed("border-t-4 border-transparent opacity-60", "Nothing left to review.")

    /** The most urgent need of the threads whose own strips are in one column. */
    private def needOf(th: Threads, boxes: Vector[PlacedBox]): ColumnNeed =
      val stretches = boxes.flatMap(_.own).flatMap(_.mark.members)
      val billing   = stretches.filter(m => th.bills(m.window.threadId)).map(_.state)
      if billing.exists { case StretchState.InZohoByHand(_) => true; case _ => false } then ColumnNeed.Settle
      else if boxes.exists(b => b.box.info.billing == Billing.Unmarked && b.own.nonEmpty) then ColumnNeed.Decide
      else if billing.contains(StretchState.Unbilled) then ColumnNeed.Bill
      else ColumnNeed.Nothing

    private def allBoxes(p: PlacedBox): Vector[PlacedBox] = p +: p.children.flatMap(allBoxes)

    private def allMarks(p: PlacedBox): Vector[(PlacedMark, Int, Option[String])] =
      p.own.zipWithIndex.map((pm, i) => (pm, p.column, if i == 0 then Some(p.box.info.title) else None)) ++
        p.children.flatMap(allMarks)

    private def timeline(p: Props, s: State, tz: String, screen: SessionScreenJs, th: Threads): VdomNode =
      val infos     = th.accts.map(t =>
        ThreadInfo(t.threadId, t.title, t.parentThreadId, billingOf(t), t.createdAt, t.archivedAt)
      )
      val ref       = screen.sessions(screen.current)
      val session   = Session(ref.start.toLong, ref.end.toLong, Set.empty)
      val stretches = screen.stretches.toVector.map(l =>
        SessionStretch(Window(l.threadId, l.day, l.start.toLong, l.end.toLong), stateOf(l.state), textOf(l))
      )
      val layout    = SessionGeometry.place(session, SessionView.forest(session, infos, stretches))
      // Every thread in column order, so any thread can be found by name without scrolling the diagram.
      val byColumn = layout.roots.flatMap(allBoxes).groupBy(_.column).toVector.sortBy(_._1)
      val needs    = byColumn.map((column, boxes) => column -> needOf(th, boxes)).toMap
      def columnId(boxes: Vector[PlacedBox]): String = boxes.minBy(_.yTop).box.info.id
      val cols     = Columns(byColumn.collect {
        case (column, boxes) if s.folding.getOrElse(columnId(boxes), needs(column) == ColumnNeed.Nothing) => column
      }.toSet)
      val width    = cols.left(layout.columns) + 20
      val header   = <.div(
        ^.className := "sticky top-0 z-40 border-b border-border bg-background",
        ^.style     := js.Dynamic.literal("height" -> HeaderPx, "width" -> width),
        // The screen has padding above this row; an opaque strip fills it so nothing scrolls into view above the names.
        <.div(
          ^.className := "absolute inset-x-0 bg-background",
          ^.style     := js.Dynamic.literal("top" -> -40, "height" -> 40)
        ),
        byColumn.map { (column, boxes) =>
          val names  = boxes.sortBy(_.yTop).map(_.box.info)
          val need   = needs(column)
          val folded = cols.folded(column)
          val fold   = <.button(
            ^.`type`    := "button",
            ^.title     := (if folded then "Unfold this column" else "Fold this column to a narrow strip"),
            ^.className := "shrink-0 rounded px-0.5 text-muted-foreground hover:bg-muted",
            ^.onClick --> $.modState(st => st.copy(folding = st.folding + (columnId(boxes) -> !folded))),
            if folded then "▸" else "◂"
          )
          <.div(
            ^.key       := s"col-$column",
            ^.title     := (names.map(_.title) :+ need.words).mkString("\n"),
            ^.className := s"absolute overflow-hidden text-[10px] leading-[14px] ${need.headerClass}",
            ^.style     := js.Dynamic.literal(
              "left"   -> (cols.left(column) + 4),
              "top"    -> 2,
              "width"  -> (cols.width(column) - 10),
              "height" -> (HeaderPx - 4)
            ),
            if folded then fold
            else names.take(3).zipWithIndex.map { (t, i) =>
              val label = if i == 2 && names.length > 3 then s"${t.title} +${names.length - 3} more" else t.title
              <.div(
                ^.key       := t.id,
                ^.className := "flex items-center gap-1",
                fold.when(i == 0),
                <.button(
                  ^.`type`    := "button",
                  ^.className := "min-w-0 flex-1 truncate text-left font-semibold hover:underline",
                  ^.onClick --> p.openThread(t.id),
                  label
                ),
                billingChip(th, t.id, t.billing)
              )
            }.toVdomArray
          )
        }.toVdomArray,
        // The picker opens under the header row, at the column of the thread it changes.
        s.picking.flatMap(id => byColumn.find(_._2.exists(_.box.info.id == id)).map(_._1).zip(th.account(id))).map {
          (column, a) =>
            <.div(
              ^.className := "absolute z-50 w-72 rounded border border-border bg-background p-1 text-xs shadow-md",
              ^.style     := js.Dynamic.literal("left" -> cols.left(column), "top" -> HeaderPx),
              s.projects.fold[VdomNode](<.div(^.className := "p-1 text-muted-foreground", "Loading…"))(projects =>
                BillToSelect.Component.withKey(a.threadId)(BillToSelect.Props(
                  a,
                  projects.find(_.projectId == a.projectId),
                  s.catalog,
                  p.call,
                  $.modState(_.copy(picking = None)) >> reloadCurrent
                ))
              )
            )
        }
      )
      <.div(
        ^.style := js.Dynamic.literal("width" -> width),
        header,
        <.div(
        ^.className := "relative",
        ^.style     := js.Dynamic.literal(
          "height" -> (layout.height + 40),
          "width"  -> width
        ),
        layout.roots.flatMap(boxView(p, cols, _)).toVdomArray,
        layout.roots.flatMap(allMarks).map((pm, c, t) => markView(s, th, tz, pm, cols, c, t)).toVdomArray,
        layout.quiet.zipWithIndex.map { (q, i) =>
          <.div(
            ^.key       := s"quiet-$i",
            ^.className := "absolute left-0 bg-muted/60 text-center text-[10px] text-muted-foreground",
            ^.style     := js.Dynamic.literal(
              "top"    -> q.yTop,
              "height" -> (q.yBottom - q.yTop),
              "width"  -> cols.left(layout.columns)
            ),
            s"${hoursMinutes(q.durationMs.toDouble)} with no attention on any thread"
          )
        }.toVdomArray,
        screen.signals.toVector.zipWithIndex.flatMap { (g, i) =>
          val lane = SignalLanes.indexWhere(_._1 == g.kind)
          Option.when(lane >= 0) {
            val title = infos.find(_.id == g.threadId).fold(g.threadId)(_.title)
            <.div(
              ^.key       := s"sig-$i",
              ^.className := s"absolute h-[5px] w-[5px] rounded-full ${SignalLanes(lane)._2}",
              ^.style     := js.Dynamic.literal("top" -> (layout.yAt(g.at.toLong) - 2), "left" -> (2 + lane * 6)),
              ^.title     := s"${g.kind} · ${clock(g.at, tz)} · $title"
            )
          }
        }.toVdomArray,
        layout.axis.zipWithIndex.map { (a, i) =>
          <.div(
            ^.key       := s"ax-$i",
            ^.className := "absolute left-0 text-right text-[10px] text-muted-foreground",
            ^.style     := js.Dynamic.literal("top" -> a._1, "width" -> (AxisPx - 6)),
            clock(a._2.toDouble, tz)
          )
        }.toVdomArray
        )
      )

    /** What is ticked, priced per billing root, with what stops a send, and the buttons to send or settle. */
    /** Everything the old per-day card said about a stretch, for the mark under the pointer. */
    private def detailCard(s: State, tz: String, screen: SessionScreenJs, th: Threads, keys: Vector[String]): VdomNode =
      val lines = screen.stretches.toVector.filter(l => keys.contains(l.key)).sortBy(_.start)
      lines.headOption.fold(EmptyVdom: VdomNode) { first =>
        val last     = lines.last
        val total    = lines.map(_.durationMs).sum
        val messages = lines.flatMap(_.messages.toVector)
        val kinds    = lines.map(_.state.kind).distinct
        def on(id: js.UndefOr[String]): String =
          id.toOption.flatMap(i => screen.submissions.get(i)).fold("")(at => s" on ${stamp(at, tz)}")
        val stateLine =
          if !th.bills(first.threadId) && kinds == Vector("unbilled") then
            "Not billable: the thread has no client, so this time is shown but never billed."
          else if kinds.length > 1 then s"Mixed: ${kinds.mkString(", ")}"
          else
            first.state.kind match
              case "sent"         => s"Sent to Zoho${on(first.state.submissionId)}"
              case "settled"      => s"Settled without billing${on(first.state.submissionId)}"
              case "leftOut"      => "Left out: in no send until put back"
              case "inZohoByHand" =>
                "In Zoho by hand: " + first.state.trace.toOption.fold("")(t => traceText(t, tz)) +
                  ". Tick it to include it anyway."
              case _              => "Unbilled"
        val summary   = lines.map(_.summary).filter(_.nonEmpty).mkString(" ")
        val shown     = messages.take(12)
        <.div(
          // Opposite the pointer, so it never hides the stretch being pointed at; above the send panel (z-50).
          ^.style     := js.Dynamic.literal("left" -> s.hoverAt.left, "width" -> s.hoverAt.width),
          ^.className := "pointer-events-none fixed bottom-4 z-[70] max-h-[70vh] space-y-1 overflow-hidden rounded-md border border-border bg-card p-3 text-xs shadow-lg",
          <.div(^.className := "font-semibold", first.title),
          <.div(
            ^.className := "tabular-nums",
            s"${clock(first.start, tz)}–${clock(last.end, tz)} · ${hoursMinutes(total)} recorded" +
              (if lines.length > 1 then s" · ${lines.length} stretches" else "")
          ),
          <.div(^.className := "text-muted-foreground", stateLine),
          <.div(
            summary,
            <.span(^.className := "ml-1 text-muted-foreground", "(first message; no summary yet)").when(
              lines.exists(!_.fromModel)
            )
          ).when(summary.nonEmpty),
          <.div(
            ^.className := "text-muted-foreground",
            s"${messages.count(_.speaker == "User")} from you, ${messages.count(_.speaker == "Agent")} replies, ${messages.count(_.speaker == "Work")} actions"
          ),
          conversationList(js.Array(shown*), tz),
          <.div(^.className := "text-muted-foreground", s"+${messages.length - shown.length} more turns").when(
            messages.length > shown.length
          )
        )
      }

    private def sendPanel(p: Props, s: State, tz: String, screen: SessionScreenJs, th: Threads): VdomNode =
      val byRoot = keysByRoot(screen, s.draft)
      val ref    = screen.sessions(screen.current)
      if byRoot.isEmpty then
        <.div(^.className := "text-xs text-muted-foreground", "Nothing is ticked. Click a stretch to tick it.")
      else
        <.div(
          ^.className := "space-y-2",
          byRoot.toVector.sortBy(_._1).map { (root, keys) =>
            val plan     = s.plans.get(root)
            val blockers = plan.fold(Vector.empty[String])(_.blockers.toVector)
            val missing  = screen.stretches.toVector.count(l => keys.contains(l.key) && !l.fromModel)
            val blocks   = s.ledgers.get(root).fold(Vector.empty[Block])(l =>
              l.blocks.toVector.filter(b => nullable(b.decision).isEmpty && b.end > ref.start && b.start < ref.end)
            )
            <.div(
              ^.key       := root,
              ^.className := cardClass,
              <.div(
                ^.className := "flex flex-wrap items-center gap-2",
                <.span(^.className := "font-medium", th.title(root)),
                <.span(
                  ^.className := "text-xs text-muted-foreground",
                  plan.fold("pricing…")(pl =>
                    s"${keys.length} stretches · ${hoursMinutes(pl.totalMs)} → " +
                      pl.entries.map(e => s"${Words.dayLabel(e.day)} ${e.logTime}").mkString(", ")
                  )
                )
              ),
              plan.filter(_.handEntered.length > 0).map(pl =>
                <.div(
                  ^.className := "space-y-0.5 rounded border border-amber-500/40 bg-amber-500/10 px-2 py-1 text-xs",
                  <.div(
                    ^.className := "font-medium",
                    s"Already in Zoho on these dates, not from bb (${pl.handEntered.length}): check against the ticked stretches for double billing"
                  ),
                  pl.handEntered.toVector.map(h =>
                    <.div(
                      ^.key := h.timeEntryId,
                      s"${h.date}: ${hoursMinutes(h.minutes * 60_000)} ${h.taskName} by ${h.userName}: ${h.notes} (${h.billedStatus})"
                    )
                  ).toVdomArray
                )
              ),
              blockers.map(b => <.div(^.key := b, ^.className := "text-xs text-destructive", b)).toVdomArray,
              blocks.map { b =>
                BlockCard.Component.withKey(b.key)(BlockCard.Props(
                  b,
                  tz,
                  b.titles.toMap,
                  busy = s.busy,
                  key = BlockKey.parse(b.key).getOrElse(BlockKey(b.day, b.start.toLong, b.end.toLong, b.threadIds.toSet)),
                  decide = decide(root),
                  undecide = undecide,
                  openThread = p.openThread
                )): VdomNode
              }.toVdomArray,
              <.div(
                ^.className := "flex gap-2",
                <.button(
                  ^.`type`    := "button",
                  ^.className := primaryButtonClass,
                  ^.disabled  := (s.busy || plan.isEmpty || blockers.nonEmpty),
                  ^.onClick --> send(root, billed = true),
                  "Send to Zoho"
                ),
                <.button(
                  ^.`type`    := "button",
                  ^.className := buttonClass,
                  ^.disabled  := (s.busy || plan.isEmpty),
                  ^.onClick --> send(root, billed = false),
                  "Settle without billing"
                ),
                <.button(
                  ^.`type`    := "button",
                  ^.className := buttonClass,
                  ^.title     := "Asks a model to write a one-line summary of each ticked stretch that has none, and the notes for each day's entry.",
                  ^.disabled  := (s.busy || s.writing(root)),
                  ^.onClick --> writeSummaries(root),
                  if s.writing(root) then "Writing summaries…" else s"Write summaries ($missing without one)"
                ).when(missing > 0 || s.writing(root))
              )
            )
          }.toVdomArray
        )

    def render(p: Props, s: State): VdomElement =
      // Ticked roots without a plan yet are being priced, unless pricing failed and said so.
      val pricing = s.error.isEmpty && s.screen.exists(sc => keysByRoot(sc, s.draft).keys.exists(r => !s.plans.contains(r)))
      val working = s.loading || s.busy || pricing || s.writing.nonEmpty
      <.div(
        VdomAttr("data-sessions-panel") := "",
        ^.className := "space-y-3",
        // Sticks to the top of the Sessions panel, wherever the panel sits in the window; takes no height.
        <.div(
          ^.className := "sticky top-0 z-[60] flex h-0 justify-end",
          <.div(
            ^.className := "mt-1 mr-1 flex h-fit items-center gap-2 rounded-md border border-border bg-background px-3 py-1 text-xs shadow-md",
            <.span(^.className := "inline-block h-3 w-3 animate-spin rounded-full border-2 border-muted-foreground border-t-transparent"),
            if s.loading then "Loading…"
            else if s.busy then "Saving…"
            else if pricing then "Pricing ticked time…"
            else "Writing summaries…"
          )
        ).when(working),
        s.error.map(e => banner("error", e)),
        s.screen.map { screen =>
          val tz = screen.timeZone
          if screen.current < 0 then <.div(^.className := "text-sm text-muted-foreground", "No attention recorded yet.")
          else
            val ref        = screen.sessions(screen.current)
            val th         = threadsOf(screen)
            def go(i: Int) = load(Some(screen.sessions(i).start.toLong))
            <.div(
              ^.className := "space-y-3",
              <.div(
                ^.className := "space-y-1",
                <.div(
                  ^.className := "flex items-center gap-2 text-xs text-muted-foreground",
                  <.span("All sessions, oldest first. Bold means time still to bill."),
                  <.button(
                    ^.`type`    := "button",
                    ^.className := buttonClass,
                    ^.disabled  := s.loading,
                    ^.onClick --> load(None),
                    "Oldest with time to bill"
                  )
                ),
                <.div(
                  ^.className := "flex max-h-28 flex-wrap gap-1 overflow-auto",
                  screen.sessions.toVector.zipWithIndex.map { (ss, i) =>
                    val left = ss.unbilledMs > 0
                    <.button(
                      ^.key       := ss.start,
                      ^.`type`    := "button",
                      ^.title     := s"${ss.threads} threads, ${hoursMinutes(ss.end - ss.start)} long",
                      ^.className := "rounded border px-2 py-0.5 text-[11px] " +
                        (if i == screen.current then "border-primary bg-primary text-primary-foreground"
                         else if left then "border-border font-semibold hover:bg-accent"
                         else "border-border text-muted-foreground hover:bg-accent"),
                      ^.disabled  := s.loading,
                      ^.onClick --> go(i),
                      s"${shortDay(ss.start, tz)} ${clock(ss.start, tz)}" +
                        (if left then s" · ${hoursMinutes(ss.unbilledMs)} to bill" else "")
                    )
                  }.toVdomArray
                )
              ),
              <.div(
                ^.className := "flex flex-wrap items-center gap-2",
                <.button(
                  ^.`type`    := "button",
                  ^.className := buttonClass,
                  ^.disabled  := (screen.current == 0 || s.loading),
                  ^.onClick --> go(screen.current - 1),
                  "← Earlier session"
                ),
                <.span(
                  ^.className := "text-sm font-medium",
                  s"${stamp(ref.start, tz)} to ${clock(ref.end, tz)} · ${hoursMinutes(ref.end - ref.start)} · ${ref.threads} threads"
                ),
                <.button(
                  ^.`type`    := "button",
                  ^.className := buttonClass,
                  ^.disabled  := (screen.current == screen.sessions.length - 1 || s.loading),
                  ^.onClick --> go(screen.current + 1),
                  "Later session →"
                ),
                <.span(
                  ^.className := "text-xs text-muted-foreground",
                  s"session ${screen.current + 1} of ${screen.sessions.length}; a gap of 2 hours with no attention starts a new one"
                )
              ),
              legend,
              {
                val waiting = unsummarized(screen)
                val count   = waiting.values.map(_.size).sum
                <.button(
                  ^.`type`    := "button",
                  ^.className := buttonClass,
                  ^.title     := "Asks a model to write a one-line summary of each unbilled stretch in this session that has none, ticked or not.",
                  ^.disabled  := (s.busy || waiting.keys.exists(s.writing)),
                  ^.onClick --> summarizeSession,
                  if waiting.keys.exists(s.writing) then "Writing summaries…" else s"Write summaries for this session ($count without one)"
                ).when(count > 0)
              },
              s.hover.map(keys => detailCard(s, tz, screen, th, keys)),
              // Above the timeline header's opaque strip, which reaches 40px above the header to hide what scrolls under it.
              <.div(
                ^.className := "relative z-50 space-y-1",
                {
                  val tickable = screen.stretches.toVector.filter(l => th.bills(l.threadId) && draftableKind(stateOf(l.state)))
                  val ticked   = tickable.count(l => s.draft(l.key))
                  <.div(
                    ^.className := "flex items-center gap-2 text-xs text-muted-foreground",
                    <.progress(^.className := "h-1.5 w-32", VdomAttr("max") := tickable.length, ^.value := ticked.toString),
                    s"$ticked of ${tickable.length} tickable stretches ticked"
                  ).when(tickable.nonEmpty)
                },
                sendPanel(p, s, tz, screen, th)
              ),
              <.div(^.className := "rounded-md border border-border p-2", timeline(p, s, tz, screen, th))
            )
        }.getOrElse(<.div(^.className := "text-sm text-muted-foreground", "Loading…").when(s.loading))
      )

  val Component = ScalaComponent
    .builder[Props]("SessionsView")
    .initialState(State(None, loading = false, error = None, Set.empty, Map.empty, Map.empty, busy = false, hover = None, hoverAt = HoverAt(0, 480), writing = Set.empty, picking = None, projects = None, catalog = None, leaving = Set.empty, folding = Map.empty))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.load(None))
    .componentDidUpdate(x =>
      arrived(x.prevProps.event, x.currentProps.event) match
        case Some(event) => x.backend.onEvent(event)
        case None        => Callback.empty
    )
    .build
