package billingapp

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
    busy: Boolean)

  /** The ticks of each session, kept outside the component so a reload or a visit elsewhere does not lose them. */
  private val savedDrafts = scala.collection.mutable.Map.empty[Double, Set[String]]

  /** What the screen knows of the session's threads: who each is, and which thread's submission each bills through. */
  private final class Threads(val accts: Vector[ThreadAccount]):
    private val byId                       = accts.map(t => t.threadId -> t).toMap
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
  private val AxisPx  = 64

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

  /** What a stretch shows without a click: the first thing the user said in it, else the summary. */
  private def textOf(l: LedgerStretchJs): Option[String] =
    l.messages.find(_.speaker == "User").map(_.text.trim).filter(_.nonEmpty)
      .orElse(Option(l.summary).map(_.trim).filter(_.nonEmpty))

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
              .map(r => arrive(r) >> price)
              .recover { case e => $.modState(_.copy(loading = false, error = Some(errorMessage(e)))) }
          )
      }

    private def threadsOf(screen: SessionScreenJs) = new Threads(accounts(screen.threads))

    /** A new screen: the ticks come back from where this session left them, else every unbilled stretch of a thread
      * that bills is ticked.
      */
    private def arrive(r: SessionScreenJs): Callback =
      val th        = threadsOf(r)
      val draftable = r.stretches.toVector.filter(l => th.bills(l.threadId) && draftableKind(stateOf(l.state))).map(_.key).toSet
      val fresh     = r.stretches.toVector.filter(l => th.bills(l.threadId) && l.state.kind == "unbilled").map(_.key).toSet
      val saved     = if r.current >= 0 then savedDrafts.get(r.sessions(r.current).start) else None
      $.modState(_.copy(
        screen = Some(r),
        loading = false,
        draft = saved.fold(fresh)(_.intersect(draftable))
      ))

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

    private def tick(keys: Vector[String]): Callback =
      $.modState(s =>
        val on = keys.forall(s.draft)
        s.copy(draft = if on then s.draft -- keys else s.draft ++ keys)
      ) >> price

    private def leaveOut(keys: Vector[String], out: Boolean): Callback =
      $.props.flatMap(p =>
        $.modState(s => s.copy(draft = if out then s.draft -- keys else s.draft, busy = true)) >>
          Callback.future(
            keys.foldLeft(Future.unit)((done, key) =>
              done.flatMap(_ => rpc(p.call, BillingRpc.SetExcluded(key, out)).map(_ => ()))
            ).map(_ => $.modState(_.copy(busy = false)) >> reloadCurrent)
              .recover { case e => $.modState(_.copy(busy = false, error = Some(errorMessage(e)))) }
          )
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

    private def markView(
      s: State,
      th: Threads,
      tz: String,
      pm: PlacedMark,
      column: Int,
      title: Option[String]): VdomNode =
      val members  = pm.mark.members
      val keys     = members.map(_.window.key)
      val state    = members.head.state
      val canTick  = th.bills(members.head.window.threadId) && members.forall(m => draftableKind(m.state))
      val ticked   = canTick && keys.forall(s.draft)
      val canOut   = th.bills(members.head.window.threadId) &&
        (members.forall(m => draftableKind(m.state)) || members.forall(_.state == StretchState.LeftOut))
      val isOut    = members.forall(_.state == StretchState.LeftOut)
      val height   = math.max(pm.yBottom - pm.yTop - 2, 14.0)
      val parts    = members.length
      val tiny     = members.forall(_.durationMs < SessionView.TinyMs)
      val (cls, head, body) = pm.mark match
        case Mark.One(st)  =>
          (
            stateClass(st.state, ticked),
            s"${hoursMinutes(st.durationMs.toDouble)} ${clock(st.window.start.toDouble, tz)} · ${stateWord(st.state)}",
            st.text.getOrElse("")
          )
        case Mark.Cluster(f, _) =>
          val states = members.map(m => stateWord(m.state)).distinct
          (
            stateClass(f.state, ticked) + " border-dotted",
            s"$parts ${if tiny then "short" else "parts"} · ${hoursMinutes(pm.mark.durationMs.toDouble)} ${clock(f.window.start.toDouble, tz)}" +
              (if states.length > 1 then " · mixed" else s" · ${states.head}"),
            members.flatMap(_.text).headOption.getOrElse("")
          )
      <.div(
        ^.key       := keys.head,
        ^.title     := (title.fold("")(_ + "\n") + head + (if body.nonEmpty then s"\n$body" else "")),
        ^.className := s"absolute overflow-hidden rounded border px-1 text-[10px] leading-tight $cls " +
          (if canTick then "cursor-pointer" else ""),
        ^.style     := js.Dynamic.literal(
          "left"   -> (AxisPx + column * ColPx + 2),
          "top"    -> pm.yTop,
          "width"  -> (ColPx - 8),
          "height" -> height
        ),
        ^.onClick --> (if canTick && !s.busy then tick(keys) else Callback.empty),
        title.map(t => <.div(^.className := "truncate font-semibold", t)),
        <.div(
          ^.className := "flex items-center gap-1 font-medium whitespace-nowrap",
          if canTick then <.span(if ticked then "☑" else "☐") else EmptyVdom,
          <.span(^.className := "truncate", head)
        ),
        <.div(^.className := "break-words text-muted-foreground", body.take(160)).when(body.nonEmpty && height > 44),
        <.button(
          ^.`type`    := "button",
          ^.className := "absolute right-0.5 bottom-0.5 rounded bg-background/80 px-1 text-[9px] underline",
          ^.disabled  := s.busy,
          ^.onClick ==> ((e: ReactMouseEvent) => e.stopPropagationCB >> leaveOut(keys, out = !isOut)),
          if isOut then "put back" else "leave out"
        ).when(canOut && height > 26)
      )

    private def boxView(p: PlacedBox, openThread: String => Callback): Vector[VdomNode] =
      val billing = p.box.info.billing
      val cls     = billing match
        case Billing.Bills         => "border-blue-500/70 bg-blue-500/5"
        case Billing.NotClientWork => "border-border bg-muted/40"
        case Billing.Unmarked      => "border-dashed border-amber-500/70 bg-amber-500/5"
      val frame   = <.div(
        ^.key       := s"box-${p.box.info.id}",
        ^.className := s"absolute rounded border $cls",
        ^.style     := js.Dynamic.literal(
          "left"   -> (AxisPx + p.column * ColPx),
          "top"    -> p.yTop,
          "width"  -> (p.span * ColPx - 4),
          "height" -> math.max(p.yBottom - p.yTop, 16.0)
        )
      )
      // A box with marks of its own carries its title on its first mark; a container shows it at its top.
      val title: VdomNode = if p.own.isEmpty then <.button(
        ^.`type`    := "button",
        ^.key       := s"title-${p.box.info.id}",
        ^.className := "absolute truncate text-left text-[10px] font-semibold hover:underline",
        ^.style     := js.Dynamic.literal(
          "left"  -> (AxisPx + p.column * ColPx + 4),
          "top"   -> (p.yTop + 1),
          "width" -> (ColPx - 10)
        ),
        ^.onClick --> openThread(p.box.info.id),
        p.box.info.title
      ) else EmptyVdom
      Vector(frame, title) ++ p.children.flatMap(boxView(_, openThread))

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
      <.div(
        ^.className := "relative",
        ^.style     := js.Dynamic.literal(
          "height" -> (layout.height + 40),
          "width"  -> (layout.columns * ColPx + AxisPx + 20)
        ),
        layout.roots.flatMap(boxView(_, p.openThread)).toVdomArray,
        layout.roots.flatMap(allMarks).map((pm, c, t) => markView(s, th, tz, pm, c, t)).toVdomArray,
        layout.quiet.zipWithIndex.map { (q, i) =>
          <.div(
            ^.key       := s"quiet-$i",
            ^.className := "absolute left-0 bg-muted/60 text-center text-[10px] text-muted-foreground",
            ^.style     := js.Dynamic.literal(
              "top"    -> q.yTop,
              "height" -> (q.yBottom - q.yTop),
              "width"  -> (layout.columns * ColPx + AxisPx)
            ),
            s"${hoursMinutes(q.durationMs.toDouble)} with no attention on any thread"
          )
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

    /** What is ticked, priced per billing root, with what stops a send, and the buttons to send or settle. */
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
                )
              )
            )
          }.toVdomArray
        )

    def render(p: Props, s: State): VdomElement =
      <.div(
        ^.className := "space-y-3",
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
              <.div(
                ^.className := "flex flex-wrap gap-3 text-xs text-muted-foreground",
                <.span("blue: unbilled"),
                <.span("green: sent"),
                <.span("grey: left out or settled"),
                <.span("amber: in Zoho by hand"),
                <.span("ringed ☑: ticked, goes into the next send"),
                <.span("dashed box: thread never marked as client work or not")
              ),
              sendPanel(p, s, tz, screen, th),
              <.div(^.className := "overflow-auto rounded-md border border-border p-2", timeline(p, s, tz, screen, th))
            )
        }.getOrElse(<.div(^.className := "text-sm text-muted-foreground", "Loading…").when(s.loading))
      )

  val Component = ScalaComponent
    .builder[Props]("SessionsView")
    .initialState(State(None, loading = false, error = None, Set.empty, Map.empty, Map.empty, busy = false))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.load(None))
    .componentDidUpdate(x =>
      arrived(x.prevProps.event, x.currentProps.event) match
        case Some(BillingEvent.Changed) => x.backend.reloadCurrent
        case _                          => Callback.empty
    )
    .build
