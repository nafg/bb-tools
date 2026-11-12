package billingapp

import activitycore.{Window, Session, Sessions}

import billingcore.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*

import org.scalajs.dom

import scala.scalajs.js

/** The Sessions tab: one session of attention across every thread, drawn as thread boxes down a shared time axis, each
  * stretch showing its state and what was said or done in it.
  */
object SessionsView:
  final case class Props(call: RpcCall, openThread: String => Callback, event: RealtimeEventJs | Null)
  /** `draft` is the set of ticked stretch keys; `plans` the server's pricing of the ticked stretches per billing root;
    * `shared` the session's shared stretches, decided or not, by billing root, read whatever is ticked (None until
    * read).
    */
  final case class State(
    screen: Option[SessionScreenJs],
    loading: Boolean,
    error: Option[String],
    draft: Set[String],
    plans: Map[String, PlanJs],
    shared: Option[Vector[(String, Vector[Block])]],
    busy: Boolean,
    /** The keys of the mark the pointer is over, whose full detail shows in the card at the corner. */
    hover: Option[Vector[String]],
    /** Where the hover card goes, in window pixels: inside the Sessions panel, on the side away from the pointer. */
    hoverAt: HoverAt,
    /** The billing roots whose summaries are being written, each with when this page first saw it being written. */
    writing: Map[String, Double],
    /** How many summary runs have ended since summaries last started being written, while others still run. */
    written: Int,
    /** How far the save under way has got, when it is made of steps that can be counted. */
    saving: Option[Count],
    /** When each `Busy` activity under way began, by its id. */
    since: Map[String, Double],
    /** The time the busy indicator's clocks read, advanced every second while anything is under way. */
    now: Double,
    /** The thread whose bill-to picker is open under its box's name. */
    picking: Option[String],
    /** The bb projects, with the Zoho projects each bills to, read the first time a picker opens. */
    projects: Option[Vector[ProjectInfo]],
    /** The keys being left out or put back, until the screen that shows the change arrives. */
    leaving: Set[String],
    /** Columns folded or unfolded by hand, by the id of the column's first thread; true is folded. Any other column
      * is folded when it has nothing left to review.
      */
    folding: Map[String, Boolean],
    /** The shared stretches whose decision panel is open, by their keys, and where it was asked for. */
    deciding: Option[Deciding],
    /** The last send or settle made here, as the server recorded it, until dismissed. */
    sent: Option[SubmissionRecord])

  /** A decision panel asked for at a point of the window, for the shared stretches with these keys. */
  final case class Deciding(keys: Vector[String], x: Double, y: Double)

  /** Back-to-back shared stretches of the same threads, decided alike, in the ledger of `root`: one strip on the
    * timeline, decided with one click.
    */
  private final case class SharedRun(root: String, blocks: Vector[Block]):
    def keys: Vector[String]     = blocks.map(_.key)
    def start: Double            = blocks.head.start
    def end: Double              = blocks.last.end
    def durationMs: Double       = blocks.map(_.durationMs).sum
    def threadIds: Vector[String] = blocks.head.threadIds.toVector
    def toDecide: Boolean        = blocks.exists(b => b.decision == null && b.billableThreadIds.nonEmpty)

  /** The decision as text, so stretches decided alike can be told apart from ones decided otherwise. */
  private def decisionText(b: Block): String =
    if b.decision == null then "" else js.JSON.stringify(b.decision.asInstanceOf[js.Any])

  private def keyOf(b: Block): BlockKey =
    BlockKey.parse(b.key).getOrElse(BlockKey(b.day, b.start.toLong, b.end.toLong, b.threadIds.toSet))

  /** The session's shared stretches, each once, run together where they follow one another with the same threads and
    * the same decision.
    */
  private def sharedRuns(shared: Vector[(String, Vector[Block])]): Vector[SharedRun] =
    shared.flatMap((root, blocks) => blocks.map(root -> _)).distinctBy(_._2.key).sortBy(_._2.start)
      .foldLeft(Vector.empty[SharedRun]) { case (runs, (root, b)) =>
        runs.lastOption match
          case Some(last)
              if last.root == root && last.end == b.start && last.threadIds.toSet == b.threadIds.toSet &&
                decisionText(last.blocks.last) == decisionText(b) =>
            runs.init :+ last.copy(blocks = last.blocks :+ b)
          case _ => runs :+ SharedRun(root, Vector(b))
      }

  /** The shared stretches still to decide that hold some of a thread's window. */
  private def undecidedIn(runs: Vector[SharedRun], w: Window): Vector[SharedRun] =
    runs.filter(r => r.toDecide && r.threadIds.contains(w.threadId) && r.start < w.end && r.end > w.start)

  /** The fill of a stretch waiting on a shared-stretch decision: the strips' stripes, faint. */
  private val WaitingHatch = "repeating-linear-gradient(45deg, rgb(245 158 11 / 0.18) 0 6px, transparent 6px 12px)"

  /** How much of a shared stretch a thread got from its decision: all, part, none, or not decided yet. */
  private enum Share:
    case All, Part, NoneOfIt, Undecided

  private def shareOf(b: Block, threadId: String): Share =
    if b.decision == null then Share.Undecided
    else
      val d = b.decision.asInstanceOf[billingapp.Decision]
      d.kind match
        case "wholeTo"  => if d.threadId.contains(threadId) then Share.All else Share.NoneOfIt
        case "portions" =>
          d.ms.toOption.flatMap(_.get(threadId)).filter(_ > 0) match
            case Some(ms) => if ms >= b.durationMs then Share.All else Share.Part
            case None     => Share.NoneOfIt
        case _          => Share.NoneOfIt

  /** The hover card's horizontal place: its left edge and its width. */
  final case class HoverAt(left: Double, width: Double)

  /** `done` of `of` things, `unit` naming one of them. */
  final case class Count(done: Int, of: Int, unit: String)

  /** Something the page is waiting on: `id` keys when it began; `count` is how far along it is, when that is known. */
  final case class Busy(id: String, words: String, count: Option[Count])

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

  /** Where the columns sit across the page, after an axis `axisPx` wide: a folded column is a narrow strip. */
  private final case class Columns(folded: Set[Int], axisPx: Int):
    def width(c: Int): Int              = if folded(c) then FoldedPx else ColPx
    def left(c: Int): Int               = axisPx + (0 until c).map(width).sum
    def spanWidth(c: Int, n: Int): Int  = (c until c + n).map(width).sum

  /** Height of the sticky row naming every column; a box's own sticky name stops just under it. */
  private val HeaderPx = 50
  /** The axis's clock times, right-aligned. */
  private val ClockPx = 60
  /** Lanes left of the clock times for the dots of threads with no box to sit on: one lane per signal kind. */
  private val LanesPx = 32
  /** A shared-stretch strip's width, over the right edge of the marks; and its least height. */
  private val StripPx    = 9
  private val StripMinPx = 14

  /** A signal dot's diameter. */
  private val DotPx   = 5

  /** The signal kinds, in their lanes left to right when they need lanes, each with its dot's colour and what it
    * means.
    */
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
      case "givenAway"    => StretchState.GivenAway(s.to.toOption.fold(Set.empty[String])(_.toSet))
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
      case StretchState.GivenAway(_)    => "given away"

  /** Who a decision gave a thread's time to, in words: `to Manage requests`, `to nobody`. */
  private def givenTo(to: Iterable[String], title: String => String): String =
    if to.isEmpty then "to nobody" else to.map(title).toVector.sorted.mkString("to ", " + ", "")

  /** A running time as `42s`, or `3:07` from a minute on. */
  private def elapsed(ms: Double): String =
    val secs = math.max(0, (ms / 1000).toInt)
    if secs < 60 then s"${secs}s" else f"${secs / 60}:${secs % 60}%02d"

  /** `n` of a noun given in the singular: `1 thread`, `3 stretches`. */
  private def counted(n: Int, noun: String): String =
    if n == 1 then s"$n $noun" else if noun.endsWith("ch") then s"$n ${noun}es" else s"$n ${noun}s"

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
        st =>
          val writing = r.summarizing.toVector.map(root => root -> st.writing.getOrElse(root, js.Date.now())).toMap
          val sameSession = st.screen.flatMap(sessionRoots).map(_._2.start) == sessionRoots(r).map(_._2.start)
          st.copy(
            screen = Some(r),
            shared = if sameSession then st.shared else None,
            loading = false,
            leaving = Set.empty,
            writing = writing,
            written = if writing.isEmpty then 0 else st.written,
            draft = saved.fold(Set.empty[String])(_.intersect(draftable))
          ),
        price >> loadShared
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
            })
        }
      }

    /** The billing roots of the open session's client threads, and the session's span. */
    private def sessionRoots(screen: SessionScreenJs): Option[(Vector[String], Period)] =
      Option.when(screen.current >= 0) {
        val th  = threadsOf(screen)
        val ref = screen.sessions(screen.current)
        val roots = screen.stretches.toVector.map(_.threadId).filter(th.bills).map(th.rootOf).distinct.sorted
        (roots, Period(ref.start.toLong, ref.end.toLong))
      }

    /** Reads the undecided shared stretches of the open session, whatever is ticked: each must be decided before the
      * time in it can be sent.
      */
    private def loadShared: Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        s.screen.flatMap(sessionRoots) match
          case None                                    => $.modState(_.copy(shared = Some(Vector.empty)))
          case Some((roots, _)) if roots.isEmpty       => $.modState(_.copy(shared = Some(Vector.empty)))
          case Some((roots, period))                   =>
            val at = period.start
            Callback.future(
              rpc(p.call, BillingRpc.SessionShared(roots, period))
                .map(r =>
                  $.modState(st =>
                    // A reply for a session no longer open is dropped.
                    if st.screen.flatMap(sessionRoots).exists(_._2.start == at) then
                      st.copy(shared = Some(r.roots.toVector.map(x => x.root -> x.blocks.toVector)))
                    else st
                  )
                )
                .recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
            )
      }

    /** Saves decisions one after another, then reads the session again: a decision changes the state of the
      * stretches it gives away, what is shared, and what the ticked stretches cost.
      */
    private def saveDecisions(calls: Vector[RpcCall => scala.concurrent.Future[Any]]): Callback =
      $.props.flatMap { p =>
        val all = calls.foldLeft(scala.concurrent.Future.unit)((done, call) => done.flatMap(_ => call(p.call).map(_ => ())))
        $.modState(_.copy(busy = true)) >>
          Callback.future(
            all.map(_ => $.modState(_.copy(busy = false, deciding = None)) >> reloadCurrent)
              .recover { case e => $.modState(_.copy(busy = false, error = Some(errorMessage(e)))) }
          )
      }

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
        $.modState(st => st.copy(writing = st.writing + (root -> st.writing.getOrElse(root, js.Date.now())), error = None)) >>
          Callback.future(
            rpc(p.call, BillingRpc.Summarize(root, subthreads = true, DraftPick.Keys(keys.toSet)))
              .map(_ => Callback.empty)
              .recover { case e => $.modState(st => ended(st, root).copy(error = Some(errorMessage(e)))) }
          )
      }

    /** A summary run that ended: counted as done while others still run, the count starting over once none do. */
    private def ended(st: State, root: String): State =
      if !st.writing.contains(root) then st
      else
        val writing = st.writing - root
        st.copy(writing = writing, written = if writing.isEmpty then 0 else st.written + 1)

    def onEvent(event: BillingEvent): Callback =
      event match
        case BillingEvent.SummariesWritten(id)      => $.modState(ended(_, id)) >> reloadCurrent
        case BillingEvent.SummariesFailed(id, why)  =>
          $.modState(st => ended(st, id).copy(error = Some(s"Summaries not written: $why")))
        case BillingEvent.Changed                   => reloadCurrent

    /** Opens or closes a thread's bill-to picker, reading the bb projects the first time. */
    private def togglePicker(id: String): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        val open = $.modState(_.copy(picking = Option.when(!s.picking.contains(id))(id)))
        if s.projects.nonEmpty then open
        else
          open >> Callback.future(
            rpc(p.call, BillingRpc.ProjectMappings)
              .map(r => $.modState(_.copy(projects = Some(r.projects.toVector))))
              .recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
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

    /** Leaves out or puts back the stretches one at a time, counting them off as each is saved. */
    private def leaveOut(keys: Vector[String], out: Boolean): Callback =
      $.props.flatMap { p =>
        def from(i: Int): Callback =
          if i == keys.length then $.modState(_.copy(busy = false, saving = None)) >> reloadCurrent
          else
            Callback.future(
              rpc(p.call, BillingRpc.SetExcluded(keys(i), out))
                .map(_ => $.modState(st => st.copy(saving = st.saving.map(_.copy(done = i + 1)))) >> from(i + 1))
                .recover { case e =>
                  $.modState(st => st.copy(busy = false, saving = None, leaving = st.leaving -- keys, error = Some(errorMessage(e))))
                }
            )
        $.modState(
          s =>
            s.copy(
              draft = if out then s.draft -- keys else s.draft ++ keys,
              busy = true,
              saving = Some(Count(0, keys.length, "stretch")),
              leaving = s.leaving ++ keys
            ),
          saveDraft
        ) >> from(0)
      }

    /** What the page is waiting on now, in the order the indicator lists them. */
    def busyNow(s: State): Vector[Busy] =
      val byRoot  = s.screen.fold(Map.empty[String, Vector[String]])(keysByRoot(_, s.draft))
      // Ticked roots without a plan yet are being priced, unless pricing failed and said so.
      val pricing = s.error.isEmpty && byRoot.keys.exists(r => !s.plans.contains(r))
      Vector(
        Option.when(s.loading)(Busy("loading", "Loading", None)),
        Option.when(!s.loading && s.screen.exists(_.current >= 0) && s.shared.isEmpty)(
          Busy("shared", "Finding shared stretches", None)
        ),
        Option.when(s.busy)(Busy("saving", "Saving", s.saving)),
        Option.when(pricing)(
          Busy("pricing", "Pricing ticked time", Some(Count(byRoot.keys.count(s.plans.contains), byRoot.size, "thread")))
        ),
        Option.when(s.writing.nonEmpty)(
          Busy("writing", "Writing summaries", Some(Count(s.written, s.written + s.writing.size, "thread")))
        )
      ).flatten

    /** Keeps `since` to the activities under way: one that has just begun begins now; one that has ended is dropped. */
    def trackBusy: Callback =
      $.state.flatMap { s =>
        val ids = busyNow(s).map(_.id).toSet
        if ids == s.since.keySet then Callback.empty
        else
          val now = js.Date.now()
          $.modState(st => st.copy(since = ids.map(id => id -> st.since.getOrElse(id, now)).toMap, now = now))
      }

    private var clockTimer: Option[Int] = None

    /** Advances `now` every second, re-drawing the running times, while anything is under way. */
    def startClock: Callback =
      Callback {
        clockTimer = Some(dom.window.setInterval(
          () => $.state.flatMap(s => $.modState(_.copy(now = js.Date.now())).when_(s.since.nonEmpty)).runNow(),
          1000
        ))
      }

    def stopClock: Callback = Callback(clockTimer.foreach(dom.window.clearInterval))

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
      decideRun(root, Vector(key), allocation)

    /** The same decision for each of the stretches, in the ledger of `root`. */
    private def decideRun(root: String, keys: Vector[BlockKey], allocation: Allocation): Callback =
      saveDecisions(keys.map(key => call => rpc(call, BillingRpc.Decide(root, subthreads = true, key, allocation))))

    /** Gives every undecided shared stretch of the open session that has exactly one client thread to that thread. */
    private def settleSole: Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        (s.screen.flatMap(sessionRoots), s.shared) match
          case (Some((_, period)), Some(shared)) =>
            val roots = shared.collect {
              case (root, blocks) if blocks.exists(b => b.decision == null && b.billableThreadIds.length == 1) => root
            }
            saveDecisions(roots.map(root => call => rpc(call, BillingRpc.SettleAll(root, subthreads = true, Some(period)))))
          case _                                => Callback.empty
      }

    private def undecide(key: BlockKey): Callback = undecideRun(Vector(key))

    private def undecideRun(keys: Vector[BlockKey]): Callback =
      saveDecisions(keys.map(key => call => rpc(call, BillingRpc.Undecide(key))))

    private def send(root: String, billed: Boolean): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        (s.screen, s.plans.get(root)) match
          case (Some(screen), Some(plan)) =>
            val keys     = keysByRoot(screen, s.draft).getOrElse(root, Vector.empty)
            val title    = threadsOf(screen).title(root)
            val entries  = plan.entries.toVector.map(e =>
              s"${Words.dayLabel(e.day)}  ${e.logTime}  ${ZohoEntries.notes(e.notes)}"
            ).mkString("\n\n")
            val question =
              if billed then
                s"Send ${hoursMinutes(plan.totalMs)} for \"$title\" to ${targetText(plan)} as these ${plan.entries.length} Zoho entries?\n\n$entries\n\nThe ${keys.length} ticked stretches count as billed from then on."
              else s"Mark the ${keys.length} ticked stretches of \"$title\" (${hoursMinutes(plan.totalMs)}) as settled without billing?"
            if !dom.window.confirm(question) then Callback.empty
            else
              $.modState(_.copy(busy = true)) >>
                Callback.future(
                  rpc(p.call, BillingRpc.Submit(root, subthreads = true, DraftPick.Keys(keys.toSet), billed))
                    .map(r =>
                      $.modState(st => st.copy(busy = false, draft = st.draft -- keys, sent = recordOf(r))) >> reloadCurrent
                    )
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
        case StretchState.GivenAway(_)    => "border-dashed border-border bg-muted/40 text-muted-foreground"
      if ticked then base + " ring-2 ring-primary" else base

    /** A shared-stretch strip's fill for a thread: striped while undecided, green for what it got, grey for none. */
    private def sharedStyle(share: Share): js.Object & js.Dynamic =
      share match
        case Share.Undecided =>
          js.Dynamic.literal("backgroundImage" -> "repeating-linear-gradient(45deg, rgb(245 158 11) 0 3px, transparent 3px 6px)")
        case Share.All       => js.Dynamic.literal("backgroundColor" -> "rgb(34 197 94)")
        case Share.Part      => js.Dynamic.literal("backgroundColor" -> "rgb(34 197 94 / 0.45)")
        case Share.NoneOfIt  => js.Dynamic.literal("backgroundColor" -> "rgb(161 161 170 / 0.7)")

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
        <.div(
          ^.className := "flex flex-wrap items-center gap-x-3 gap-y-1 sm:col-span-2 lg:col-span-3",
          <.span("Dots on a thread's left edge, when:"),
          SignalLanes.map((kind, colour, meaning) =>
            <.span(
              ^.key       := kind,
              ^.title     := kind,
              ^.className := "flex items-center gap-1",
              <.span(^.className := s"inline-block h-[7px] w-[7px] shrink-0 rounded-full $colour"),
              meaning
            )
          ).toVdomArray
        ),
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
        sample("5m 22:28 · to Other thread", StretchState.GivenAway(Set.empty), ticked = false, "Shared time you gave wholly to another thread, or to nobody. Nothing to bill here."),
        <.div(
          ^.key       := "waiting",
          ^.className := "flex items-center gap-2",
          <.div(
            ^.className := "w-40 shrink-0 rounded border border-dashed border-amber-500 bg-background px-1 py-0.5 text-[10px] font-medium leading-tight",
            ^.style     := js.Dynamic.literal("backgroundImage" -> WaitingHatch),
            "shared: decide 5m 22:28"
          ),
          <.span("Some of its time was shared with another thread and who gets it is not decided. Click it to decide; then it can be ticked.")
        ),
        <.div(
          ^.key       := "shared-strip",
          ^.className := "flex items-center gap-2 sm:col-span-2 lg:col-span-3",
          <.div(^.className := "flex w-40 shrink-0 gap-1",
            <.div(^.className := "h-5 w-[6px] rounded-sm border border-amber-500", ^.style := sharedStyle(Share.Undecided)),
            <.div(^.className := "h-5 w-[6px] rounded-sm", ^.style := sharedStyle(Share.All)),
            <.div(^.className := "h-5 w-[6px] rounded-sm", ^.style := sharedStyle(Share.Part)),
            <.div(^.className := "h-5 w-[6px] rounded-sm", ^.style := sharedStyle(Share.NoneOfIt))
          ),
          <.span("A strip at a thread's right edge: time it shared with another thread. Striped: still to decide; green: this thread got it (paler: part of it); grey: another thread or nobody did. Click a strip to decide or change it.")
        ),
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
      title: Option[String],
      runs: Vector[SharedRun]): VdomNode =
      val folded   = cols.folded(column)
      val members  = pm.mark.members
      val keys     = members.map(_.window.key)
      // The shared stretches still to decide that this mark's time is in: a tag on the mark opens the first of them.
      val toDecide = members.flatMap(m => undecidedIn(runs, m.window)).distinct
      val open     = th.bills(members.head.window.threadId) && members.forall(m => draftableKind(m.state))
      // Time that cannot be sent until a shared stretch is decided is not offered for ticking: a click decides instead.
      val waiting  = open && toDecide.nonEmpty
      val canTick  = open && !waiting
      val ticked   = open && keys.forall(s.draft)
      val canOut   = th.bills(members.head.window.threadId) &&
        (members.forall(m => draftableKind(m.state)) || members.forall(_.state == StretchState.LeftOut))
      val isOut    = members.forall(_.state == StretchState.LeftOut)
      val height   = math.max(pm.yBottom - pm.yTop - 2, 14.0)
      val parts    = members.length
      val tiny     = members.forall(_.durationMs < SessionView.TinyMs)
      // Time on a thread that does not bill is shown, never decided: hollow, and "not billable" in place of "unbilled".
      val hollow = !th.bills(members.head.window.threadId) && members.forall(_.state == StretchState.Unbilled)
      def word(st: StretchState): String =
        if hollow then "not billable"
        else
          st match
            case StretchState.GivenAway(to) => givenTo(to, th.title)
            case other                      => stateWord(other)
      def look(st: StretchState): String =
        if hollow then "border-border/60 bg-background font-normal text-muted-foreground/60"
        else if waiting then "border-dashed border-amber-500 bg-background" + (if ticked then " ring-2 ring-primary" else "")
        else stateClass(st, ticked)
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
          (if canTick || waiting then "cursor-pointer " else "") + (if keys.exists(s.leaving) then "animate-pulse opacity-60" else ""),
        ^.style     := {
          val st = js.Dynamic.literal(
            "left"   -> (cols.left(column) + 2),
            "top"    -> pm.yTop,
            "width"  -> (cols.width(column) - 8),
            "height" -> height
          )
          if waiting then st.updateDynamic("backgroundImage")(WaitingHatch)
          st
        },
        ^.onClick ==> ((e: ReactMouseEvent) =>
          if s.busy then Callback.empty
          else if waiting then
            $.modState(_.copy(deciding = Some(Deciding(toDecide.head.keys, e.clientX, e.clientY))))
          else if canTick then tick(keys)
          else Callback.empty
        ),
        title.map(_ => <.div(^.className := "h-[14px]")),
        <.div(
          ^.className := (if hollow then "flex items-center gap-1 whitespace-nowrap" else "flex items-center gap-1 font-medium whitespace-nowrap"),
          if canTick then <.span(if ticked then "☑" else "☐") else EmptyVdom,
          toDecide.headOption.map(run =>
            <.button(
              ^.`type`    := "button",
              ^.title     := s"Part of this time was shared with other threads and is still to decide (${counted(toDecide.length, "shared stretch")}). Click to decide.",
              ^.className := "shrink-0 rounded bg-amber-500 px-1 text-[9px] font-semibold leading-[13px] text-white hover:bg-amber-600",
              ^.onClick ==> ((e: ReactMouseEvent) =>
                e.stopPropagationCB >> $.modState(_.copy(deciding = Some(Deciding(run.keys, e.clientX, e.clientY))))
              ),
              if toDecide.length == 1 then "shared: decide" else s"${toDecide.length} shared: decide"
            )
          ),
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
      case Share    extends ColumnNeed("border-t-4 border-dotted border-amber-500", "Has shared time still to decide.")
      case Bill     extends ColumnNeed("border-t-4 border-blue-500", "Has unbilled time.")
      case Nothing  extends ColumnNeed("border-t-4 border-transparent opacity-60", "Nothing left to review.")

    /** The most urgent need of the threads whose own strips are in one column. */
    private def needOf(th: Threads, boxes: Vector[PlacedBox], toDecide: Set[String]): ColumnNeed =
      val stretches = boxes.flatMap(_.own).flatMap(_.mark.members)
      val billing   = stretches.filter(m => th.bills(m.window.threadId)).map(_.state)
      if billing.exists { case StretchState.InZohoByHand(_) => true; case _ => false } then ColumnNeed.Settle
      else if boxes.exists(b => b.box.info.billing == Billing.Unmarked && b.own.nonEmpty) then ColumnNeed.Decide
      else if boxes.exists(b => toDecide(b.box.info.id)) then ColumnNeed.Share
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
      val runs     = sharedRuns(s.shared.getOrElse(Vector.empty))
      val toDecide = runs.filter(_.toDecide).flatMap(_.threadIds).toSet
      val needs    = byColumn.map((column, boxes) => column -> needOf(th, boxes, toDecide)).toMap
      def columnId(boxes: Vector[PlacedBox]): String = boxes.minBy(_.yTop).box.info.id
      val boxOf    = byColumn.flatMap(_._2).map(b => b.box.info.id -> b).toMap
      // A signal sits on the left edge of its thread's box; one whose thread has no box goes in a lane of the axis,
      // which is widened for the lanes only when some signal needs one.
      val (onBoxes, inLanes) = screen.signals.toVector.zipWithIndex
        .flatMap((g, i) => SignalLanes.indexWhere(_._1 == g.kind) match { case -1 => None; case lane => Some((g, i, lane)) })
        .partition((g, _, _) => boxOf.contains(g.threadId))
      val cols     = Columns(
        byColumn.collect {
          case (column, boxes) if s.folding.getOrElse(columnId(boxes), needs(column) == ColumnNeed.Nothing) => column
        }.toSet,
        ClockPx + (if inLanes.nonEmpty then LanesPx else 0)
      )
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
                BillToSelect.Component.withKey(a.threadId)(BillToSelect.forAccount(
                  a,
                  projects.find(_.projectId == a.projectId),
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
        layout.roots.flatMap(allMarks).map((pm, c, t) => markView(s, th, tz, pm, cols, c, t, runs)).toVdomArray,
        // Each shared stretch as a strip down the right edge of every thread in it that has a box here.
        runs.filter(r => r.toDecide || r.blocks.forall(_.decision != null)).flatMap { run =>
          run.threadIds.flatMap(boxOf.get).map { box =>
            val t      = box.box.info.id
            val share  = shareOf(run.blocks.head, t)
            // A strip is never shorter than StripMinPx, centred on its time, so a stretch of seconds can be found.
            val (top, bottom) =
              val (a, b) = (layout.yAt(run.start.toLong), layout.yAt(run.end.toLong))
              if b - a >= StripMinPx then (a, b) else ((a + b - StripMinPx) / 2, (a + b + StripMinPx) / 2)
            val others = run.threadIds.filterNot(_ == t).map(th.title).mkString(" + ")
            val status = share match
              case Share.Undecided => "who gets it? Click to decide."
              case Share.All       => "this thread got it. Click to change."
              case Share.Part      => "this thread got part of it. Click to change."
              case Share.NoneOfIt  =>
                givenTo(run.blocks.head.threadIds.filter(o => shareOf(run.blocks.head, o) != Share.NoneOfIt), th.title) +
                  ". Click to change."
            <.div(
              ^.key       := s"shared-${run.keys.head}-$t",
              ^.title     := s"${clock(run.start, tz)}–${clock(run.end, tz)} · ${hoursMinutes(run.durationMs)} shared with $others" +
                (if run.blocks.length > 1 then s" (${run.blocks.length} stretches)" else "") + s" · $status",
              ^.className := "absolute z-20 cursor-pointer rounded-sm hover:ring-2 hover:ring-primary " +
                (if share == Share.Undecided then "border border-amber-500 " else "") +
                (if s.deciding.exists(_.keys == run.keys) then "ring-2 ring-primary" else ""),
              ^.style     := {
                val st = sharedStyle(share)
                st.updateDynamic("left")(cols.left(box.column) + cols.width(box.column) - StripPx - 1)
                st.updateDynamic("top")(top)
                st.updateDynamic("width")(StripPx)
                st.updateDynamic("height")(bottom - top)
                st
              },
              ^.onClick ==> ((e: ReactMouseEvent) =>
                e.stopPropagationCB >> $.modState(_.copy(deciding = Some(Deciding(run.keys, e.clientX, e.clientY))))
              )
            ): VdomNode
          }
        }.toVdomArray,
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
        (onBoxes ++ inLanes).map { (g, i, lane) =>
          val title = infos.find(_.id == g.threadId).fold(g.threadId)(_.title)
          val left  = boxOf.get(g.threadId).fold(2.0 + lane * 6)(b => cols.left(b.column) - DotPx / 2.0)
          <.div(
            ^.key       := s"sig-$i",
            // The ring parts dots that overlap, and a dot from the box's border.
            ^.className := s"absolute rounded-full ring-1 ring-background ${SignalLanes(lane)._2}",
            ^.style     := js.Dynamic.literal(
              "top"    -> (layout.yAt(g.at.toLong) - DotPx / 2.0),
              "left"   -> left,
              "width"  -> DotPx,
              "height" -> DotPx
            ),
            ^.title     := s"${g.kind} · ${clock(g.at, tz)} · $title" +
              (if boxOf.contains(g.threadId) then "" else " (a thread with no box in this session)")
          )
        }.toVdomArray,
        layout.axis.zipWithIndex.map { (a, i) =>
          <.div(
            ^.key       := s"ax-$i",
            ^.className := "absolute left-0 text-right text-[10px] text-muted-foreground",
            ^.style     := js.Dynamic.literal("top" -> a._1, "width" -> (cols.axisPx - 6)),
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
              case "givenAway"    =>
                s"Given away: all of it is shared time decided ${givenTo(first.state.to.toOption.fold(Vector.empty[String])(_.toVector), th.title)}. Click the strip at its right edge to change that."
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

    /** How many shared stretches of the session are still to decide, and the one click that gives each with a single
      * client thread to it. The stretches themselves are the striped strips on the timeline.
      */
    private def sharedLine(s: State): VdomNode =
      val runs  = sharedRuns(s.shared.getOrElse(Vector.empty)).filter(_.toDecide)
      val sole  = runs.flatMap(_.blocks).count(b => b.decision == null && b.billableThreadIds.length == 1)
      if runs.isEmpty then EmptyVdom
      else
        <.div(
          ^.className := "flex flex-wrap items-center gap-2 text-xs",
          <.span(
            ^.className := "font-medium",
            s"${counted(runs.length, "shared stretch")} still to decide: the striped strips at the right edge of the threads. Click one to decide it."
          ),
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.title     := "Each stretch that has exactly one client thread in it goes wholly to that thread. The others stay for you to decide.",
            ^.disabled  := s.busy,
            ^.onClick --> settleSole,
            s"Give each to its only client thread ($sole)"
          ).when(sole > 0)
        )

    /** The decision panel of the shared stretches asked for, next to where it was asked for: one stretch with every
      * way to decide it, or a run of them decided together, each still open to its own decision below.
      */
    private def decisionPanel(p: Props, s: State, tz: String, th: Threads): VdomNode =
      val runs = sharedRuns(s.shared.getOrElse(Vector.empty))
      s.deciding.flatMap(d => runs.find(_.keys == d.keys).orElse(runs.find(_.keys.exists(d.keys.contains))).map(d -> _))
        .fold(EmptyVdom: VdomNode) { (d, run) =>
          val width = 440.0
          val left  = math.max(8.0, math.min(d.x + 12, dom.window.innerWidth - width - 8))
          val top   = math.max(8.0, math.min(d.y - 20, dom.window.innerHeight * 0.3))
          def card(b: Block): VdomNode =
            BlockCard.Component.withKey(b.key)(BlockCard.Props(
              b,
              tz,
              b.titles.toMap,
              busy = s.busy,
              key = keyOf(b),
              decide = decide(run.root),
              undecide = undecide,
              openThread = p.openThread,
              foldDecided = false
            ))
          val keys  = run.blocks.map(keyOf)
          <.div(
            ^.className := "fixed z-[80] max-h-[70vh] space-y-2 overflow-auto rounded-md border border-border bg-card p-3 text-xs shadow-lg",
            ^.style     := js.Dynamic.literal("left" -> left, "top" -> top, "width" -> width),
            <.div(
              ^.className := "flex items-center gap-2",
              <.span(
                ^.className := "font-medium",
                s"Shared ${clock(run.start, tz)}–${clock(run.end, tz)} · ${hoursMinutes(run.durationMs)}"
              ),
              <.span(^.className := "text-muted-foreground", s"${run.blocks.length} stretches back to back").when(run.blocks.length > 1),
              <.button(
                ^.`type`    := "button",
                ^.className := "ml-auto rounded px-1 text-muted-foreground hover:bg-muted",
                ^.onClick --> $.modState(_.copy(deciding = None)),
                "✕"
              )
            ),
            if run.blocks.length == 1 then card(run.blocks.head)
            else
              TagMod(
                <.div(
                  ^.className := "flex flex-wrap items-center gap-2",
                  run.threadIds.map(t =>
                    <.button(
                      ^.key       := t,
                      ^.`type`    := "button",
                      ^.className := buttonClass,
                      ^.disabled  := s.busy,
                      ^.onClick --> decideRun(run.root, keys, Allocation.WholeTo(t)),
                      s"All to ${th.title(t).take(30)}"
                    )
                  ).toVdomArray,
                  <.button(
                    ^.`type`    := "button",
                    ^.className := buttonClass,
                    ^.disabled  := s.busy,
                    ^.onClick --> decideRun(run.root, keys, Allocation.Nobody),
                    "Nobody gets it"
                  ),
                  <.button(
                    ^.`type`    := "button",
                    ^.className := buttonClass,
                    ^.disabled  := s.busy,
                    ^.onClick --> undecideRun(run.blocks.filter(_.decision != null).map(keyOf)),
                    "Undo decisions"
                  ).when(run.blocks.exists(_.decision != null))
                ),
                <.details(
                  <.summary(^.className := "cursor-pointer select-none text-muted-foreground", "Each stretch on its own, with what went on in it"),
                  <.div(^.className := "space-y-1", run.blocks.map(card).toVdomArray)
                )
              )
          )
        }

    /** Where a plan's entries go in Zoho, in words. */
    private def targetText(plan: PlanJs): String =
      if plan.target == null then "no Zoho project yet"
      else
        val t = plan.target.asInstanceOf[Target]
        s"${t.customerName}: ${t.projectName} / ${t.taskName}"

    /** Every Zoho entry the plan would create, exactly as it would be sent: date, time, and notes. */
    private def entriesPreview(plan: PlanJs): VdomNode =
      <.div(
        ^.className := "space-y-1 text-xs",
        <.div(
          ^.className := "text-muted-foreground",
          s"Send to Zoho creates ${counted(plan.entries.length, "entry")} in ${targetText(plan)}:"
        ),
        plan.entries.toVector.map { e =>
          val notes = ZohoEntries.notes(e.notes)
          <.div(
            ^.key       := e.day,
            ^.className := "rounded border border-border px-2 py-1",
            <.div(
              ^.className := "flex flex-wrap gap-2",
              <.span(^.className := "font-medium", Words.dayLabel(e.day)),
              <.span(^.className := "font-medium tabular-nums", e.logTime),
              <.span(^.className := "text-muted-foreground", s"(${hoursMinutes(e.exactMs)} recorded)"),
              <.span(
                ^.className := "text-muted-foreground",
                if e.notesEdited then "notes edited by you"
                else if e.notesFromModel then "notes written by the model"
                else "notes are the stretches' lines joined"
              ),
              <.span(^.className := "text-amber-600", s"notes cut to ${notes.length} characters").when(notes != e.notes)
            ),
            <.div(^.className := "whitespace-pre-wrap", notes)
          )
        }.toVdomArray
      )

    /** What the last send or settle made, as the server recorded it. */
    private def sentCard(s: State, tz: String): VdomNode =
      s.sent.fold(EmptyVdom: VdomNode) { r =>
        <.div(
          ^.className := "space-y-1 rounded-md border border-green-500/50 bg-green-500/10 px-3 py-2 text-xs",
          <.div(
            ^.className := "flex flex-wrap items-center gap-2",
            <.span(
              ^.className := "font-medium",
              if r.billed then
                s"Sent to Zoho ${stamp(r.createdAt.toDouble, tz)}: \"${r.rootTitle}\" to ${r.target.customerName}: ${r.target.projectName} / ${r.target.taskName}"
              else
                s"Settled without billing ${stamp(r.createdAt.toDouble, tz)}: \"${r.rootTitle}\", ${counted(r.spans.length, "stretch")}, ${hoursMinutes(r.spans.map(sp => sp.end - sp.start).sum.toDouble)}"
            ),
            <.button(
              ^.`type`    := "button",
              ^.className := "ml-auto rounded px-1 text-muted-foreground hover:bg-muted",
              ^.onClick --> $.modState(_.copy(sent = None)),
              "✕"
            )
          ),
          r.entries.map(e =>
            <.div(
              ^.key       := e.timeEntryId,
              ^.className := "rounded border border-border bg-background px-2 py-1",
              <.div(
                ^.className := "flex flex-wrap gap-2",
                <.span(^.className := "font-medium", Words.dayLabel(e.day)),
                <.span(^.className := "font-medium tabular-nums", ZohoEntries.logTime(e.minutes)),
                <.span(^.className := "text-muted-foreground", s"Zoho entry ${e.timeEntryId}")
              ),
              <.div(^.className := "whitespace-pre-wrap", e.notes)
            )
          ).toVdomArray
        )
      }

    private def sendPanel(p: Props, s: State, tz: String, screen: SessionScreenJs, th: Threads): VdomNode =
      val byRoot = keysByRoot(screen, s.draft)
      if byRoot.isEmpty then
        <.div(^.className := "text-xs text-muted-foreground", "Nothing is ticked. Click a stretch to tick it.")
      else
        <.div(
          ^.className := "space-y-2",
          byRoot.toVector.sortBy(_._1).map { (root, keys) =>
            val plan     = s.plans.get(root)
            val blockers = plan.fold(Vector.empty[String])(_.blockers.toVector)
            val missing  = screen.stretches.toVector.count(l => keys.contains(l.key) && !l.fromModel)
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
              plan.filter(_.entries.length > 0).map(entriesPreview),
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
                  ^.disabled  := (s.busy || s.writing.contains(root)),
                  ^.onClick --> writeSummaries(root),
                  s.writing.get(root).fold(s"Write summaries ($missing without one)")(since =>
                    s"Writing summaries… ${elapsed(s.now - since)}"
                  )
                ).when(missing > 0 || s.writing.contains(root))
              )
            )
          }.toVdomArray
        )

    def render(p: Props, s: State): VdomElement =
      val busy = busyNow(s)
      <.div(
        VdomAttr("data-sessions-panel") := "",
        ^.className := "space-y-3",
        // Sticks to the top of the Sessions panel, wherever the panel sits in the window; takes no height. It is always
        // there, empty when nothing is under way, so the gap the panel puts between its children does not come and go.
        <.div(
          ^.className := "sticky top-0 z-[60] flex h-0 justify-end",
          <.div(
            ^.className := "mt-1 mr-1 h-fit space-y-1 rounded-md border border-border bg-background px-3 py-1 text-xs shadow-md",
            busy.map { b =>
              // A count shows only when there is more than one thing to count; the running time always shows.
              val count = b.count.filter(_.of > 1)
              <.div(
                ^.key       := b.id,
                ^.className := "flex items-center gap-2",
                <.span(^.className := "inline-block h-3 w-3 animate-spin rounded-full border-2 border-muted-foreground border-t-transparent"),
                <.span(b.words + "…"),
                count.map(c => <.progress(^.className := "h-1.5 w-16", VdomAttr("max") := c.of, ^.value := c.done.toString)),
                count.map(c => <.span(^.className := "tabular-nums text-muted-foreground", s"${c.done} of ${counted(c.of, c.unit)}")),
                <.span(^.className := "tabular-nums text-muted-foreground", elapsed(s.now - s.since.getOrElse(b.id, s.now)))
              )
            }.toVdomArray
          ).when(busy.nonEmpty)
        ),
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
                val since   = waiting.keys.flatMap(s.writing.get).minOption
                <.button(
                  ^.`type`    := "button",
                  ^.className := buttonClass,
                  ^.title     := "Asks a model to write a one-line summary of each unbilled stretch in this session that has none, ticked or not.",
                  ^.disabled  := (s.busy || since.nonEmpty),
                  ^.onClick --> summarizeSession,
                  since.fold(s"Write summaries for this session ($count without one)")(t => s"Writing summaries… ${elapsed(s.now - t)}")
                ).when(count > 0)
              },
              s.hover.map(keys => detailCard(s, tz, screen, th, keys)),
              decisionPanel(p, s, tz, th),
              // Above the timeline header's opaque strip, which reaches 40px above the header to hide what scrolls under it.
              <.div(
                ^.className := "relative z-50 space-y-1",
                {
                  val runs     = sharedRuns(s.shared.getOrElse(Vector.empty))
                  val tickable = screen.stretches.toVector.filter(l =>
                    th.bills(l.threadId) && draftableKind(stateOf(l.state)) &&
                      undecidedIn(runs, Window(l.threadId, l.day, l.start.toLong, l.end.toLong)).isEmpty
                  )
                  val ticked   = tickable.count(l => s.draft(l.key))
                  <.div(
                    ^.className := "flex items-center gap-2 text-xs text-muted-foreground",
                    <.progress(^.className := "h-1.5 w-32", VdomAttr("max") := tickable.length, ^.value := ticked.toString),
                    s"$ticked of ${tickable.length} tickable stretches ticked"
                  ).when(tickable.nonEmpty)
                },
                sharedLine(s),
                sentCard(s, tz),
                sendPanel(p, s, tz, screen, th)
              ),
              <.div(^.className := "rounded-md border border-border p-2", timeline(p, s, tz, screen, th))
            )
        }.getOrElse(<.div(^.className := "text-sm text-muted-foreground", "Loading…").when(s.loading))
      )

  val Component = ScalaComponent
    .builder[Props]("SessionsView")
    .initialState(State(None, loading = false, error = None, Set.empty, Map.empty, shared = None, busy = false, hover = None, hoverAt = HoverAt(0, 480), writing = Map.empty, written = 0, saving = None, since = Map.empty, now = js.Date.now(), picking = None, projects = None, leaving = Set.empty, folding = Map.empty, deciding = None, sent = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(x => x.backend.load(None) >> x.backend.startClock)
    .componentDidUpdate(x =>
      x.backend.trackBusy >> (arrived(x.prevProps.event, x.currentProps.event) match
        case Some(event) => x.backend.onEvent(event)
        case None        => Callback.empty)
    )
    .componentWillUnmount(_.backend.stopClock)
    .build
