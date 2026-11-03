package billingapp

import activitycore.Window

import billingcore.{
  Allocation,
  Billability,
  BillingEvent,
  BillingRpc,
  BlockKey,
  CivilDate,
  DayInterval,
  DraftPick,
  SubmissionRecord,
  Submissions,
  Words,
  ZohoTarget
}
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import org.scalajs.dom

import scala.concurrent.Future
import scala.scalajs.js

/** The phase of the thread ledger screen. Every screen is a function of the phase; an event that does not fit the phase
  * is ignored, and a reply to a request that is no longer the latest is dropped.
  */
enum LedgerPhase:
  /** The ledger is being fetched; `shown` is the previous one, kept on screen frozen. */
  case Loading(request: Int, shown: Option[LedgerJs])

  /** The ledger is on screen; stretches can be ticked, struck out, decided. */
  case Reviewing(ledger: LedgerJs)

  /** A model is writing lines and day notes for the draft. */
  case Writing(ledger: LedgerJs)

  /** A send or a settle is in flight. */
  case Sending(ledger: LedgerJs, billed: Boolean)

  /** The transaction is done: the ledger shows the new states, and the banner says what went. */
  case Sent(ledger: LedgerJs, outcome: SubmissionRecord)
  case Failed(shown: Option[LedgerJs], error: String)

  def shownLedger: Option[LedgerJs] =
    this match
      case Loading(_, shown) => shown
      case Reviewing(l)      => Some(l)
      case Writing(l)        => Some(l)
      case Sending(l, _)     => Some(l)
      case Sent(l, _)        => Some(l)
      case Failed(shown, _)  => shown

  def editable: Boolean =
    this match
      case Reviewing(_) => true
      case _            => false

/** The thread ledger: every stretch of the thread's recording with its state, and a draft made by ticking stretches. */
object SubmissionPanel:
  final case class Props(
    threadId: String,
    timeZone: String,
    call: RpcCall,
    onDone: Callback,
    close: Callback,
    openThread: String => Callback,
    /** The latest message on the billing channel: how a summaries run ended, or that something changed. */
    event: RealtimeEventJs | Null)

  final case class State(
    phase: LedgerPhase,
    /** The ticked stretch keys: the draft. */
    draft: Set[String],
    subthreads: Boolean,
    /** The draft as the server priced it: entries, notes, blockers. None while a pricing is in flight. */
    priced: Option[PlanJs],
    pricing: Int,
    nextRequest: Int,
    /** Entry notes as edited, by day. */
    notes: Map[String, String],
    /** The days whose stretches the user has expanded. */
    stretchesOpen: Set[String])

  /** The ticks of each thread's open screen, kept outside the component: if the screen is mounted afresh (the host
    * redrawing it after a change), its ticks come back instead of resetting to everything unbilled.
    */
  private val savedDrafts = scala.collection.mutable.Map.empty[String, Set[String]]

  private def draftableKind(kind: String): Boolean = kind == "unbilled" || kind == "inZohoByHand"

  /** Stretches of one thread in one state with no more than this between them read as one piece of work. */
  private val MergeGapMs: Double = 45 * 60_000.0

  /** A stretch this short, or one with nothing said or done in it, is shown as a one-line row. */
  private val TrivialMs: Double = 2 * 60_000.0

  /** Neighbouring stretches shown as one card: same thread, same state, a short gap between. Ticking and leaving out
    * act on every member; the members keep their own keys.
    */
  private final case class StretchGroup(members: Vector[LedgerStretchJs]):
    def first: LedgerStretchJs    = members.head
    def last: LedgerStretchJs     = members.last
    def keys: Vector[String]      = members.map(_.key)
    def durationMs: Double        = members.map(_.durationMs).sum
    def kind: String              = first.state.kind
    def messages: Vector[UserMessageJs] = members.flatMap(_.messages.toVector)
    def trivial: Boolean          = messages.isEmpty || durationMs < TrivialMs

  private def groupStretches(sorted: Vector[LedgerStretchJs]): Vector[StretchGroup] =
    sorted.foldLeft(Vector.empty[StretchGroup]) { (acc, st) =>
      acc.lastOption match
        case Some(g)
            if g.last.threadId == st.threadId && g.kind == st.state.kind && st.start - g.last.end <= MergeGapMs =>
          acc.init :+ StretchGroup(g.members :+ st)
        case _ => acc :+ StretchGroup(Vector(st))
    }

  /** The draft's total per day, computed here from the ledger so ticking is instant; the server prices the entries. */
  private def localTotals(l: LedgerJs, draft: Set[String]): Map[String, Long] =
    val windows   = l.stretches.toVector.filter(s => draft(s.key)).map(s =>
      Window(s.threadId, s.day, s.start.toLong, s.end.toLong)
    )
    val recording = l.recording.toVector.map(r => DayInterval(r.day, r.threadId, r.start.toLong, r.end.toLong))
    val decisions = l.blocks.toVector.flatMap(b =>
      for
        key   <- BlockKey.parse(b.key)
        d     <- nullable(b.decision)
        alloc <- d.kind match
                   case "wholeTo"  => d.threadId.toOption.map(Allocation.WholeTo.apply)
                   case "portions" =>
                     d.ms.toOption.map(ms => Allocation.Portions(ms.toMap.map((t, v) => t -> v.toLong)))
                   case "nobody"   => Some(Allocation.Nobody)
                   case _          => None
      yield key -> alloc
    ).toMap
    val billable  = l.billableThreadIds.toSet
    val target    = ZohoTarget("", "", "", "", "", "")
    Submissions.plan(
      accounts(l.covered).map(_.threadId),
      windows,
      recording,
      decisions,
      t => if billable(t) then Billability.Billable(target) else Billability.NotBillable
    ).totalsByDay

  final class Backend($ : BackendScope[Props, State]):
    private val root = Ref[dom.html.Div]

    def scrollIntoView: Callback =
      root.foreach(el =>
        val _ = el.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal("behavior" -> "smooth", "block" -> "start"))
      )

    /** Move to `phase` only if the request `id` is still the latest one; a slower, older reply changes nothing. */
    private def arrive(id: Int)(phase: LedgerPhase): Callback =
      $.modState(s =>
        s.phase match
          case LedgerPhase.Loading(current, _) if current == id => s.copy(phase = phase)
          case _                                                => s
      )

    /** Fetch the ledger (after `before`, a change to it, has been applied), then price the draft. The draft keeps its
      * ticks across reloads, even when none are ticked; keys that are gone or no longer draftable fall away. The first
      * load, or one with `keepDraft` off, ticks everything unbilled.
      */
    def reload(before: Props => Future[Unit] = _ => Future.unit, keepDraft: Boolean = true): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        val id = s.nextRequest
        $.modState(_.copy(phase = LedgerPhase.Loading(id, s.phase.shownLedger), nextRequest = id + 1)) >>
          Callback.future(
            before(p)
              .flatMap(_ =>
                rpc(p.call, BillingRpc.Ledger(p.threadId, s.subthreads))
              )
              .map { l =>
                val draftable = l.stretches.toVector.filter(st => draftableKind(st.state.kind)).map(_.key).toSet
                val allUnbilled = l.stretches.toVector.filter(_.state.kind == "unbilled").map(_.key).toSet
                // The ticks as they are when the reply arrives, not as they were when the fetch began.
                val restored = savedDrafts.get(p.threadId)
                val keep     = keepDraft && (s.phase.shownLedger.isDefined || restored.isDefined)
                arrive(id)(LedgerPhase.Reviewing(l)) >>
                  $.modState(
                    st =>
                      val base = if s.phase.shownLedger.isDefined then st.draft else restored.getOrElse(st.draft)
                      st.copy(draft = if keep then base.intersect(draftable) else allUnbilled),
                    price
                  )
              }
              .recover { case e => arrive(id)(LedgerPhase.Failed(s.phase.shownLedger, errorMessage(e))) }
          )
      }

    /** Ask the server to price the current draft: entries with notes and blockers. Stale replies are dropped. */
    def price: Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        val id = s.pricing + 1
        if s.draft.isEmpty then savedDrafts -= p.threadId else savedDrafts(p.threadId) = s.draft
        $.modState(_.copy(pricing = id, priced = None)) >>
          Callback.future(
            rpc(p.call, BillingRpc.Draft(p.threadId, s.subthreads, DraftPick.Keys(s.draft)))
              .map(pl =>
                $.modState(st =>
                  if st.pricing == id then
                    st.copy(priced = Some(pl), notes = pl.entries.map(e => e.day -> e.notes).toMap)
                  else st
                )
              )
              .recover { case _ => Callback.empty }
          )
      }

    /** A change to the draft: local and instant; the server prices it afterwards. */
    private def setDraft(f: (LedgerJs, Set[String]) => Set[String]): Callback =
      $.state.flatMap(s =>
        s.phase match
          case LedgerPhase.Reviewing(l) => $.modState(_.copy(draft = f(l, s.draft)), price)
          case _                        => Callback.empty
      )

    private def tick(g: StretchGroup): Callback =
      setDraft((_, d) => if g.keys.forall(d) then d -- g.keys else d ++ g.keys)

    /** "Up to here": every unbilled stretch that ends at or before this one, and this one. */
    def upToHere(st: LedgerStretchJs): Callback =
      setDraft((l, _) =>
        l.stretches.toVector.filter(x => x.state.kind == "unbilled" && x.end <= st.end).map(_.key).toSet + st.key
      )

    def tickDay(day: String, on: Boolean): Callback =
      setDraft((l, d) =>
        val keys = l.stretches.toVector.filter(x => x.day == day && x.state.kind == "unbilled").map(_.key).toSet
        if on then d ++ keys else d -- keys
      )

    def tickAll(on: Boolean): Callback =
      setDraft((l, _) =>
        if on then l.stretches.toVector.filter(_.state.kind == "unbilled").map(_.key).toSet else Set.empty
      )

    def setSubthreads(on: Boolean): Callback = $.modState(_.copy(subthreads = on), reload(keepDraft = false))

    /** A change to the ledger itself (strike-out, decision): only while reviewing; reloads. */
    private def change(run: Props => Future[Unit]): Callback =
      $.state.flatMap(s => if s.phase.editable then reload(before = run) else Callback.empty)

    private def leaveOut(g: StretchGroup, out: Boolean): Callback =
      $.modState(s => s.copy(draft = if out then s.draft -- g.keys else s.draft)) >>
        change(p =>
          g.keys.foldLeft(Future.unit)((done, key) => done.flatMap(_ => rpc(p.call, BillingRpc.SetExcluded(key, out)).map(_ => ())))
        )

    def decide(key: BlockKey, allocation: Allocation): Callback =
      $.state.flatMap(s =>
        change(p => rpc(p.call, BillingRpc.Decide(p.threadId, s.subthreads, key, allocation)).map(_ => ()))
      )

    def undecide(key: BlockKey): Callback = change(p => rpc(p.call, BillingRpc.Undecide(key)).map(_ => ()))

    private def toggleStretches(day: String): Callback =
      $.modState(s => s.copy(stretchesOpen = if s.stretchesOpen(day) then s.stretchesOpen - day else s.stretchesOpen + day))

    def editNotes(day: String, text: String): Callback = $.modState(s => s.copy(notes = s.notes.updated(day, text)))

    private def setNotes(day: CivilDate, text: String): Callback =
      $.props.flatMap(p =>
        Callback.future(
          rpc(p.call, BillingRpc.SetNotes(p.threadId, day, Option(text).filter(_.trim.nonEmpty)))
            .map(_ => price)
            .recover { case e =>
              $.modState(s => s.copy(phase = LedgerPhase.Failed(s.phase.shownLedger, errorMessage(e))))
            }
        )
      )

    /** The entry's day as the server named it: its `yyyy-MM-dd`. */
    private def onDay(day: String)(f: CivilDate => Callback): Callback =
      CivilDate.parseOption(day).fold(
        $.modState(s => s.copy(phase = LedgerPhase.Failed(s.phase.shownLedger, s"not a day: $day")))
      )(f)

    def saveNotes(day: String): Callback  =
      $.state.flatMap(s => onDay(day)(d => setNotes(d, s.notes.getOrElse(day, ""))))
    def resetNotes(day: String): Callback = onDay(day)(setNotes(_, ""))

    /** Starts one model run for the draft's stretches without a line and its days without notes. The run can take
      * minutes, so the request only starts it; `BillingEvent` says when it ended (see `onEvent`).
      */
    def writeSummaries: Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        s.phase match
          case LedgerPhase.Reviewing(l) =>
            $.modState(_.copy(phase = LedgerPhase.Writing(l))) >>
              Callback.future(
                rpc(p.call, BillingRpc.Summarize(p.threadId, s.subthreads, DraftPick.Keys(s.draft)))
                  .map(_ => Callback.empty)
                  .recover { case e => $.modState(_.copy(phase = LedgerPhase.Failed(Some(l), errorMessage(e)))) }
              )
          case _                        => Callback.empty
      }

    /** What the backend published, against the phase: a finished run shows its lines; a failed one says why. */
    def onEvent(event: BillingEvent): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        (s.phase, event) match
          case (LedgerPhase.Writing(_), BillingEvent.SummariesWritten(id)) if id == p.threadId       => reload()
          case (LedgerPhase.Writing(l), BillingEvent.SummariesFailed(id, error)) if id == p.threadId =>
            $.modState(_.copy(phase = LedgerPhase.Failed(Some(l), s"Summaries not written: $error")))
          // A run started before this screen opened, or a change made elsewhere: the ledger is stale.
          case (LedgerPhase.Reviewing(_), BillingEvent.SummariesWritten(id)) if id == p.threadId     => reload()
          case (LedgerPhase.Reviewing(_), BillingEvent.Changed)                                      => reload()
          case _                                                                                     => Callback.empty
      }

    /** Send (or settle) exactly the ticked stretches. The confirmation states what will happen. */
    def submit(billed: Boolean): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        (s.phase, s.priced) match
          case (LedgerPhase.Reviewing(l), Some(pl)) =>
            val target   = nullable(l.target).fold("")(t => s"${t.customerName}: ${t.projectName} / ${t.taskName}")
            val question =
              if billed then
                s"Send ${hoursMinutes(pl.totalMs)} as ${pl.entries.length} Zoho ${
                    if pl.entries.length == 1 then "entry" else "entries"
                  } (${pl.entries.map(e => Words.dayLabel(e.day)).mkString(", ")}) to $target? The ${s.draft.size} ticked stretches count as billed from then on; the rest stays unbilled."
              else
                s"Mark the ${s.draft.size} ticked stretches (${hoursMinutes(pl.totalMs)}) as settled without billing? They will not be offered again (undo is possible)."
            if !dom.window.confirm(question) then Callback.empty
            else
              $.modState(_.copy(phase = LedgerPhase.Sending(l, billed))) >>
                Callback.future(
                  rpc(p.call, BillingRpc.Submit(p.threadId, s.subthreads, DraftPick.Keys(s.draft), billed))
                    .map(reply =>
                      recordOf(reply) match
                        // The ledger is fetched again so the ticked stretches flip to sent or settled in place.
                        case Some(sub) =>
                          $.modState(_.copy(phase = LedgerPhase.Sent(l, sub), draft = Set.empty, priced = None)) >>
                            p.onDone >> reloadKeepingSent(sub)
                        case None      =>
                          $.modState(_.copy(phase =
                            LedgerPhase.Failed(Some(l), "the send's reply was not a submission")
                          )) >>
                            p.onDone >> reload()
                    )
                    // Entries that did reach Zoho before a failure stand as a submission; the overview must know.
                    .recover { case e =>
                      $.modState(_.copy(phase = LedgerPhase.Failed(Some(l), errorMessage(e)))) >> p.onDone >> reload()
                    }
                )
          case _                                    => Callback.empty
      }

    /** After a send: fetch the ledger again but stay in the Sent phase, so the banner and the new chips show together.
      */
    private def reloadKeepingSent(sub: SubmissionRecord): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        Callback.future(
          rpc(p.call, BillingRpc.Ledger(p.threadId, s.subthreads))
            .map(l => $.modState(_.copy(phase = LedgerPhase.Sent(l, sub))))
            .recover { case _ => Callback.empty }
        )
      }

    def undo(sub: SubmissionRecord): Callback =
      $.props.flatMap { p =>
        if !dom.window.confirm(
            if sub.billed then "Undo this send? Its Zoho entries are deleted and its stretches are unbilled again."
            else "Undo this settle? Its stretches are unbilled again."
          )
        then Callback.empty
        else
          $.modState(s => s.copy(phase = s.phase.shownLedger.fold(s.phase)(LedgerPhase.Reviewing.apply))) >>
            reload(before =
              p => rpc(p.call, BillingRpc.Undo(sub.id)).map(_ => ())
            ) >> p.onDone
      }

    /** The phase, stated in one line at the top of the screen. */
    private def status(s: State): VdomElement =
      val (text, cls) = s.phase match
        case LedgerPhase.Sending(_, true)  => ("Sending to Zoho Books…", "bg-amber-500/15")
        case LedgerPhase.Sending(_, false) => ("Marking settled…", "bg-amber-500/15")
        case LedgerPhase.Writing(_)        =>
          (
            "Writing summaries in a hidden thread; a long draft can take a few minutes, and the run goes on if you leave this screen. Nothing is sent.",
            "bg-amber-500/15"
          )
        case LedgerPhase.Loading(_, None)  => ("Loading the thread's recording…", "bg-muted")
        case LedgerPhase.Loading(_, _)     => ("Updating…", "bg-muted")
        case LedgerPhase.Failed(_, _)      =>
          ("Failed. The message below says what did and did not happen.", "bg-destructive/15 text-destructive")
        case LedgerPhase.Sent(_, sub)      =>
          (
            if sub.billed then "Sent. The ticked stretches are billed now."
            else "Settled. The ticked stretches count as settled now.",
            "bg-green-500/15"
          )
        case LedgerPhase.Reviewing(_)      => ("Reviewing. Nothing is sent until you press Send.", "bg-muted")
      <.div(^.className := s"rounded-md px-3 py-2 text-sm font-medium $cls", text)

    /** The stretch's state as a chip, in words; an unbilled stretch has none. */
    private def stateChip(l: LedgerJs, st: LedgerStretchJs): Option[VdomElement] =
      def chip(cls: String, text: String, title: String = "") =
        <.span(^.className := s"rounded px-1.5 py-0.5 text-xs $cls", ^.title := title, text)
      def on(id: js.UndefOr[String])                          =
        id.toOption.flatMap(i => records(l.history).find(_.id == i)).map(sub =>
          stamp(sub.createdAt.toDouble, l.timeZone)
        ).getOrElse("")
      st.state.kind match
        case "sent"         => Some(chip("bg-green-500/20", s"sent ${on(st.state.submissionId)}"))
        case "settled"      => Some(chip("bg-muted text-muted-foreground", s"settled ${on(st.state.submissionId)}"))
        case "leftOut"      => Some(chip("bg-muted text-muted-foreground", "left out"))
        case "inZohoByHand" =>
          Some(chip(
            "bg-amber-500/20",
            "in Zoho by hand",
            st.state.trace.toOption.fold("")(t => traceText(t, l.timeZone)) + ". Tick it to include it anyway."
          ))
        case _              => None

    private def tickBox(s: State, g: StretchGroup, frozen: Boolean) =
      <.input(
        ^.`type`    := "checkbox",
        ^.className := "mt-1 h-5 w-5 shrink-0",
        ^.checked   := g.keys.forall(s.draft),
        ^.disabled  := (frozen || !draftableKind(g.kind)),
        ^.onChange --> tick(g)
      )

    private def range(l: LedgerJs, g: StretchGroup): String =
      s"${clock(g.first.start, l.timeZone)}–${clock(g.last.end, l.timeZone)}"

    private def leaveOutButton(g: StretchGroup, frozen: Boolean) =
      <.button(
        ^.`type`    := "button",
        ^.className := "underline text-muted-foreground",
        ^.disabled  := frozen,
        ^.onClick --> leaveOut(g, out = g.kind != "leftOut"),
        if g.kind == "leftOut" then "put back" else "leave out for good"
      ).when(draftableKind(g.kind) || g.kind == "leftOut")

    /** A one-line row for a stretch that needs no reading: the same ticking and leaving out, none of the card. */
    private def compactRow(l: LedgerJs, s: State, g: StretchGroup, frozen: Boolean): VdomNode =
      <.div(
        ^.key       := g.first.key,
        ^.className := "flex items-center gap-x-2 whitespace-nowrap rounded-md border px-3 py-1 text-xs " +
          (if g.keys.forall(s.draft) then "border-primary/60 bg-primary/5" else "border-border"),
        tickBox(s, g, frozen),
        <.span(^.className := "font-medium tabular-nums", range(l, g)),
        <.span(^.className := "text-muted-foreground", s"${hoursMinutes(g.durationMs)} recorded"),
        <.span(
          ^.className := "min-w-0 flex-1 truncate text-muted-foreground",
          if g.messages.isEmpty then "nothing said or done" else g.members.map(_.summary).mkString(" ")
        ),
        stateChip(l, g.first),
        leaveOutButton(g, frozen)
      )

    private def groupCard(l: LedgerJs, s: State, g: StretchGroup, frozen: Boolean): VdomNode =
      val ticked = g.keys.forall(s.draft)
      val st     = g.first
      <.div(
        ^.key       := st.key,
        ^.className := "rounded-md border px-3 py-2 text-sm " +
          (if ticked then "border-primary/60 bg-primary/5" else "border-border"),
        <.div(
          ^.className := "flex items-start gap-2",
          tickBox(s, g, frozen),
          <.div(
            ^.className := "min-w-0 flex-1 space-y-1",
            <.div(
              ^.className := "flex flex-wrap items-center gap-x-2 gap-y-1",
              <.span(^.className := "font-medium tabular-nums", range(l, g)),
              <.span(^.className := "text-muted-foreground", s"${hoursMinutes(g.durationMs)} recorded"),
              <.span(^.className := "text-xs text-muted-foreground", s"(${g.members.length} stretches)").when(
                g.members.length > 1
              ),
              <.span(^.className := "text-xs text-muted-foreground", st.title.take(40)).when(l.covered.length > 1),
              stateChip(l, st)
            ),
            <.div(
              ^.className := (if g.kind == "leftOut" then "text-muted-foreground line-through" else ""),
              g.members.map(_.summary).mkString(" "),
              <.span(^.className := "ml-1 text-xs text-muted-foreground", "(first message; no summary yet)").when(
                g.members.exists(!_.fromModel)
              )
            ),
            <.details(
              ^.className := "text-xs text-muted-foreground",
              <.summary(
                ^.className := "cursor-pointer select-none",
                s"${g.messages.count(_.speaker == "User")} from you, ${g.messages.count(
                    _.speaker == "Agent"
                  )} replies, ${g.messages.count(_.speaker == "Work")} actions"
              ),
              conversationList(js.Array(g.messages*), l.timeZone),
              <.button(
                ^.`type`    := "button",
                ^.className := "underline",
                ^.onClick --> $.props.flatMap(_.openThread(st.threadId)),
                "Open the thread"
              )
            ),
            <.div(
              ^.className := "flex flex-wrap gap-3 text-xs",
              <.button(
                ^.`type`    := "button",
                ^.className := "underline text-muted-foreground",
                ^.disabled  := frozen,
                ^.onClick --> upToHere(g.last),
                "tick everything up to here"
              ).when(draftableKind(g.kind)),
              leaveOutButton(g, frozen)
            )
          )
        )
      )

    private def groupView(l: LedgerJs, s: State, g: StretchGroup, frozen: Boolean): VdomNode =
      if g.trivial then compactRow(l, s, g, frozen) else groupCard(l, s, g, frozen)

    private def entryBox(s: State, e: EntryJs, frozen: Boolean): VdomElement =
      val shown = s.notes.getOrElse(e.day, e.notes)
      <.div(
        ^.className := "space-y-1 rounded-md border border-primary/40 bg-primary/5 px-3 py-2 text-sm",
        <.div(
          ^.className := "flex flex-wrap items-center gap-2 text-xs",
          <.span(^.className := "font-medium", s"Entry for ${Words.dayLabel(e.day)}"),
          <.span(s"${hoursMinutes(e.exactMs)} recorded, ${e.logTime} to Zoho"),
          <.span(
            ^.className      := "text-muted-foreground",
            if e.notesEdited then "(notes edited)"
            else if e.notesFromModel then "(notes written by the model)"
            else "(the lines joined; \"Write summaries\" writes a note)"
          )
        ),
        <.textarea(
          ^.className := inputClass + " w-full",
          ^.rows      := 3,
          ^.disabled  := frozen,
          ^.value     := shown,
          ^.onChange ==> ((ev: ReactEventFromInput) => { val v = ev.target.value; editNotes(e.day, v) })
        ),
        <.div(
          ^.className := "flex gap-2",
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := (frozen || shown == e.notes),
            ^.onClick --> saveNotes(e.day),
            "Save notes"
          ),
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := frozen,
            ^.onClick --> resetNotes(e.day),
            "Use the generated notes"
          ).when(e.notesEdited)
        )
      )

    /** Each covered thread's account, in the same words as the lists, then what the draft holds. */
    private def summaryLine(l: LedgerJs, s: State): VdomElement =
      val covered = accounts(l.covered)
      val inDraft = localTotals(l, s.draft).values.sum
      <.div(
        ^.className := "space-y-0.5 text-sm",
        covered.map(a =>
          <.div(
            ^.key := a.threadId,
            <.span(^.className := "font-medium", a.title.take(40)).when(covered.length > 1),
            <.span(": ").when(covered.length > 1),
            Words.totals(a.totals, a.submissions)
          )
        ).toVdomArray,
        <.div(
          ^.className := "font-medium",
          if s.draft.isEmpty then "Nothing ticked" else s"${hoursMinutes(inDraft.toDouble)} to bill from the ticked stretches"
        )
      )

    private def daySection(
      l: LedgerJs,
      s: State,
      day: String,
      stretches: Vector[LedgerStretchJs],
      frozen: Boolean): VdomNode =
      val unbilled = stretches.filter(_.state.kind == "unbilled")
      val allOn    = unbilled.nonEmpty && unbilled.forall(st => s.draft(st.key))
      val ticked   = localTotals(l, s.draft).get(day)
      val blocks   = l.blocks.toVector.filter(_.day == day)
      val entry    = s.priced.flatMap(_.entries.find(_.day == day))
      val (leftOut, live) = groupStretches(stretches).partition(_.kind == "leftOut")
      <.section(
        ^.key       := day,
        ^.className := "space-y-2",
        <.div(
          ^.className := "flex flex-wrap items-center justify-between gap-2 border-b border-border pb-1",
          <.div(
            <.span(^.className := "font-semibold", Words.dayLabel(day)),
            <.span(
              ^.className      := "ml-2 text-xs text-muted-foreground",
              s"${hoursMinutes(unbilled.map(_.durationMs).sum)} recorded, unbilled" +
                ticked.fold("")(t => s" · ${hoursMinutes(t.toDouble)} to bill from the ticked (time shared with other threads counts only as decided)")
            )
          ),
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := (frozen || unbilled.isEmpty),
            ^.onClick --> tickDay(day, !allOn),
            if allOn then "Untick day" else "Tick day"
          )
        ),
        entry.map(e => entryBox(s, e, frozen)),
        blocks.flatMap(b => BlockKey.parse(b.key).map(b -> _)).map { (b, key) =>
          BlockCard.Component.withKey(b.key)(BlockCard.Props(
            b,
            l.timeZone,
            b.titles.toMap,
            busy = frozen,
            key = key,
            decide = decide,
            undecide = undecide,
            openThread = id => $.props.flatMap(_.openThread(id))
          )): VdomNode
        }.toVdomArray,
        // The day's entry is the thing sent; its stretches are the evidence, open when there is no entry to read.
        <.details(
          ^.className := "space-y-2",
          ^.open      := s.stretchesOpen(day),
          <.summary(
            ^.className := "cursor-pointer select-none text-xs font-medium",
            ^.onClick ==> (e => e.preventDefaultCB >> toggleStretches(day)),
            s"Stretches (${live.length})"
          ),
          <.div(^.className := "space-y-2 pt-2", live.map(g => groupView(l, s, g, frozen)).toVdomArray)
        ).when(live.nonEmpty),
        <.details(
          ^.className := "space-y-1 text-xs",
          <.summary(
            ^.className := "cursor-pointer select-none font-medium",
            s"${leftOut.length} left out (${hoursMinutes(leftOut.map(_.durationMs).sum)} recorded)"
          ),
          <.div(^.className := "space-y-1 pt-1", leftOut.map(g => compactRow(l, s, g, frozen)).toVdomArray)
        ).when(leftOut.nonEmpty)
      )

    private def ledgerScreen(p: Props, s: State, l: LedgerJs): VdomElement =
      val frozen = !s.phase.editable
      val byDay  = l.stretches.toVector.groupBy(_.day).toVector.sortBy(_._1)
      val priced = s.priced
      val total  = localTotals(l, s.draft).values.sum
      <.div(
        ^.className := "space-y-4" + (if frozen then " opacity-80" else ""),
        summaryLine(l, s),
        <.div(
          ^.className := "text-xs text-muted-foreground",
          nullable(l.target).fold("Not client work: choose where this thread bills first.")(t =>
            s"Bills to ${t.customerName}: ${t.projectName} / ${t.taskName}"
          ) +
            (if l.leftOut.nonEmpty then
               s". Left out: ${l.leftOut.map(x => s"${accountOf(x.thread).fold("?")(_.title)} (${x.why})").mkString("; ")}"
             else "")
        ),
        <.div(
          ^.className := "flex flex-wrap items-center gap-2",
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := frozen,
            ^.onClick --> tickAll(true),
            "Tick all unbilled"
          ),
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := (frozen || s.draft.isEmpty),
            ^.onClick --> tickAll(false),
            "Untick all"
          ),
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := (frozen || !l.summariesAvailable || s.draft.isEmpty),
            ^.title := "One hidden model run writes the missing lines of the ticked stretches and the notes of their days",
            ^.onClick --> writeSummaries,
            "Write summaries"
          ),
          <.label(
            ^.className := "flex items-center gap-1 text-xs",
            <.input(
              ^.`type`    := "checkbox",
              ^.className := "h-4 w-4",
              ^.checked   := s.subthreads,
              ^.disabled  := frozen,
              ^.onChange ==> ((e: ReactEventFromInput) => { val v = e.target.checked; setSubthreads(v) })
            ),
            <.span("With subthreads")
          )
        ),
        nullable(l.handEnteredError).map(e => banner("warn", s"Could not read Zoho's existing entries: $e")),
        <.div(^.className := "text-sm text-muted-foreground", "Nothing recorded for this thread.").when(
          l.stretches.isEmpty
        ),
        byDay.map((day, sts) => daySection(l, s, day, sts.sortBy(_.start), frozen)).toVdomArray,
        <.details(
          ^.className := "text-xs",
          <.summary(
            ^.className := "cursor-pointer select-none font-medium",
            s"Sent or settled before (${l.history.length})"
          ),
          <.div(
            ^.className := "space-y-1 pt-1",
            records(l.history).map(sub =>
              <.div(
                ^.key       := sub.id,
                ^.className := "flex flex-wrap items-center justify-between gap-2 border-b border-border py-1",
                <.span(Words.submission(sub, ms => stamp(ms.toDouble, l.timeZone))),
                <.button(
                  ^.`type`    := "button",
                  ^.className := buttonClass,
                  ^.disabled  := frozen,
                  ^.onClick --> undo(sub),
                  if sub.billed then "Undo this send" else "Undo this settle"
                )
              )
            ).toVdomArray,
            <.div(^.className := "text-muted-foreground", "Nothing yet.").when(l.history.isEmpty)
          )
        ),
        <.details(
          ^.className := "text-xs",
          <.summary(
            ^.className := "cursor-pointer select-none font-medium",
            s"In Zoho on these days, not from bb (${l.handEntered.length})"
          ),
          <.div(
            ^.className := "space-y-1 pt-1",
            l.handEntered.map(h =>
              <.div(
                ^.key := h.timeEntryId,
                s"${h.date}: ${hoursMinutes(h.minutes * 60_000)} ${h.taskName} by ${h.userName}: ${h.notes} (${h.billedStatus})"
              )
            ).toVdomArray
          )
        ).when(l.handEntered.nonEmpty),
        // The action bar stays at the bottom of the screen, with the running total.
        <.div(
          ^.className := "sticky bottom-0 -mx-4 flex flex-wrap items-center gap-2 border-t border-border bg-card px-4 py-3",
          <.button(
            ^.`type`    := "button",
            ^.className := primaryButtonClass + " min-h-11",
            ^.disabled  := (frozen || s.draft.isEmpty || priced.isEmpty || priced.exists(_.blockers.nonEmpty)),
            ^.onClick --> submit(billed = true),
            priced match
              case Some(pl) if pl.entries.nonEmpty =>
                s"Send ${hoursMinutes(pl.totalMs)} in ${pl.entries.length} ${
                    if pl.entries.length == 1 then "entry" else "entries"
                  }"
              case _ if s.draft.nonEmpty           => s"Send ${hoursMinutes(total.toDouble)}…"
              case _                               => "Tick stretches to send"
          ),
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass + " min-h-11",
            ^.disabled  := (frozen || s.draft.isEmpty || priced.isEmpty || l.target == null),
            ^.title     := "The ticked stretches count as settled and are not offered again",
            ^.onClick --> submit(billed = false),
            "Settle without billing"
          ),
          priced.toVector.flatMap(_.blockers.toVector).map(b =>
            <.span(^.key := b, ^.className := "text-xs text-destructive", b)
          ).toVdomArray
        )
      )

    def render(p: Props, s: State): VdomElement =
      <.div.withRef(root)(
        ^.className := "space-y-4 rounded-lg border border-primary/40 bg-card p-4",
        <.div(
          ^.className := "flex items-start justify-between gap-2",
          <.div(
            ^.className := "text-base font-semibold",
            s.phase.shownLedger.flatMap(l => accountOf(l.root)).fold("Bill this thread")(_.title)
          ),
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass + " min-h-9 shrink-0",
            ^.onClick --> p.close,
            "Close"
          )
        ),
        status(s),
        s.phase match
          case LedgerPhase.Sent(l, sub)         =>
            <.div(
              ^.className := "space-y-4",
              banner(
                "info",
                <.div(^.className := "font-medium", Words.submission(sub, ms => stamp(ms.toDouble, l.timeZone))),
                <.div(
                  ^.className     := "mt-1 flex flex-wrap gap-2",
                  <.button(
                    ^.`type`    := "button",
                    ^.className := buttonClass,
                    ^.onClick --> $.modState(_.copy(phase = LedgerPhase.Reviewing(l))),
                    "Continue"
                  ),
                  <.button(
                    ^.`type`    := "button",
                    ^.className := buttonClass,
                    ^.onClick --> undo(sub),
                    if sub.billed then "Undo this send" else "Undo this settle"
                  )
                )
              ),
              ledgerScreen(p, s, l)
            )
          case LedgerPhase.Failed(shown, error) =>
            <.div(
              ^.className := "space-y-4",
              banner(
                "error",
                <.div(error),
                <.button(^.`type` := "button", ^.className := "mt-1 " + buttonClass, ^.onClick --> reload(), "Reload")
              ),
              shown.map(l => ledgerScreen(p, s, l))
            )
          case LedgerPhase.Loading(_, None)     => <.div(^.className := "text-sm text-muted-foreground", "Loading…")
          case other                            => other.shownLedger.map(l => ledgerScreen(p, s, l))
      )

  val Component = ScalaComponent
    .builder[Props]("SubmissionPanel")
    .initialState(State(
      phase = LedgerPhase.Loading(0, None),
      draft = Set.empty,
      subthreads = true,
      priced = None,
      pricing = 0,
      nextRequest = 0,
      notes = Map.empty,
      stretchesOpen = Set.empty
    ))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    // The screen opens below the list, out of view on a long page: bring it to the top of the viewport.
    .componentDidMount(x => x.backend.scrollIntoView >> x.backend.reload())
    .componentDidUpdate(x =>
      arrived(x.prevProps.event, x.currentProps.event).fold(Callback.empty)(x.backend.onEvent)
    )
    .build
