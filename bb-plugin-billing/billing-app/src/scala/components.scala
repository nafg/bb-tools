package billingapp

import billingcore.{
  Allocation,
  BillingEvent,
  BillingRpc,
  BlockKey,
  CivilDate,
  MappingSource,
  ProjectZoho,
  Reply,
  Standing,
  SubmissionRecord,
  ThreadAccount,
  WireJs,
  Words
}
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import org.scalajs.dom

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

private given ExecutionContext = ExecutionContext.parasitic

private val buttonClass        =
  "rounded-md border border-border px-2.5 py-1 text-xs hover:bg-accent disabled:cursor-not-allowed disabled:opacity-50"
private val primaryButtonClass =
  "rounded-md bg-primary px-3 py-1.5 text-sm font-medium text-primary-foreground hover:bg-primary/90 disabled:cursor-not-allowed disabled:opacity-50"
private val inputClass         =
  "rounded-md border border-input bg-background px-2 py-1 text-sm outline-none focus-visible:ring-2 focus-visible:ring-ring"
private val cardClass          = "space-y-3 rounded-lg border border-border bg-card p-4 text-sm"

private def banner(tone: String, content: TagMod*): VdomElement =
  val colors = tone match
    case "error" => "border-destructive/40 bg-destructive/10 text-destructive"
    case "warn"  => "border-amber-500/40 bg-amber-500/10"
    case _       => "border-border bg-accent/40"
  <.div((^.className := s"rounded-md border px-3 py-2 text-xs $colors") +: content*)

/** How the page reads each reply of the contract: the JS view behind a `Reply` marker. */
sealed trait ReplyView[R]:
  type Js

object ReplyView:
  type Aux[R, J] = ReplyView[R] { type Js = J }
  private def as[R, J]: Aux[R, J] = new ReplyView[R] { type Js = J }

  given Aux[Reply.Ok, js.Any]                             = as
  given Aux[Reply.Overview, OverviewResult]               = as
  given Aux[Reply.Ledger, LedgerJs]                       = as
  given Aux[Reply.Plan, PlanJs]                           = as
  given Aux[Reply.Submission, js.Any]                     = as
  given Aux[Reply.Deleted, js.Any]                        = as
  given Aux[Reply.Submissions, SubmissionsResult]         = as
  given Aux[Reply.Catalog, CatalogResult]                 = as
  given Aux[Reply.Mapping, MappingResult]                 = as
  given Aux[Reply.ProjectMappings, ProjectMappingsResult] = as
  given Aux[Reply.Report, ReportResult]                   = as

/** The event the backend published, when the latest realtime message is one the contract knows. */
private def billingEvent(e: RealtimeEventJs | Null): Option[BillingEvent] =
  nullable(e).flatMap(x => BillingEvent.decode(WireJs.fromJs(x.payload)))

/** A new realtime message arrived between two renders: the event it carries, if the contract knows it. */
private def arrived(prev: RealtimeEventJs | Null, current: RealtimeEventJs | Null): Option[BillingEvent] =
  if nullable(prev).map(_.seq) == nullable(current).map(_.seq) then None else billingEvent(current)

/** Sends a request of the contract and reads its reply as the view declared for it. */
private def rpc[R](call: RpcCall, request: BillingRpc[R])(using view: ReplyView[R]): Future[view.Js] =
  call(request.method, WireJs.toJs(request.encode)).toFuture.map(_.asInstanceOf[view.Js])

/** A Zoho trace in words. */
private def traceText(t: ZohoTraceJs, timeZone: String): String =
  t.kind match
    case "timerRan" =>
      val end = if t.end - t.start < 24 * 3600_000 then clock(t.end, timeZone) else stamp(t.end, timeZone)
      s"Zoho timer ${clock(t.start, timeZone)}–$end (${hoursMinutes(t.end - t.start)}): ${t.detail}"
    case _          => s"Zoho timer started ${clock(t.start, timeZone)}, not stopped in this thread: ${t.detail}"

/** A conversation excerpt: timed turns with who spoke, the agent's actions in italics. */
private def conversationList(messages: js.Array[UserMessageJs], timeZone: String): VdomElement =
  <.ul(
    ^.className := "ml-3 list-disc space-y-0.5 py-1",
    messages.map(m =>
      <.li(
        ^.key       := m.seq,
        ^.className := (if m.speaker == "Work" then "italic" else ""),
        <.span(^.className := "mr-1 tabular-nums", clock(m.at, timeZone)),
        <.span(
          ^.className      := "mr-1 font-medium",
          m.speaker match
            case "User"  => "You:"
            case "Agent" => "Agent:"
            case _       => ""
        ),
        m.text
      )
    ).toVdomArray
  )

/** One overlap block with its decision controls. */
object BlockCard:
  final case class Props(
    block: Block,
    timeZone: String,
    titles: Map[String, String],
    busy: Boolean,
    key: BlockKey,
    decide: (BlockKey, Allocation) => Callback,
    undecide: BlockKey => Callback,
    openThread: String => Callback)

  final case class State(splitting: Boolean, portions: Map[String, String], error: Option[String])

  final class Backend($ : BackendScope[Props, State]):
    private def describe(p: Props, d: Decision): String =
      d.kind match
        case "wholeTo"  => s"all to ${p.titles.getOrElse(d.threadId.getOrElse(""), d.threadId.getOrElse(""))}"
        case "portions" =>
          d.ms.fold("split")(ms =>
            ms.toList.map((t, v) => s"${p.titles.getOrElse(t, t)} ${hoursMinutes(v)}").mkString("split: ", ", ", "")
          )
        case _          => "billed to nobody"

    private def submitPortions(p: Props, s: State): Callback =
      val parsed = p.block.threadIds.toList.map { t =>
        t -> s.portions.get(t).map(_.trim).filter(_.nonEmpty).fold(Option(0.0))(_.toDoubleOption)
      }
      if parsed.exists(_._2.isEmpty) then $.modState(_.copy(error = Some("minutes must be numbers")))
      else
        val ms    = parsed.collect { case (t, Some(v)) if v > 0 => t -> (v * 60_000).toLong }.toMap
        val total = parsed.map(_._2.get).sum * 60_000
        if total > p.block.durationMs + 1 then
          $.modState(_.copy(error = Some("the minutes add up to more than the overlap holds")))
        else
          p.decide(p.key, Allocation.Portions(ms)) >> $.modState(_.copy(
            splitting = false,
            error = None
          ))

    def render(p: Props, s: State): VdomElement =
      val b      = p.block
      val header =
      <.div(
        ^.className := "flex flex-wrap items-center gap-2",
        <.span(^.className := "font-medium", s"${clock(b.start, p.timeZone)}–${clock(b.end, p.timeZone)}"),
        <.span(^.className := "text-muted-foreground", hoursMinutes(b.durationMs)),
        nullable(b.decision).map(d =>
          <.span(^.className := "rounded bg-green-500/20 px-1.5 text-xs", describe(p, d))
        ),
        <.span(^.className := "rounded bg-destructive/20 px-1.5 text-xs", "who gets it?").when(
          b.decision == null && b.billableThreadIds.nonEmpty
        ),
        <.span(^.className := "text-xs text-muted-foreground", "no client thread involved").when(
          b.billableThreadIds.isEmpty
        )
      )
      val body   = TagMod(
        // Each thread of the stretch: its name and project, a way in, and what went on in it during these minutes.
        <.div(
          ^.className := "space-y-1 text-xs",
          b.threads.map { bt =>
            <.div(
              ^.key := bt.threadId,
              <.span(^.className := "font-medium", bt.title),
              <.span(^.className := "ml-1 text-muted-foreground", s"(${bt.projectName})").when(bt.projectName.nonEmpty),
              <.span(^.className := "ml-1 text-muted-foreground", "not client work").when(!bt.billable),
              <.button(
                ^.`type`         := "button",
                ^.className      := "ml-2 underline text-muted-foreground",
                ^.onClick --> p.openThread(bt.threadId),
                "open"
              ),
              <.details(
                ^.className      := "text-muted-foreground",
                <.summary(
                  ^.className := "cursor-pointer select-none",
                  if bt.messages.isEmpty then "nothing said or done in this thread during the stretch"
                  else
                    s"${bt.messages.count(_.speaker == "User")} from you, ${bt.messages.count(
                        _.speaker == "Agent"
                      )} replies, ${bt.messages.count(_.speaker == "Work")} actions during the stretch"
                ),
                conversationList(bt.messages, p.timeZone)
              )
            )
          }.toVdomArray,
          <.div(
            ^.className := "text-muted-foreground",
            b.threadIds.map(t =>
              p.titles.getOrElse(t, t) + (if b.billableThreadIds.contains(t) then "" else " (not client work)")
            ).mkString(" + ")
          ).when(b.threads.isEmpty)
        ),
        <.div(
          ^.className := "flex flex-wrap items-center gap-2",
          b.threadIds.map { t =>
            <.button(
              ^.key       := t,
              ^.`type`    := "button",
              ^.className := buttonClass,
              ^.disabled  := p.busy,
              ^.onClick --> p.decide(p.key, Allocation.WholeTo(t)),
              s"All to ${p.titles.getOrElse(t, t).take(30)}"
            )
          }.toVdomArray,
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := p.busy,
            ^.onClick --> $.modState(s => s.copy(splitting = !s.splitting)),
            "Split minutes"
          ),
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := p.busy,
            ^.onClick --> p.decide(p.key, Allocation.Nobody),
            "Nobody gets it"
          ),
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := p.busy,
            ^.onClick --> p.undecide(p.key),
            "Undo decision"
          ).when(b.decision != null)
        ).when(b.billableThreadIds.nonEmpty || b.decision != null),
        <.div(
          ^.className := "flex flex-wrap items-end gap-2",
          b.threadIds.map { t =>
            <.label(
              ^.key       := t,
              ^.className := "flex flex-col text-xs",
              <.span(p.titles.getOrElse(t, t).take(30)),
              <.input(
                ^.className   := inputClass + " w-24",
                ^.placeholder := "minutes",
                ^.value       := s.portions.getOrElse(t, ""),
                ^.onChange ==> ((e: ReactEventFromInput) => {
                  val v = e.target.value; $.modState(st => st.copy(portions = st.portions.updated(t, v)))
                })
              )
            )
          }.toVdomArray,
          <.button(
            ^.`type`    := "button",
            ^.className := buttonClass,
            ^.disabled  := p.busy,
            ^.onClick --> submitPortions(p, s),
            "Save split"
          ),
          s.error.map(m => <.span(^.className := "text-xs text-destructive", m))
        ).when(s.splitting)
      )
      // A decided stretch folds to its header; opening it brings the controls back, to change the decision.
      if b.decision != null then
        <.details(
          ^.className := "rounded-md border border-border px-3 py-2",
          <.summary(^.className := "cursor-pointer select-none", header),
          <.div(^.className := "space-y-2 pt-2", body)
        )
      else
        <.div(^.className := "space-y-2 rounded-md border border-border px-3 py-2", header, body)

  val Component = ScalaComponent
    .builder[Props]("BlockCard")
    .initialState(State(splitting = false, portions = Map.empty, error = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .build

/** A choice in a "Bill to" select: `value` is `projectId|taskId`, or one of the two special values. */
final case class BillToOption(value: String, label: String)

object BillTo:
  val Inherit    = "inherit"
  val Unbillable = "unbillable"

  def value(projectId: String, taskId: String): String = s"$projectId|$taskId"

  /** Every project and task in the catalog. */
  def allOptions(catalog: CatalogResult): Vector[BillToOption] =
    catalog.projects.toVector.flatMap { pr =>
      pr.tasks.toVector.map(t =>
        BillToOption(value(pr.projectId, t.taskId), s"${pr.customerName}: ${pr.projectName} / ${t.taskName}")
      )
    }

  /** The bb project's Zoho projects: every task of each, the chosen default task first. */
  def projectOptions(project: ProjectInfo, catalog: Option[CatalogResult]): Vector[BillToOption] =
    project.zoho.toVector.flatMap { z =>
      val tasks =
        catalog.flatMap(_.projects.find(_.projectId == z.projectId)).map(_.tasks.toVector).getOrElse(Vector.empty)
      val label = (taskName: String) => s"${z.customerName}: ${z.projectName} / $taskName"
      val rest  =
        tasks.filter(_.taskId != z.taskId).map(t => BillToOption(value(z.projectId, t.taskId), label(t.taskName)))
      BillToOption(value(z.projectId, z.taskId), label(z.taskName)) +: rest
    }

  /** True when the thread's own entry decided where it bills. */
  def own(a: ThreadAccount): Boolean = a.resolved.source.contains(MappingSource.Thread(a.threadId))

  /** The select's current value for a thread: its own choice, or the inherited placeholder when it has none. */
  def current(a: ThreadAccount): String =
    if own(a) then a.billsTo.fold(Unbillable)(z => value(z.projectId, z.taskId)) else Inherit

  /** The RPC that makes `value` the thread's own choice. */
  def save(call: RpcCall, threadId: String, value: String): Future[Unit] =
    val request = value match
      case Inherit    => Right(BillingRpc.ClearMapping(threadId))
      case Unbillable => Right(BillingRpc.MarkUnbillable(threadId))
      case other      =>
        other.split('|') match
          case Array(projectId, taskId) => Right(BillingRpc.Assign(threadId, projectId, taskId))
          case _                        => Left(s"not a project and task: $other")
    request.fold(m => Future.failed(IllegalArgumentException(m)), r => rpc(call, r).map(_ => ()))

/** A "Bill to" select for one thread; saving reloads the week. */
object BillToSelect:
  final case class Props(
    thread: ThreadAccount,
    project: Option[ProjectInfo],
    catalog: Option[CatalogResult],
    call: RpcCall,
    reload: Callback)
  final case class State(busy: Boolean, error: Option[String])

  final class Backend($ : BackendScope[Props, State]):
    private def change(e: ReactEventFromInput): Callback =
      val value = e.target.value
      $.props.flatMap { p =>
        $.modState(_.copy(busy = true, error = None)) >>
          Callback.future(
            BillTo.save(p.call, p.thread.threadId, value)
              .map(_ => $.modState(_.copy(busy = false)) >> p.reload)
              .recover { case err => $.modState(_.copy(busy = false, error = Some(errorMessage(err)))) }
          )
      }

    def render(p: Props, s: State): VdomElement =
      val t              = p.thread
      val hasParent      = t.parentThreadId.nonEmpty
      val own            = BillTo.own(t)
      val projectDefault = p.project.map(_.zoho.toVector).filter(_.length == 1).map(_.head)
      val fromProject    = p.project.map(BillTo.projectOptions(_, p.catalog)).getOrElse(Vector.empty)
      val fromProjectIds = fromProject.map(_.value).toSet
      val inheritLabel   =
        if hasParent then "Same as parent thread"
        else projectDefault.fold("Not chosen")(z => s"Project default: ${z.projectName} / ${z.taskName}")
      <.div(
        ^.className := "space-y-0.5",
        <.select(
          ^.className := inputClass + " w-full",
          ^.value     := BillTo.current(t),
          ^.disabled  := s.busy,
          ^.onChange ==> change,
          <.option(^.value := BillTo.Inherit, inheritLabel),
          <.option(^.value := BillTo.Unbillable, "Not client work"),
          <.optgroup(
            VdomAttr("label") := s"${p.project.fold("This bb project")(_.name)}'s Zoho projects",
            fromProject.map(o => <.option(^.key := o.value, ^.value := o.value, o.label)).toVdomArray
          ).when(fromProject.nonEmpty),
          p.catalog.fold(EmptyVdom: VdomNode)(c =>
            <.optgroup(
              VdomAttr("label") := "Any Zoho project",
              BillTo.allOptions(c).filterNot(o => fromProjectIds(o.value)).map(o =>
                <.option(^.key := o.value, ^.value := o.value, o.label)
              ).toVdomArray
            )
          )
        ),
        <.div(
          ^.className := "text-xs text-muted-foreground",
          t.resolved.source match
            case Some(MappingSource.Thread(_)) => s"Follows its parent: ${Words.billsTo(t.resolved)}"
            case Some(MappingSource.Project)   => s"Follows the bb project's default: ${Words.billsTo(t.resolved)}"
            case None if hasParent             => "Nothing above it has chosen, so it is not client work."
            case None                          => "Not client work until chosen."
        ).when(!own),
        s.error.map(m => <.div(^.className := "text-xs text-destructive", m))
      )

  val Component = ScalaComponent
    .builder[Props]("BillToSelect")
    .initialState(State(busy = false, error = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .build

/** The thread tree of the To-bill view: one row per thread, nested under its parent, grouped by bb project. */
/** The thread's standing as a chip: the same word the CLI and the agent use for it. */
private def standingChip(standing: Standing): VdomElement =
  val cls = standing match
    case Standing.NotClientWork | Standing.NothingRecorded | Standing.Settled => "bg-muted text-muted-foreground"
    case Standing.ArchivedToSettle                                            => "bg-amber-500/20"
    case Standing.OpenUnbilled | Standing.OpenPartlyBilled                    => "bg-blue-500/15"
  <.span(^.className := s"ml-1 rounded px-1 text-xs $cls", standing.label)

object ThreadTree:
  final case class Props(
    o: OverviewResult,
    catalog: Option[CatalogResult],
    call: RpcCall,
    reload: Callback,
    openThread: String => Callback,
    goTo: String => Callback,
    /** Opens the ledger of a thread. */
    openPlan: String => Callback,
    /** The thread whose ledger is open, if any. */
    openId: Option[String],
    showOthers: Boolean,
    toggleOthers: Callback)

  def render(p: Props): VdomElement =
    val all                                      = accounts(p.o.threads)
    val ids                                      = all.map(_.threadId).toSet
    val children                                 = all.groupBy(t => t.parentThreadId.filter(ids).getOrElse(""))
    val projectsById                             = p.o.projects.map(pr => pr.projectId -> pr).toMap
    def subtreeMs(t: ThreadAccount): Long        =
      t.totals.unbilledMs + children.getOrElse(t.threadId, Vector.empty).map(subtreeMs).sum
    val anyClient                                = all.exists(_.billsTo.isDefined)
    val needDecision                             = all.count(_.standing.needsDecision)
    def needsDecision(t: ThreadAccount): Boolean =
      t.standing.needsDecision || children.getOrElse(t.threadId, Vector.empty).exists(needsDecision)
    val showOthers                               = p.showOthers || !anyClient
    def keep(t: ThreadAccount): Boolean          =
      showOthers || t.billsTo.isDefined || children.getOrElse(t.threadId, Vector.empty).exists(keep)

    def row(t: ThreadAccount, depth: Int): VdomNode =
      <.tr(
        ^.key       := t.threadId,
        ^.className := (if t.billsTo.isDefined then "align-top" else "align-top text-muted-foreground"),
        <.td(
          ^.className := "py-1 pr-2",
          ^.style     := js.Dynamic.literal("paddingLeft" -> s"${depth * 1.25}rem"),
          <.span(^.className := "text-muted-foreground", "└ ").when(depth > 0),
          <.button(
            ^.`type`    := "button",
            ^.className := "text-left hover:underline",
            ^.onClick --> p.openThread(t.threadId),
            t.title
          ),
          standingChip(t.standing),
          <.div(
            ^.className := "text-xs text-muted-foreground",
            Words.when(t, ms => shortDay(ms.toDouble, p.o.timeZone))
          )
        ),
        <.td(
          ^.className := "py-1 pr-2",
          BillToSelect.Component.withKey(t.threadId)(BillToSelect.Props(
            t,
            projectsById.get(t.projectId),
            p.catalog,
            p.call,
            p.reload
          ))
        ),
        <.td(
          ^.className := "py-1 text-xs",
          <.div(^.className := "text-muted-foreground", Words.totals(t.totals, t.submissions)),
          <.button(
            ^.`type`    := "button",
            ^.className := (if p.openId.contains(t.threadId) then buttonClass
                            else if t.standing.needsDecision then primaryButtonClass
                            else buttonClass),
            ^.onClick --> p.openPlan(t.threadId),
            if p.openId.contains(t.threadId) then "Reviewing above ↑" else "Review and bill…"
          ).when(t.billsTo.isDefined && t.totals.unbilledMs > 0)
        )
      )

    def tree(t: ThreadAccount, depth: Int): Vector[VdomNode] =
      if !keep(t) then Vector.empty
      else
        row(t, depth) +: children.getOrElse(t.threadId, Vector.empty).sortBy(c =>
          (!needsDecision(c), -subtreeMs(c))
        ).flatMap(c =>
          tree(c, depth + 1)
        )

    val projects = all
      .filter(t => t.parentThreadId.filter(ids).isEmpty)
      .groupBy(t => (t.projectId, t.projectName))
      .toVector
      .map { case ((projectId, name), roots) =>
        (projectId, name, roots.sortBy(r => (!needsDecision(r), -subtreeMs(r))), roots.flatMap(r => tree(r, 0)))
      }
      .filter(_._4.nonEmpty)
      .sortBy { case (_, _, roots, _) => (!roots.exists(needsDecision), -roots.map(subtreeMs).sum) }

    <.div(
      ^.className := cardClass,
      <.div(^.className := "text-sm font-medium", "Threads"),
      <.div(
        ^.className     := "text-xs text-muted-foreground",
        s"Threads with recorded attention, grouped by bb project and nested under their parent: where each bills, what is unbilled, and the way into its ledger." +
          (if needDecision > 0 then
             s" $needDecision archived ${if needDecision == 1 then "thread needs" else "threads need"} a decision: archiving ends a task, so bill it or settle it without billing."
           else "") +
          " A thread under another follows it unless it chooses its own; a thread that chooses nothing follows its bb project's default."
      ),
      projects.map { case (projectId, name, roots, rows) =>
        val info = projectsById.get(projectId)
        <.div(
          ^.key       := projectId,
          ^.className := "space-y-1",
          <.div(
            ^.className := "flex flex-wrap items-center justify-between gap-2 border-b border-border pt-2 pb-1",
            <.div(
              <.span(
                ^.className := "text-sm font-semibold",
                if name.isEmpty then "Threads whose bb project is unknown" else name
              ),
              <.span(
                ^.className := "ml-2 text-xs text-muted-foreground",
                s"${hoursMinutes(roots.map(subtreeMs).sum.toDouble)} unbilled"
              )
            ),
            <.div(
              ^.className := "text-xs text-muted-foreground",
              info.map(_.zoho.toVector).filter(_.nonEmpty) match
                case Some(Vector(z)) =>
                  s"Zoho: ${z.customerName}: ${z.projectName} / ${z.taskName} (default for its threads)"
                case Some(zs)        =>
                  s"Zoho: ${zs.map(z => s"${z.customerName}: ${z.projectName}").mkString(", ")} (threads choose)"
                case None            => "No Zoho project yet"
              ,
              <.button(
                ^.`type`    := "button",
                ^.className := "ml-2 underline",
                ^.onClick --> p.goTo("projects"),
                "change"
              )
            )
          ),
          <.table(
            ^.className := "w-full text-xs",
            <.thead(
              <.tr(
                ^.className := "text-left text-muted-foreground",
                <.th(^.className := "py-1 pr-2", "Thread"),
                <.th(^.className := "py-1 pr-2 w-2/5", "Bill to"),
                <.th(^.className := "py-1", "Unbilled and sent")
              )
            ),
            <.tbody(rows.toVdomArray)
          )
        ): VdomNode
      }.toVdomArray,
      <.button(
        ^.`type`    := "button",
        ^.className := "text-xs text-muted-foreground hover:underline",
        ^.onClick --> p.toggleOthers,
        if showOthers then "Hide threads that are not client work" else "Show threads that are not client work"
      ).when(anyClient)
    )

/** The To-bill view: what needs a decision, then every thread with recorded time and where it bills. */
object ToBillView:
  final case class Props(
    call: RpcCall,
    openThread: String => Callback,
    goTo: String => Callback,
    event: RealtimeEventJs | Null)
  final case class State(
    o: Option[OverviewResult],
    catalog: Option[CatalogResult],
    loading: Boolean,
    error: Option[String],
    showOthers: Boolean,
    open: Option[String],
    sinceDays: Int,
    /** Bumped on every reload so the recent-activity card reloads too. */
    loads: Int)

  final class Backend($ : BackendScope[Props, State]):
    def load: Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        $.modState(st => st.copy(loading = true, error = None, loads = st.loads + 1)) >>
          Callback.future(
            rpc(p.call, BillingRpc.Overview((js.Date.now() - s.sinceDays.toDouble * 24 * 3600_000).toLong))
              .map(o => $.modState(_.copy(o = Some(o), loading = false)) >> loadCatalog(o))
              .recover { case e => $.modState(_.copy(loading = false, error = Some(errorMessage(e)))) }
          )
      }

    private def loadCatalog(o: OverviewResult): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        if !o.zoho.configured || s.catalog.nonEmpty then Callback.empty
        else
          Callback.future(rpc(p.call, BillingRpc.Catalog(refresh = false)).map(c =>
            $.modState(_.copy(catalog = Some(c)))
          ).recover { case _ => Callback.empty })
      }

    def setSince(days: Int): Callback = $.modState(_.copy(sinceDays = days)) >> load

    private def setup(o: OverviewResult, p: Props): VdomElement =
      val mapped                                                     = o.projects.count(_.zoho.nonEmpty)
      def step(n: Int, title: String, status: VdomNode, ok: Boolean) =
        <.div(
          ^.className := "flex items-start gap-2",
          <.span(
            ^.className := (if ok then "rounded-full bg-green-500/20 px-2 text-xs"
                            else "rounded-full bg-amber-500/20 px-2 text-xs"),
            n.toString
          ),
          <.div(
            <.div(^.className := "text-sm font-medium", title),
            <.div(^.className := "text-xs text-muted-foreground", status)
          )
        )
      <.div(
        ^.className := cardClass,
        <.div(
          ^.className := "text-xs text-muted-foreground",
          "bb records when each thread has your attention: speaking, typing, or reading it. Nothing is logged by hand. " +
            "A stretch of recording is billed once a submission covers it, and not before. You make a submission by choosing stretches, normally everything unbilled up to a moment, and sending them: one Zoho entry per calendar day, with a summary of what you did. Archiving a thread is the natural time; an open thread can be billed at any time. Any submission can be undone, in any order."
        ),
        step(
          1,
          "Connect Zoho Books",
          if o.zoho.configured then <.span(s"Connected. Days run midnight to midnight in ${o.timeZone}.")
          else
            <.span(
              s"Not connected: ${o.zoho.missing.mkString(", ")} missing. ",
              <.a(^.href := "/settings/plugins", ^.className := "underline", "Open plugin settings"),
              "."
            )
          ,
          o.zoho.configured
        ),
        step(
          2,
          "Say which Zoho projects each bb project bills to",
          <.span(
            s"$mapped of ${o.projects.length} bb projects with recent time have Zoho projects. ",
            <.button(
              ^.`type`    := "button",
              ^.className := "underline",
              ^.onClick --> p.goTo("projects"),
              "Set them up on the Projects tab"
            ),
            "."
          ),
          mapped > 0
        ),
        step(
          3,
          "Summaries",
          if o.summariesAvailable then
            <.span(
              "\"Write summaries\" in a submission has a model describe each stretch of work in a hidden thread; the entry notes join those lines. The summaryModel setting picks the model (bb billing models lists them); empty uses bb's default."
            )
          else
            <.span(
              "bb offers no model right now: each stretch shows your first message in it, and the entry notes join those lines. Edit them before sending."
            )
          ,
          o.summariesAvailable
        )
      )

    def render(p: Props, s: State): VdomElement =
      <.div(
        ^.className := "space-y-4",
        s.error.map(m => banner("error", m)),
        s.o.map { o =>
          <.div(
            ^.className := "space-y-4",
            banner(
              "error",
              <.div(
                ^.className := "font-medium",
                "The activity recorder is not available, so there is no recorded time to bill."
              ),
              <.div(s"bb said: ${nullable(o.recorder.error).getOrElse("unknown error")}")
            ).when(!o.recorder.available),
            o.warnings.map(m => banner("warn", m)).toVdomArray,
            setup(o, p),
            s.open.map { threadId =>
              SubmissionPanel.Component.withKey(threadId)(SubmissionPanel.Props(
                threadId,
                o.timeZone,
                p.call,
                onDone = load,
                close = $.modState(_.copy(open = None)),
                openThread = p.openThread,
                event = p.event
              ))
            },
            <.div(
              ^.className := "flex flex-wrap items-center gap-2 text-xs text-muted-foreground",
              <.span("Showing threads with recorded time in the last"),
              Vector(7, 28, 90).map(d =>
                <.button(
                  ^.key       := d,
                  ^.`type`    := "button",
                  ^.className := (if s.sinceDays == d then primaryButtonClass else buttonClass),
                  ^.onClick --> setSince(d),
                  s"$d days"
                )
              ).toVdomArray,
              <.button(
                ^.`type`    := "button",
                ^.className := buttonClass,
                ^.disabled  := s.loading,
                ^.onClick --> load,
                "Reload"
              )
            ),
            ThreadTree.render(ThreadTree.Props(
              o,
              s.catalog,
              p.call,
              load,
              p.openThread,
              p.goTo,
              id => $.modState(_.copy(open = Some(id))),
              s.open,
              s.showOthers,
              $.modState(st => st.copy(showOthers = !st.showOthers))
            ))
          )
        },
        <.div(^.className := "text-sm text-muted-foreground", "Loading…").when(s.loading && s.o.isEmpty)
      )

  val Component = ScalaComponent
    .builder[Props]("ToBillView")
    .initialState(State(
      o = None,
      catalog = None,
      loading = false,
      error = None,
      showOthers = false,
      open = None,
      sinceDays = 28,
      loads = 0
    ))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.load)
    .componentDidUpdate(x =>
      arrived(x.prevProps.event, x.currentProps.event) match
        case Some(BillingEvent.Changed) => x.backend.load
        case _                          => Callback.empty
    )
    .build

/** Past submissions, with undo. */
object SubmissionsView:
  final case class Props(call: RpcCall, openThread: String => Callback, event: RealtimeEventJs | Null)
  final case class State(subs: Vector[SubmissionRecord], timeZone: String, busy: Boolean, error: Option[String])

  final class Backend($ : BackendScope[Props, State]):
    def load: Callback =
      $.props.flatMap(p =>
        Callback.future(rpc(p.call, BillingRpc.Submissions).map(r =>
          $.modState(_.copy(subs = records(r.submissions), timeZone = r.timeZone))
        ).recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) })
      )

    def undo(id: String): Callback =
      $.props.flatMap { p =>
        if !dom.window.confirm(
            "Undo this submission? Its Zoho entries are deleted and the threads become unbilled again from where they were."
          )
        then Callback.empty
        else
          $.modState(_.copy(busy = true, error = None)) >>
            Callback.future(rpc(p.call, BillingRpc.Undo(id)).map(_ =>
              $.modState(_.copy(busy = false)) >> load
            ).recover { case e => $.modState(_.copy(busy = false, error = Some(errorMessage(e)))) })
      }

    def render(p: Props, s: State): VdomElement =
      <.div(
        ^.className := cardClass,
        <.div(^.className := "text-sm font-medium", "Submissions"),
        <.div(
          ^.className     := "text-xs text-muted-foreground",
          "Every send to Zoho and every \"mark settled\", newest first. Undo deletes the Zoho entries it created and makes the time unbilled again."
        ),
        s.error.map(m => banner("error", m)),
        <.div(^.className := "text-xs text-muted-foreground", "Nothing sent or settled yet.").when(s.subs.isEmpty),
        s.subs.map { sub =>
          <.div(
            ^.key       := sub.id,
            ^.className := "flex flex-wrap items-center justify-between gap-2 border-b border-border py-1 text-xs",
            <.div(
              <.button(
                ^.`type`    := "button",
                ^.className := "font-medium hover:underline",
                ^.onClick --> p.openThread(sub.rootThreadId),
                sub.rootTitle
              ),
              <.span(
                ^.className := "ml-2 text-muted-foreground",
                Words.submission(sub, ms => stamp(ms.toDouble, s.timeZone))
              )
            ),
            <.button(
              ^.`type`    := "button",
              ^.className := buttonClass,
              ^.disabled  := s.busy,
              ^.onClick --> undo(sub.id),
              if sub.billed then "Undo this send" else "Undo this settle"
            )
          )
        }.toVdomArray
      )

  val Component = ScalaComponent
    .builder[Props]("SubmissionsView")
    .initialState(State(subs = Vector.empty, timeZone = "UTC", busy = false, error = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.load)
    .componentDidUpdate(x =>
      arrived(x.prevProps.event, x.currentProps.event) match
        case Some(BillingEvent.Changed) => x.backend.load
        case _                          => Callback.empty
    )
    .build

/** The Projects tab: which Zoho projects each bb project's threads may bill to, and the default task of each. */
object ProjectsView:
  final case class Props(call: RpcCall)
  final case class State(
    projects: Vector[ProjectInfo],
    catalog: Option[CatalogResult],
    adding: Map[String, String],
    busy: Set[String],
    error: Option[String])

  final class Backend($ : BackendScope[Props, State]):
    def load: Callback =
      $.props.flatMap { p =>
        Callback.future(
          (for
            mappings <- rpc(p.call, BillingRpc.ProjectMappings)
            catalog  <- rpc(p.call, BillingRpc.Catalog(refresh = false)).map(Some(_)).recover { case _ => None }
          yield $.modState(_.copy(projects = mappings.projects.toVector, catalog = catalog, error = None)))
            .recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
        )
      }

    private def save(project: ProjectInfo, zoho: Vector[(String, String)]): Callback =
      $.props.flatMap { p =>
        val request = BillingRpc.SetProjectMapping(project.projectId, zoho.map(ProjectZoho.apply))
        $.modState(st => st.copy(busy = st.busy + project.projectId, error = None)) >>
          Callback.future(
            rpc(p.call, request)
              .map(_ => $.modState(st => st.copy(busy = st.busy - project.projectId)) >> load)
              .recover { case e =>
                $.modState(st => st.copy(busy = st.busy - project.projectId, error = Some(errorMessage(e))))
              }
          )
      }

    private def current(project: ProjectInfo): Vector[(String, String)] =
      project.zoho.toVector.map(z => z.projectId -> z.taskId)

    def render(@scala.annotation.unused p: Props, s: State): VdomElement =
      <.div(
        ^.className := "space-y-4",
        <.div(
          ^.className := cardClass,
          <.div(^.className := "text-sm font-medium", "Zoho projects for each bb project"),
          <.div(
            ^.className     := "text-xs text-muted-foreground",
            "A thread bills to one of its bb project's Zoho projects (or to any other, if it insists). Pick the task its threads bill to by default. A bb project with exactly one Zoho project bills every thread there unless the thread says otherwise."
          ),
          s.error.map(m => banner("error", m)),
          <.div(
            ^.className := "text-xs text-muted-foreground",
            "Connect Zoho in the plugin settings to list its projects."
          ).when(s.catalog.isEmpty)
        ),
        s.projects.map { project =>
          val busy    = s.busy(project.projectId)
          val chosen  = current(project)
          val addable = s.catalog.map(_.projects.toVector.filterNot(c => chosen.exists(_._1 == c.projectId))).getOrElse(
            Vector.empty
          )
          <.div(
            ^.key       := project.projectId,
            ^.className := cardClass,
            <.div(^.className := "text-sm font-semibold", project.name),
            project.zoho.map { z =>
              val tasks = s.catalog.flatMap(_.projects.find(_.projectId == z.projectId)).map(
                _.tasks.toVector
              ).getOrElse(Vector.empty)
              <.div(
                ^.key       := z.projectId,
                ^.className := "flex flex-wrap items-center gap-2 text-xs",
                <.span(s"${z.customerName}: ${z.projectName}"),
                <.span(^.className := "text-muted-foreground", "default task"),
                <.select(
                  ^.className      := inputClass,
                  ^.value          := z.taskId,
                  ^.disabled       := (busy || tasks.isEmpty),
                  ^.onChange ==> ((e: ReactEventFromInput) => {
                    val v = e.target.value;
                    save(project, chosen.map((pr, t) => if pr == z.projectId then (pr, v) else (pr, t)))
                  }),
                  (if tasks.isEmpty then Vector(z.taskId -> z.taskName)
                   else tasks.map(t => t.taskId -> t.taskName)).map((id, name) =>
                    <.option(^.key := id, ^.value := id, name)
                  ).toVdomArray
                ),
                <.button(
                  ^.`type`         := "button",
                  ^.className      := buttonClass,
                  ^.disabled       := busy,
                  ^.onClick --> save(project, chosen.filterNot(_._1 == z.projectId)),
                  "Remove"
                )
              )
            }.toVdomArray,
            <.div(
              ^.className := "flex flex-wrap items-center gap-2 text-xs",
              <.span(^.className := "text-muted-foreground", "Add a Zoho project"),
              <.select(
                ^.className      := inputClass,
                ^.value          := "",
                ^.disabled       := busy,
                ^.onChange ==> ((e: ReactEventFromInput) => {
                  val v = e.target.value
                  addable.find(_.projectId == v).flatMap(c =>
                    c.tasks.headOption.map(t => (c.projectId, t.taskId))
                  ) match
                    case Some(pair) => save(project, chosen :+ pair)
                    case None       => Callback.empty
                }),
                <.option(^.value := "", "choose…"),
                addable.map(c =>
                  <.option(^.key := c.projectId, ^.value := c.projectId, s"${c.customerName}: ${c.projectName}")
                ).toVdomArray
              )
            ).when(s.catalog.nonEmpty)
          )
        }.toVdomArray
      )

  val Component = ScalaComponent
    .builder[Props]("ProjectsView")
    .initialState(State(projects = Vector.empty, catalog = None, adding = Map.empty, busy = Set.empty, error = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.load)
    .build

/** Client report: what was sent to Zoho for a customer over a date range. */
object ReportView:
  final case class Props(call: RpcCall, openThread: String => Callback)
  final case class State(
    catalog: Option[CatalogResult],
    customerId: String,
    from: String,
    to: String,
    report: Option[ReportResult],
    busy: Boolean,
    error: Option[String])

  final class Backend($ : BackendScope[Props, State]):
    def loadCatalog: Callback =
      $.props.flatMap { p =>
        Callback.future(
          rpc(p.call, BillingRpc.Catalog(refresh = false))
            .map(c => $.modState(s => s.copy(catalog = Some(c))))
            .recover { case e =>
              $.modState(_.copy(error =
                Some(s"Connect Zoho in the plugin settings to list customers. (${errorMessage(e)})")
              ))
            }
        )
      }

    def generate(customerId: String): Callback =
      $.props.zip($.state).flatMap { (p, s) =>
        (CivilDate.parseOption(s.from), CivilDate.parseOption(s.to)) match
          case (Some(from), Some(to)) =>
            $.modState(_.copy(busy = true, error = None, customerId = customerId)) >>
              Callback.future(
                rpc(p.call, BillingRpc.Report(customerId, from, to))
                  .map(r => $.modState(_.copy(busy = false, report = Some(r))))
                  .recover { case e => $.modState(_.copy(busy = false, error = Some(errorMessage(e)))) }
              )
          case _                      => $.modState(_.copy(error = Some("from and to must be yyyy-MM-dd")))
      }

    private def set(f: (State, String) => State)(e: ReactEventFromInput): Callback =
      val v = e.target.value
      $.modState(s => f(s, v))

    def render(p: Props, s: State): VdomElement =
      val customers = s.catalog.fold(Vector.empty[(String, String)])(_.projects.map(x =>
        x.customerId -> x.customerName
      ).toVector.distinct.sortBy(_._2))
      <.div(
        ^.className := cardClass,
        <.div(^.className := "text-sm font-medium", "What was sent to Zoho for a customer"),
        <.div(
          ^.className     := "text-xs text-muted-foreground",
          "Per thread, the time this plugin sent to Zoho Books in the date range, with the Markdown to paste into an email or invoice note. Only sent time counts; recorded time that was never sent is not here."
        ),
        <.div(
          ^.className     := "flex flex-wrap items-end gap-2",
          <.label(
            ^.className := "flex flex-col text-xs",
            <.span("From"),
            <.input(
              ^.`type`    := "date",
              ^.className := inputClass,
              ^.value     := s.from,
              ^.onChange ==> set((st, v) => st.copy(from = v))
            )
          ),
          <.label(
            ^.className := "flex flex-col text-xs",
            <.span("To"),
            <.input(
              ^.`type`    := "date",
              ^.className := inputClass,
              ^.value     := s.to,
              ^.onChange ==> set((st, v) => st.copy(to = v))
            )
          )
        ),
        <.div(
          ^.className     := "flex flex-wrap gap-2",
          customers.map { (id, name) =>
            <.button(
              ^.key       := id,
              ^.`type`    := "button",
              ^.className := (if s.customerId == id then primaryButtonClass else buttonClass),
              ^.disabled  := (s.busy || s.from.isEmpty || s.to.isEmpty),
              ^.onClick --> generate(id),
              name
            )
          }.toVdomArray
        ),
        <.div(^.className := "text-xs text-muted-foreground", "Pick the dates, then click a customer.").when(
          customers.nonEmpty && s.report.isEmpty
        ),
        s.error.map(m => banner("error", m)),
        s.report.map { r =>
          <.div(
            ^.className := "space-y-2",
            <.div(
              ^.className          := "font-medium",
              s"${r.customerName}: ${r.from} to ${r.to}, ${hoursMinutes(r.totalMinutes * 60_000)} sent"
            ),
            <.div(
              ^.className := "text-xs text-muted-foreground",
              "Nothing was sent to Zoho for this customer in this range."
            ).when(r.lines.isEmpty),
            <.table(
              ^.className := "w-full text-xs",
              <.tbody(
                r.lines.map { l =>
                  <.tr(
                    ^.key := l.threadId,
                    <.td(
                      ^.className    := "py-0.5 pr-2",
                      <.button(
                        ^.`type`    := "button",
                        ^.className := "text-left hover:underline",
                        ^.onClick --> p.openThread(l.threadId),
                        l.title
                      )
                    ),
                    <.td(^.className := "py-0.5 pr-2 text-muted-foreground", s"${l.project} / ${l.task}"),
                    <.td(^.className := "py-0.5 pr-2 text-muted-foreground", l.days.mkString(", ")),
                    <.td(^.className := "py-0.5 text-right font-medium", hoursMinutes(l.minutes * 60_000))
                  )
                }.toVdomArray
              )
            ).when(r.lines.nonEmpty),
            <.textarea(^.className := inputClass + " h-40 w-full font-mono", ^.readOnly := true, ^.value := r.markdown)
          )
        }
      )

  /** The current month so far, as the default range. */
  private def defaultRange: (String, String) =
    val now   = new js.Date()
    val today = CivilDate(now.getFullYear().toInt, now.getMonth().toInt + 1, now.getDate().toInt)
    (today.copy(day = 1).iso, today.iso)

  val Component = ScalaComponent
    .builder[Props]("ReportView")
    .initialState {
      val (from, to) = defaultRange
      State(catalog = None, customerId = "", from = from, to = to, report = None, busy = false, error = None)
    }
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.loadCatalog)
    .build

/** The plugin page: what to bill, the bb-project-to-Zoho setup, past submissions, and the client report. */
object ReviewPage:
  val Component = ScalaFnComponent[ReviewPageProps] { props =>
    val openThread: String => Callback       = id => Callback(props.openThread(id))
    val goTo: String => Callback             = sub => Callback(props.navigate(sub))
    val sub                                  = props.subPath.stripPrefix("/")
    val tab                                  =
      if sub.startsWith("report") then "report"
      else if sub.startsWith("projects") then "projects"
      else if sub.startsWith("submissions") then "submissions"
      else "bill"
    def tabButton(id: String, label: String) =
      <.button(
        ^.`type`    := "button",
        ^.className := (if tab == id then "border-b-2 border-primary px-3 py-1.5 text-sm font-medium"
                        else "px-3 py-1.5 text-sm text-muted-foreground hover:text-foreground"),
        ^.onClick --> goTo(if id == "bill" then "" else id),
        label
      )
    <.div(
      ^.className := "h-full overflow-auto p-4 md:p-5",
      <.div(
        ^.className := "mx-auto w-full max-w-5xl space-y-4",
        <.div(
          ^.className := "flex gap-1 border-b border-border",
          tabButton("bill", "To bill"),
          tabButton("projects", "Projects"),
          tabButton("submissions", "Submissions"),
          tabButton("report", "Client report")
        ),
        tab match
          case "report"      => ReportView.Component(ReportView.Props(props.call, openThread))
          case "projects"    => ProjectsView.Component(ProjectsView.Props(props.call))
          case "submissions" =>
            SubmissionsView.Component(SubmissionsView.Props(props.call, openThread, props.event))
          case _             => ToBillView.Component(ToBillView.Props(props.call, openThread, goTo, props.event))
      )
    )
  }

/** The thread panel: where this thread's time is billed, and controls to change it. */
object AssignPanel:
  final case class State(
    mapping: Option[MappingResult],
    catalog: Option[CatalogResult],
    projectId: String,
    taskId: String,
    busy: Boolean,
    error: Option[String])

  final class Backend($ : BackendScope[AssignPanelProps, State]):
    def load: Callback =
      $.props.flatMap { p =>
        Callback.future(
          (for
            mapping <- rpc(p.call, BillingRpc.Mapping(p.threadId))
            catalog <- rpc(p.call, BillingRpc.Catalog(refresh = false)).map(Some(_)).recover { case _ => None }
          yield $.modState { s =>
            val current = mapping.own.target.toOption
            s.copy(
              mapping = Some(mapping),
              catalog = catalog,
              projectId = current.map(_.projectId).orElse(Option(s.projectId).filter(_.nonEmpty)).getOrElse(
                catalog.flatMap(_.projects.headOption).fold("")(_.projectId)
              ),
              taskId = current.map(_.taskId).getOrElse(s.taskId),
              error = None
            )
          }).recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
        )
      }

    private def act(request: BillingRpc[Reply.Ok]): Callback =
      $.props.flatMap { p =>
        $.modState(_.copy(busy = true, error = None)) >>
          Callback.future(
            rpc(p.call, request)
              .map(_ => $.modState(_.copy(busy = false)) >> load)
              .recover { case e => $.modState(_.copy(busy = false, error = Some(errorMessage(e)))) }
          )
      }

    def refreshCatalog: Callback =
      $.props.flatMap { p =>
        $.modState(_.copy(busy = true, error = None)) >>
          Callback.future(
            rpc(p.call, BillingRpc.Catalog(refresh = true))
              .map(c => $.modState(_.copy(busy = false, catalog = Some(c))))
              .recover { case e => $.modState(_.copy(busy = false, error = Some(errorMessage(e)))) }
          )
      }

    def render(p: AssignPanelProps, s: State): VdomElement =
      val project = s.catalog.flatMap(_.projects.find(_.projectId == s.projectId))
      val tasks   = project.fold(Vector.empty[CatalogTask])(_.tasks.toVector)
      val taskId  = if tasks.exists(_.taskId == s.taskId) then s.taskId else tasks.headOption.fold("")(_.taskId)
      <.div(
        ^.className := "space-y-4 text-sm",
        s.error.map(m => banner("error", m)),
        s.mapping.map { m =>
          val src = nullable(m.resolved.source).filter(_.kind == "thread").flatMap(_.threadId.toOption)
          <.div(
            ^.className := "space-y-1",
            <.div(^.className := "font-medium", m.title),
            <.div(
              ^.className     := "text-xs",
              nullable(m.resolved.target) match
                case Some(t) =>
                  s"Billed to ${t.customerName}: ${t.projectName} / ${t.taskName}" +
                    (if nullable(m.resolved.source).exists(_.kind == "project") then " (the bb project's default)"
                     else src.filter(_ != m.threadId).fold("")(id => s" (follows thread $id)"))
                case None    =>
                  src match
                    case Some(id) if id == m.threadId => "Marked not client work."
                    case Some(id)                     => s"Not client work: thread $id is marked so."
                    case None                         =>
                      "Not client work: nothing on this thread, above it, or in its bb project says otherwise."
            ),
            <.div(
              ^.className     := "text-xs text-muted-foreground",
              m.own.kind match
                case "assigned"   => "This thread chose for itself."
                case "unbillable" =>
                  "This thread is marked not client work; threads under it stop following anything above."
                case "invalid"    => s"This thread's own choice is invalid: ${m.own.problem.getOrElse("")}"
                case _            => "This thread has not chosen; it follows its parent, else its bb project's default."
            )
          )
        },
        <.div(
          ^.className := "space-y-2",
          <.div(^.className := "text-xs font-medium text-muted-foreground", "Bill this thread to"),
          s.catalog match
            case None    => <.div(
                ^.className := "text-xs text-muted-foreground",
                "Zoho projects are unavailable; connect Zoho in the Billing plugin settings to choose a project."
              )
            case Some(c) =>
              React.Fragment(
                <.select(
                  ^.className := inputClass + " w-full",
                  ^.value     := s.projectId,
                  ^.onChange ==> ((e: ReactEventFromInput) => {
                    val v = e.target.value; $.modState(_.copy(projectId = v, taskId = ""))
                  }),
                  c.projects.map(pr =>
                    <.option(^.key := pr.projectId, ^.value := pr.projectId, s"${pr.customerName}: ${pr.projectName}")
                  ).toVdomArray
                ),
                <.select(
                  ^.className := inputClass + " w-full",
                  ^.value     := taskId,
                  ^.onChange ==> ((e: ReactEventFromInput) => {
                    val v = e.target.value; $.modState(_.copy(taskId = v))
                  }),
                  tasks.map(t => <.option(^.key := t.taskId, ^.value := t.taskId, t.taskName)).toVdomArray
                ),
                <.div(
                  ^.className := "flex flex-wrap gap-2",
                  <.button(
                    ^.`type`    := "button",
                    ^.className := primaryButtonClass,
                    ^.disabled  := (s.busy || s.projectId.isEmpty || taskId.isEmpty),
                    ^.onClick --> act(BillingRpc.Assign(p.threadId, s.projectId, taskId)),
                    "Choose"
                  ),
                  <.button(
                    ^.`type`    := "button",
                    ^.className := buttonClass,
                    ^.disabled  := s.busy,
                    ^.onClick --> refreshCatalog,
                    "Refresh projects from Zoho"
                  )
                )
              )
          ,
          <.div(
            ^.className     := "flex flex-wrap gap-2",
            <.button(
              ^.`type`    := "button",
              ^.className := buttonClass,
              ^.disabled  := s.busy,
              ^.onClick --> act(BillingRpc.MarkUnbillable(p.threadId)),
              "Not client work"
            ),
            <.button(
              ^.`type`    := "button",
              ^.className := buttonClass,
              ^.disabled  := s.busy,
              ^.onClick --> act(BillingRpc.ClearMapping(p.threadId)),
              "Follow parent or project again"
            )
          )
        )
      )

  val Component = ScalaComponent
    .builder[AssignPanelProps]("AssignPanel")
    .initialState(State(mapping = None, catalog = None, projectId = "", taskId = "", busy = false, error = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.load)
    .componentDidUpdate(x => if x.prevProps.threadId != x.currentProps.threadId then x.backend.load else Callback.empty)
    .build

/** The composer form an agent's send opens: the user reads what would reach Zoho and confirms or declines. */
object SendThreadForm:
  val Component = ScalaFnComponent[SendThreadFormProps] { props =>
    val pl = props.plan
    <.div(
      ^.className := "space-y-3 rounded-lg border border-border bg-card p-4 text-sm",
      <.div(
        ^.className := "font-medium",
        s"Send ${accountOf(pl.root).fold("this thread")(_.title)} to Zoho Books?"
      ),
      <.div(
        ^.className := "text-xs text-muted-foreground",
        "An agent asked to send this. Nothing reaches Zoho unless you confirm here."
      ),
      <.div(
        ^.className := "text-xs",
        nullable(pl.target).fold("")(t => s"${t.customerName}: ${t.projectName} / ${t.taskName}")
      ),
      <.table(
        ^.className := "w-full text-xs",
        <.tbody(
          pl.windows.map { w =>
            <.tr(
              ^.key := w.key,
              <.td(
                ^.className    := "py-0.5 pr-2 whitespace-nowrap",
                s"${w.day} ${clock(w.start, pl.timeZone)}–${clock(w.end, pl.timeZone)}"
              ),
              <.td(^.className := "py-0.5 pr-2 text-right", hoursMinutes(w.durationMs)),
              <.td(^.className := "py-0.5", w.summary)
            )
          }.toVdomArray
        )
      ),
      <.div(
        ^.className := "space-y-1",
        <.div(^.className := "text-xs font-medium", "Entries"),
        pl.entries.map(e =>
          <.div(^.key := e.day, ^.className := "text-xs", s"${e.day}: ${e.logTime} to Zoho. ${e.notes}")
        ).toVdomArray
      ),
      <.div(
        ^.className := "space-y-1",
        <.div(^.className := "text-xs font-medium text-amber-600", "Already in Zoho on these days, not from bb:"),
        pl.handEntered.map(h =>
          <.div(
            ^.key       := h.timeEntryId,
            ^.className := "text-xs",
            s"${h.date}: ${hoursMinutes(h.minutes * 60_000)} ${h.taskName}: ${h.notes}"
          )
        ).toVdomArray
      ).when(pl.handEntered.nonEmpty),
      <.div(
        ^.className := "flex flex-wrap gap-2",
        <.button(
          ^.`type`    := "button",
          ^.className := primaryButtonClass,
          ^.disabled  := pl.blockers.nonEmpty,
          ^.onClick --> Callback(props.submit(js.Dynamic.literal("confirm" -> true))),
          "Send to Zoho Books"
        ),
        <.button(
          ^.`type`    := "button",
          ^.className := buttonClass,
          ^.onClick --> Callback(props.submit(js.Dynamic.literal("confirm" -> false))),
          "Don't send"
        )
      )
    )
  }

/** Raw function components consumed by app.tsx: React calls each with the plain-JS props object. */
object Exports:
  @scala.scalajs.js.annotation.JSExportTopLevel("SendThreadForm")
  val sendThreadForm: js.Function1[SendThreadFormProps, js.Any] =
    props => SendThreadForm.Component(props).vdomElement.rawElement

  @scala.scalajs.js.annotation.JSExportTopLevel("ReviewPage")
  val reviewPage: js.Function1[ReviewPageProps, js.Any] =
    props => ReviewPage.Component(props).vdomElement.rawElement

  @scala.scalajs.js.annotation.JSExportTopLevel("AssignPanel")
  val assignPanel: js.Function1[AssignPanelProps, js.Any] =
    props => AssignPanel.Component(props).vdomElement.rawElement
