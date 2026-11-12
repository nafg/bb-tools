package billing

import billingcore.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** The backend's answers as the frontend and CLI receive them. */
object Json:
  /** `{ kind: "thread", threadId }`, `{ kind: "project" }`, or null. */
  def source(source: Option[MappingSource]): js.Dynamic =
    source match
      case Some(MappingSource.Thread(id)) => js.Dynamic.literal("kind" -> "thread", "threadId" -> id)
      case Some(MappingSource.Project)    => js.Dynamic.literal("kind" -> "project")
      case None                           => null

  def resolved(r: Resolved): js.Dynamic =
    r.billability match
      case Billability.Billable(target) =>
        js.Dynamic.literal("billable" -> true, "target" -> targetJs(target), "source" -> source(r.source))
      case Billability.NotBillable      =>
        js.Dynamic.literal("billable" -> false, "target" -> null, "source" -> source(r.source))

  def targetJs(target: ZohoTarget): js.Dynamic =
    js.Dynamic.literal(
      "projectId"    -> target.projectId,
      "projectName"  -> target.projectName,
      "taskId"       -> target.taskId,
      "taskName"     -> target.taskName,
      "customerId"   -> target.customerId,
      "customerName" -> target.customerName
    )

  /** A thread account in its wire form; the page decodes it with the same codec. */
  def thread(a: ThreadAccount): js.Any = WireJs.toJs(a.encode)

  def submission(s: SubmissionRecord): js.Any = WireJs.toJs(s.encode)

  def project(
    bbProjectId: String,
    name: String,
    zoho: Vector[ProjectZoho],
    notClientWork: Boolean,
    catalog: Option[Catalog]): js.Dynamic =
    js.Dynamic.literal(
      "projectId"     -> bbProjectId,
      "name"          -> name,
      "notClientWork" -> notClientWork,
      "zoho"      -> zoho.map { z =>
        val target = catalog.flatMap(_.target(z.projectId, z.taskId))
        js.Dynamic.literal(
          "projectId"    -> z.projectId,
          "projectName"  -> target.fold(z.projectId)(_.projectName),
          "taskId"       -> z.taskId,
          "taskName"     -> target.fold(z.taskId)(_.taskName),
          "customerName" -> target.fold("")(_.customerName)
        )
      }.toJSArray
    )

  def ok: js.Any = js.Dynamic.literal("ok" -> true)

  def overview(r: OverviewReply): js.Dynamic =
    val base = js.Dynamic.literal(
      "zoho" -> js.Dynamic.literal("configured" -> r.zohoMissing.isEmpty, "missing" -> r.zohoMissing.toJSArray),
      "summariesAvailable" -> r.summariesAvailable
    )
    r.overview match
      case Right(o)    =>
        base.updateDynamic("since")(o.since.toDouble)
        base.updateDynamic("now")(o.now.toDouble)
        base.updateDynamic("timeZone")(o.calendar.timeZone)
        base.updateDynamic("zoneSource")(zoneSourceName(o.zoneSource))
        base.updateDynamic("threads")(o.threads.sortBy(_.title).map(thread).toJSArray)
        base.updateDynamic("projects")(o.threads.map(t => t.projectId -> t.projectName).distinct.map { (id, name) =>
          project(id, name, r.projectZoho.getOrElse(id, Vector.empty), r.notClientWork(id), r.catalog)
        }.toJSArray)
        base.updateDynamic("warnings")(o.warnings.toJSArray)
        base.updateDynamic("recorder")(recorder(r.recorder))
      case Left(error) =>
        base.updateDynamic("since")(r.since.toDouble)
        base.updateDynamic("now")(js.Date.now())
        base.updateDynamic("timeZone")("")
        base.updateDynamic("zoneSource")("unknown")
        base.updateDynamic("threads")(js.Array[js.Any]())
        base.updateDynamic("projects")(js.Array[js.Dynamic]())
        base.updateDynamic("warnings")(js.Array[String]())
        base.updateDynamic("recorder")(recorder(
          r.recorder.copy(available = false, error = Some(r.recorder.error.getOrElse(error)))
        ))
    base

  def mapping(m: MappingReply): js.Dynamic =
    js.Dynamic.literal(
      "threadId" -> m.threadId,
      "title"    -> m.title,
      "own"      -> (m.own match
        case Right(MappingEntry.Assigned(t)) => js.Dynamic.literal("kind" -> "assigned", "target" -> targetJs(t))
        case Right(MappingEntry.Unbillable)  => js.Dynamic.literal("kind" -> "unbillable")
        case Right(MappingEntry.Absent)      => js.Dynamic.literal("kind" -> "inherit")
        case Left(problem)                   => js.Dynamic.literal("kind" -> "invalid", "problem" -> problem)
      ),
      "resolved" -> resolved(m.resolved),
      "choices"  -> m.choices.map(targetJs).toJSArray
    )

  def report(r: Service#Report): js.Dynamic =
    js.Dynamic.literal(
      "customerId"   -> r.customerId,
      "customerName" -> r.customerName,
      "from"         -> r.from,
      "to"           -> r.to,
      "totalMinutes" -> r.totalMinutes.toDouble,
      "markdown"     -> r.markdown,
      "lines"        -> r.lines.map(l =>
        js.Dynamic.literal(
          "threadId" -> l.threadId,
          "title"    -> l.title,
          "minutes"  -> l.minutes.toDouble,
          "days"     -> l.days.toJSArray,
          "project"  -> l.project,
          "task"     -> l.task
        )
      ).toJSArray
    )

  /** The reply of a request as the page reads it. */
  def reply[R](service: Service, rpc: BillingRpc[R], out: Served[R]): js.Any =
    rpc match
      case _: BillingRpc.Overview                       => overview(out)
      case _: BillingRpc.SessionScreen                  => sessionScreen(out)
      case _: BillingRpc.SessionShared                  => sessionShared(out)
      case _: BillingRpc.Ledger                         => ledger(out)
      case _: (BillingRpc.Draft | BillingRpc.Summarize) =>
        out match
          case p: PlanResult => planResult(p, service.blockers(p))
          case _             => ok
      case _: BillingRpc.Submit                         => submission(out)
      case _: BillingRpc.Undo                           => js.Dynamic.literal("deleted" -> out)
      case _: BillingRpc.SettleAll                      => js.Dynamic.literal("decided" -> out)
      case _: BillingRpc.Audit                          => js.Dynamic.literal("text" -> Text.audit(out))
      case _: BillingRpc.Submissions.type               =>
        js.Dynamic.literal("timeZone" -> out.timeZone, "submissions" -> out.submissions.map(submission).toJSArray)
      case _: BillingRpc.Catalog                        => out.json
      case _: BillingRpc.Mapping                        => mapping(out)
      case _: BillingRpc.ProjectMappings.type           =>
        js.Dynamic.literal("projects" -> out.projects.map((id, name, zoho) =>
          project(id, name, zoho, out.notClientWork(id), out.catalog)
        ).toJSArray)
      case _: BillingRpc.Report                         => report(out)
      case _: (BillingRpc.SetExcluded | BillingRpc.SetNotes | BillingRpc.Decide | BillingRpc.Undecide |
            BillingRpc.Assign | BillingRpc.MarkUnbillable | BillingRpc.ClearMapping | BillingRpc.SetProjectMapping |
            BillingRpc.SetProjectNotClientWork) =>
        ok

  def zoneSourceName(source: ZoneSource): String =
    source match
      case ZoneSource.Zoho   => "zoho"
      case ZoneSource.Server => "server"

  def recorder(s: RecorderStatus): js.Dynamic =
    js.Dynamic.literal("available" -> s.available, "error" -> s.error.orNull, "status" -> s.status.orNull)

  def block(
    b: OverlapBlock,
    decided: Option[Allocation],
    billable: String => Boolean,
    titles: Map[String, String],
    blockThreads: Map[String, Vector[BlockThread]] = Map.empty): js.Dynamic =
    js.Dynamic.literal(
      "threads"           -> blockThreads.getOrElse(b.key.text, Vector.empty).map(bt =>
        js.Dynamic.literal(
          "threadId"    -> bt.threadId,
          "title"       -> bt.title,
          "projectName" -> bt.projectName,
          "billable"    -> billable(bt.threadId),
          "messages"    -> bt.messages.map(message).toJSArray
        )
      ).toJSArray,
      "key"               -> b.key.text,
      "day"               -> b.day,
      "start"             -> b.start.toDouble,
      "end"               -> b.end.toDouble,
      "durationMs"        -> b.durationMs.toDouble,
      "threadIds"         -> b.threadIds.toSeq.sorted.toJSArray,
      "titles"            -> js.Dictionary(b.threadIds.toSeq.map(t => t -> titles.getOrElse(t, t))*),
      "billableThreadIds" -> b.threadIds.toSeq.filter(billable).sorted.toJSArray,
      "decision"          -> decided.map(a => WireJs.toJs(a.encode)).orNull
    )

  private def windowLine(l: WindowLine, titles: Map[String, String]): js.Dynamic =
    js.Dynamic.literal(
      "key"        -> l.window.key,
      "threadId"   -> l.window.threadId,
      "title"      -> titles.getOrElse(l.window.threadId, l.window.threadId),
      "day"        -> l.window.day,
      "start"      -> l.window.start.toDouble,
      "end"        -> l.window.end.toDouble,
      "durationMs" -> l.window.durationMs.toDouble,
      "summary"    -> l.summary,
      "fromModel"  -> l.fromModel,
      "messages"   -> l.messages.map(m =>
        js.Dynamic.literal(
          "at"      -> m.at.toDouble,
          "seq"     -> m.seq,
          "speaker" -> m.speaker.toString,
          "text"    -> m.text.take(if m.speaker == Speaker.Work then 200 else 600)
        )
      ).toJSArray
    )

  def trace(t: ZohoTrace): js.Dynamic =
    t match
      case ZohoTrace.TimerRan(s, e, d)      =>
        js.Dynamic.literal("kind" -> "timerRan", "start" -> s.toDouble, "end" -> e.toDouble, "detail" -> d)
      case ZohoTrace.TimerLeftRunning(s, d) =>
        js.Dynamic.literal("kind" -> "timerLeftRunning", "start" -> s.toDouble, "end" -> s.toDouble, "detail" -> d)

  def message(m: Message): js.Dynamic =
    js.Dynamic.literal(
      "at"      -> m.at.toDouble,
      "seq"     -> m.seq,
      "speaker" -> m.speaker.toString,
      "text"    -> m.text.take(if m.speaker == Speaker.Work then 200 else 600)
    )

  def stretchState(s: StretchState): js.Dynamic =
    s match
      case StretchState.Unbilled        => js.Dynamic.literal("kind" -> "unbilled")
      case StretchState.LeftOut         => js.Dynamic.literal("kind" -> "leftOut")
      case StretchState.InZohoByHand(t) => js.Dynamic.literal("kind" -> "inZohoByHand", "trace" -> trace(t))
      case StretchState.Sent(id)        => js.Dynamic.literal("kind" -> "sent", "submissionId" -> id)
      case StretchState.Settled(id)     => js.Dynamic.literal("kind" -> "settled", "submissionId" -> id)
      case StretchState.GivenAway(to)   => js.Dynamic.literal("kind" -> "givenAway", "to" -> to.toSeq.sorted.toJSArray)

  def stretchLine(ln: LedgerLine, titles: Map[String, String]): js.Dynamic =
    val w = ln.stretch.window
    js.Dynamic.literal(
      "key"        -> w.key,
      "threadId"   -> w.threadId,
      "title"      -> titles.getOrElse(w.threadId, w.threadId),
      "day"        -> w.day,
      "start"      -> w.start.toDouble,
      "end"        -> w.end.toDouble,
      "durationMs" -> w.durationMs.toDouble,
      "summary"    -> ln.summary,
      "fromModel"  -> ln.fromModel,
      "state"      -> stretchState(ln.stretch.state),
      "messages"   -> ln.messages.map(message).toJSArray
    )

  def sessionScreen(r: SessionScreenResult): js.Dynamic =
    js.Dynamic.literal(
      "timeZone" -> r.calendar.timeZone,
      "now"      -> r.now.toDouble,
      "sessions" -> r.sessions.map(s =>
        js.Dynamic.literal(
          "start"      -> s.start.toDouble,
          "end"        -> s.end.toDouble,
          "threads"    -> s.threadIds.size,
          "unbilledMs" -> r.unbilledMs.getOrElse(s.start, 0L).toDouble
        )
      ).toJSArray,
      "current"   -> r.chosen.fold(-1)(c => r.sessions.indexWhere(_.start == c.start)),
      "threads"   -> r.threads.map(thread).toJSArray,
      "stretches"   -> r.stretches.map(stretchLine(_, r.titles)).toJSArray,
      "submissions" -> js.Dictionary(r.submissionStamps.toSeq.map((id, at) => id -> at.toDouble)*),
      "summarizing" -> r.summarizing.toJSArray,
      "signals"     -> r.signals.map(g =>
        js.Dynamic.literal("threadId" -> g.threadId, "kind" -> g.kind, "at" -> g.at.toDouble)
      ).toJSArray
    )

  /** Each root's undecided shared stretches, as the page decides them: in the root's own ledger. */
  def sessionShared(roots: Vector[(LedgerResult, Vector[(OverlapBlock, Option[Allocation])])]): js.Dynamic =
    js.Dynamic.literal(
      "roots" -> roots.map((l, blocks) =>
        js.Dynamic.literal(
          "root"   -> l.root.threadId,
          "blocks" -> blocks.map((b, a) => block(b, a, billableIn(l), l.titles, l.blockThreads)).toJSArray
        )
      ).toJSArray
    )

  private def billableIn(l: LedgerResult): String => Boolean = t =>
    l.billability(t) match
      case Billability.Billable(_) => true
      case Billability.NotBillable => false

  def ledger(l: LedgerResult): js.Dynamic =
    val billable = billableIn(l)
    js.Dynamic.literal(
      "root" -> thread(l.root),
      "covered"     -> l.covered.map(thread).toJSArray,
      "leftOut"     -> l.leftOut.map((t, why) => js.Dynamic.literal("thread" -> thread(t), "why" -> why)).toJSArray,
      "target"      -> l.target.map(targetJs).orNull,
      "timeZone"    -> l.calendar.timeZone,
      "stretches"   -> l.stretches.map(stretchLine(_, l.titles)).toJSArray,
      "blocks"      -> l.blocks.map((b, a) => block(b, a, billable, l.titles, l.blockThreads)).toJSArray,
      "history"     -> l.history.map(submission).toJSArray,
      "handEntered" -> l.handEntered.map { h =>
        js.Dynamic.literal(
          "timeEntryId"  -> h.entry.timeEntryId,
          "date"         -> h.entry.date,
          "minutes"      -> h.entry.minutes.toDouble,
          "taskName"     -> h.entry.taskName,
          "userName"     -> h.entry.userName,
          "notes"        -> h.entry.notes,
          "billedStatus" -> h.entry.billedStatus
        )
      }.toJSArray,
      "handEnteredError"   -> l.handEnteredError.orNull,
      "summariesAvailable" -> l.summariesAvailable,
      "billableThreadIds"  -> (l.coveredIds ++ l.blocks.flatMap(_._1.threadIds)).distinct.filter(billable).toJSArray,
      "recording"          -> l.recording.map(i =>
        js.Dynamic.literal(
          "day"      -> i.day,
          "threadId" -> i.threadId,
          "start"    -> i.start.toDouble,
          "end"      -> i.end.toDouble
        )
      ).toJSArray
    )

  def planResult(p: PlanResult, blockers: Vector[String]): js.Dynamic =
    val coveredIds = p.covered.map(_.threadId).toSet
    js.Dynamic.literal(
      "root"        -> thread(p.root),
      "covered"     -> p.covered.map(thread).toJSArray,
      "leftOut"     -> p.leftOut.map((t, why) => js.Dynamic.literal("thread" -> thread(t), "why" -> why)).toJSArray,
      "target"      -> p.target.map(targetJs).orNull,
      "timeZone"    -> p.calendar.timeZone,
      "windows"     -> p.lines.map(windowLine(_, p.titles)).toJSArray,
      "entries"     -> p.entries.map { e =>
        js.Dynamic.literal(
          "day"            -> e.day,
          "exactMs"        -> e.exactMs.toDouble,
          "billedMinutes"  -> e.billedMinutes.toDouble,
          "logTime"        -> ZohoEntries.logTime(e.billedMinutes),
          "notes"          -> e.notes,
          "notesEdited"    -> e.notesEdited,
          "notesFromModel" -> e.notesFromModel
        )
      }.toJSArray,
      "totalMs"     -> p.plan.totalMs.toDouble,
      "handEntered" -> p.handEntered.map { h =>
        js.Dynamic.literal(
          "timeEntryId"  -> h.entry.timeEntryId,
          "date"         -> h.entry.date,
          "minutes"      -> h.entry.minutes.toDouble,
          "taskName"     -> h.entry.taskName,
          "userName"     -> h.entry.userName,
          "notes"        -> h.entry.notes,
          "billedStatus" -> h.entry.billedStatus
        )
      }.toJSArray,
      "handEnteredError" -> p.handEnteredError.orNull,
      "blockers"         -> blockers.toJSArray,
      "coveredIds"       -> coveredIds.toSeq.toJSArray
    )
