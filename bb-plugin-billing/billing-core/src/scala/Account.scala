package billingcore

/** A thread's recording by state, as the ledger shows it and as every list counts it. `unbilledMs` is everything no
  * submission covers, no strike-out leaves out and no decision gives to other threads; `inZohoByHandMs` is the part of
  * it a Zoho timer in the thread already logged, which only a ledger (with the thread's conversation in hand) can tell
  * apart. `givenAwayMs` is the time shared-stretch decisions gave wholly to other threads or to nobody.
  */
final case class Totals(
  unbilledMs: Long,
  inZohoByHandMs: Long,
  leftOutMs: Long,
  sentMs: Long,
  settledMs: Long,
  givenAwayMs: Long):
  def recordedMs: Long      = unbilledMs + leftOutMs + sentMs + settledMs + givenAwayMs
  def coveredMs: Long       = sentMs + settledMs
  def billedBefore: Boolean = coveredMs > 0

  def encode: Wire.Obj =
    Wire.obj(
      "unbilledMs"     -> Wire.Num(unbilledMs.toDouble),
      "inZohoByHandMs" -> Wire.Num(inZohoByHandMs.toDouble),
      "leftOutMs"      -> Wire.Num(leftOutMs.toDouble),
      "sentMs"         -> Wire.Num(sentMs.toDouble),
      "settledMs"      -> Wire.Num(settledMs.toDouble),
      "givenAwayMs"    -> Wire.Num(givenAwayMs.toDouble)
    )

object Totals:
  val zero: Totals = Totals(0, 0, 0, 0, 0, 0)

  def decode(value: Wire): Option[Totals] =
    def ms(name: String) = value.field(name).flatMap(_.asNum).map(_.toLong)
    for
      unbilled <- ms("unbilledMs"); inZoho <- ms("inZohoByHandMs"); leftOut <- ms("leftOutMs")
      sent     <- ms("sentMs"); settled    <- ms("settledMs"); givenAway <- ms("givenAwayMs")
    yield Totals(unbilled, inZoho, leftOut, sent, settled, givenAway)

/** One thread's account with billing: where it bills, what is recorded in each state, and what was sent before. The one
  * answer every list, the ledger, the CLI and the agent render.
  */
final case class ThreadAccount(
  threadId: String,
  title: String,
  projectId: String,
  projectName: String,
  parentThreadId: Option[String],
  createdAt: Long,
  archivedAt: Option[Long],
  resolved: Resolved,
  totals: Totals,
  /** How many submissions cover any of this thread's recording. */
  submissions: Int,
  /** The start of the earliest recorded window. */
  firstActivity: Option[Long],
  /** The end of the latest recorded window. */
  lastActivity: Option[Long]):
  def billsTo: Option[ZohoTarget] =
    resolved.billability match
      case Billability.Billable(t) => Some(t)
      case Billability.NotBillable => None

  def standing: Standing = Standing.of(billsTo.isDefined, resolved.source.isDefined, totals.unbilledMs, archivedAt.isDefined, submissions > 0)

  def encode: Wire.Obj =
    Wire.obj(
      "threadId"       -> Wire.Str(threadId),
      "title"          -> Wire.Str(title),
      "projectId"      -> Wire.Str(projectId),
      "projectName"    -> Wire.Str(projectName),
      "parentThreadId" -> Wire.opt(parentThreadId.map(Wire.Str.apply)),
      "createdAt"      -> Wire.Num(createdAt.toDouble),
      "archivedAt"     -> Wire.opt(archivedAt.map(a => Wire.Num(a.toDouble))),
      "resolved"       -> resolved.encode,
      "totals"         -> totals.encode,
      "submissions"    -> Wire.Num(submissions.toDouble),
      "firstActivity"  -> Wire.opt(firstActivity.map(a => Wire.Num(a.toDouble))),
      "lastActivity"   -> Wire.opt(lastActivity.map(a => Wire.Num(a.toDouble)))
    )

object ThreadAccount:
  def decode(value: Wire): Option[ThreadAccount] =
    def str(name: String) = value.field(name).flatMap(_.asStr)
    def num(name: String) = value.field(name).flatMap(_.asNum).map(_.toLong)
    for
      threadId    <- str("threadId")
      title       <- str("title")
      projectId   <- str("projectId")
      projectName <- str("projectName")
      createdAt   <- num("createdAt")
      resolved    <- value.field("resolved").flatMap(Resolved.decode)
      totals      <- value.field("totals").flatMap(Totals.decode)
      submissions <- num("submissions")
    yield ThreadAccount(
      threadId,
      title,
      projectId,
      projectName,
      str("parentThreadId"),
      createdAt,
      num("archivedAt"),
      resolved,
      totals,
      submissions.toInt,
      num("firstActivity"),
      num("lastActivity")
    )

/** A span of one thread's recording that a submission covers, with the thread's title for display. */
final case class SubmissionSpan(threadId: String, title: String, start: Long, end: Long)

/** One Zoho entry a submission created, dated `day`. */
final case class SubmissionEntry(day: String, timeEntryId: String, minutes: Long, notes: String)

/** A submission as everything outside the store sees it: the stretches it covers (the billed state itself), whether it
  * sent them or settled them without billing, and the Zoho entries it made.
  */
final case class SubmissionRecord(
  id: String,
  rootThreadId: String,
  rootTitle: String,
  spans: Vector[SubmissionSpan],
  billed: Boolean,
  target: ZohoTarget,
  createdAt: Long,
  entries: Vector[SubmissionEntry]):
  /** Submissions from before spans were recorded cover nothing by themselves; only their entries remain. */
  def legacy: Boolean = spans.isEmpty

  def encode: Wire.Obj =
    Wire.obj(
      "id"           -> Wire.Str(id),
      "rootThreadId" -> Wire.Str(rootThreadId),
      "rootTitle"    -> Wire.Str(rootTitle),
      "spans"        -> Wire.Arr(spans.map(s =>
        Wire.obj(
          "threadId" -> Wire.Str(s.threadId),
          "title"    -> Wire.Str(s.title),
          "start"    -> Wire.Num(s.start.toDouble),
          "end"      -> Wire.Num(s.end.toDouble)
        )
      )),
      "billed"       -> Wire.Bool(billed),
      "target"       -> target.encode,
      "createdAt"    -> Wire.Num(createdAt.toDouble),
      "entries"      -> Wire.Arr(entries.map(e =>
        Wire.obj(
          "day"         -> Wire.Str(e.day),
          "timeEntryId" -> Wire.Str(e.timeEntryId),
          "minutes"     -> Wire.Num(e.minutes.toDouble),
          "notes"       -> Wire.Str(e.notes)
        )
      ))
    )

object SubmissionRecord:
  def decode(value: Wire): Option[SubmissionRecord] =
    def str(name: String) = value.field(name).flatMap(_.asStr)
    def span(v: Wire)     =
      for
        threadId <- v.field("threadId").flatMap(_.asStr); title <- v.field("title").flatMap(_.asStr)
        start    <- v.field("start").flatMap(_.asNum); end      <- v.field("end").flatMap(_.asNum)
      yield SubmissionSpan(threadId, title, start.toLong, end.toLong)
    def entry(v: Wire)    =
      for
        day     <- v.field("day").flatMap(_.asStr); id        <- v.field("timeEntryId").flatMap(_.asStr)
        minutes <- v.field("minutes").flatMap(_.asNum); notes <- v.field("notes").flatMap(_.asStr)
      yield SubmissionEntry(day, id, minutes.toLong, notes)
    for
      id        <- str("id")
      root      <- str("rootThreadId")
      rootTitle <- str("rootTitle")
      spans     <- value.field("spans").flatMap(_.asArr).map(_.flatMap(span))
      billed    <- value.field("billed").flatMap(_.asBool)
      target    <- value.field("target").flatMap(ZohoTarget.decode)
      createdAt <- value.field("createdAt").flatMap(_.asNum)
      entries   <- value.field("entries").flatMap(_.asArr).map(_.flatMap(entry))
    yield SubmissionRecord(id, root, rootTitle, spans, billed, target, createdAt.toLong, entries)

/** The one sentence for each thing, so every surface says it the same way. `stamp` writes an instant in the user's
  * zone, `Oct 18, 14:05`; `dayLabel` a civil day, `Thu 2026-10-08`.
  */
object Words:
  import Durations.hoursMinutes as hm

  def target(t: ZohoTarget): String = s"${t.customerName}: ${t.projectName} / ${t.taskName}"

  def billsTo(r: Resolved): String =
    r.billability match
      case Billability.Billable(t) => target(t)
      case Billability.NotBillable => if r.source.isDefined then "not client work" else "not chosen whether client work"

  /** Which rule decided where a thread bills, seen from that thread. */
  def decidedBy(r: Resolved, threadId: String): String =
    r.source match
      case Some(MappingSource.Thread(id)) if id == threadId => "chosen here"
      case Some(MappingSource.Thread(id))                   => s"follows thread $id"
      case Some(MappingSource.Project)                      => "bb project rule"
      case None                                             => "not chosen"

  def dayLabel(day: String): String =
    CivilDate.parseOption(day).fold(day) { d =>
      val names = Vector("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      s"${names(d.dayOfWeek)} $day"
    }

  /** `47m unbilled (12m in Zoho by hand), 2h 25m sent or settled in 2 submissions`. */
  def totals(t: Totals, submissions: Int): String =
    val unbilled =
      (if t.unbilledMs == 0 then "nothing unbilled"
       else if t.unbilledMs < 60_000 then "under a minute unbilled"
       else s"${hm(t.unbilledMs)} unbilled") +
        (if t.inZohoByHandMs > 0 then s" (${hm(t.inZohoByHandMs)} in Zoho by hand)" else "")
    val before   =
      if submissions == 0 then "nothing sent before"
      else
        s"${hm(t.coveredMs)} sent or settled in $submissions ${if submissions == 1 then "submission" else "submissions"}"
    val leftOut  = if t.leftOutMs > 0 then s", ${hm(t.leftOutMs)} left out" else ""
    val toOthers = if t.givenAwayMs > 0 then s", ${hm(t.givenAwayMs)} given to other threads" else ""
    s"$unbilled, $before$leftOut$toOthers"

  /** When the work was recorded and when the thread was archived: `recorded Sep 15 – Oct 7 · archived Oct 7`. `day`
    * writes an instant as a short date.
    */
  def when(a: ThreadAccount, day: Long => String): String =
    val recorded = (a.firstActivity, a.lastActivity) match
      case (Some(f), Some(l)) if day(f) == day(l) => Some(s"recorded ${day(f)}")
      case (Some(f), Some(l))                     => Some(s"recorded ${day(f)} – ${day(l)}")
      case _                                      => None
    Vector(recorded, a.archivedAt.map(t => s"archived ${day(t)}")).flatten.mkString(" · ")

  /** The stretches a submission covers: per thread, from its first to its last clock time. */
  def spans(s: SubmissionRecord, stamp: Long => String): String =
    val byThread = s.spans.groupBy(_.threadId).toVector.sortBy(_._2.map(_.start).min)
    if byThread.isEmpty then "nothing"
    else
      byThread.map { (_, sps) =>
        val sorted = sps.sortBy(_.start)
        s"${sorted.head.title.take(40)} from ${stamp(sorted.head.start)} to ${stamp(sorted.last.end)}"
      }.mkString("; ")

  /** `Sent to Zoho on Oct 26, 13:37: Thu 2026-10-08 39m; covers X from … to …`. */
  def submission(s: SubmissionRecord, stamp: Long => String): String =
    val entries = s.entries.map(e => s"${dayLabel(e.day)} ${hm(e.minutes * 60_000)}").mkString(", ")
    if s.legacy then s"Sent per day on ${stamp(s.createdAt)}, before submissions recorded their stretches: $entries"
    else if s.billed then s"Sent to Zoho on ${stamp(s.createdAt)}: $entries; covers ${spans(s, stamp)}"
    else s"Settled without billing on ${stamp(s.createdAt)}; covers ${spans(s, stamp)}"
