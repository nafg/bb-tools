package billingapp

import billingcore.{SubmissionRecord, ThreadAccount, WireJs}

import scala.scalajs.js

// Read-only views of the plain objects the backend's Rpc returns and of the
// props app.tsx passes in. Non-native traits suffice for reading.

/** `(method, input) => Promise<result>`: the bb rpc call, bound in app.tsx. */
type RpcCall = js.Function2[String, js.Any, js.Promise[js.Any]]

trait Target extends js.Object:
  val projectId: String
  val projectName: String
  val taskId: String
  val taskName: String
  val customerId: String
  val customerName: String

trait ProjectZohoRow extends js.Object:
  val projectId: String
  val projectName: String
  val taskId: String
  val taskName: String
  val customerName: String

trait ProjectInfo extends js.Object:
  val projectId: String
  val name: String
  val zoho: js.Array[ProjectZohoRow]

trait ProjectMappingsResult extends js.Object:
  val projects: js.Array[ProjectInfo]

trait RecorderInfo extends js.Object:
  val available: Boolean
  val error: String | Null
  val status: js.Dynamic

trait ZohoStatus extends js.Object:
  val configured: Boolean
  val missing: js.Array[String]

trait OverviewResult extends js.Object:
  val since: Double
  val now: Double
  val timeZone: String
  val zoneSource: String
  val zoho: ZohoStatus
  val summariesAvailable: Boolean

  /** `billingcore.ThreadAccount`s in their wire form. */
  val threads: js.Array[js.Any]
  val projects: js.Array[ProjectInfo]
  val warnings: js.Array[String]
  val recorder: RecorderInfo

trait Decision extends js.Object:
  val kind: String
  val threadId: js.UndefOr[String]
  val ms: js.UndefOr[js.Dictionary[Double]]

trait ZohoTraceJs extends js.Object:
  /** "timerRan", "timerLeftRunning", or "entryMade". */
  val kind: String
  val start: Double
  val end: Double
  val detail: String

trait BlockThreadJs extends js.Object:
  val threadId: String
  val title: String
  val projectName: String
  val billable: Boolean
  val messages: js.Array[UserMessageJs]

trait Block extends js.Object:
  /** Every thread in the stretch, with its conversation during it. */
  val threads: js.Array[BlockThreadJs]
  val key: String
  val day: String
  val start: Double
  val end: Double
  val durationMs: Double
  val threadIds: js.Array[String]
  val titles: js.Dictionary[String]
  val billableThreadIds: js.Array[String]
  val decision: Decision | Null

trait WindowJs extends js.Object:
  val key: String
  val threadId: String
  val title: String
  val day: String
  val start: Double
  val end: Double
  val durationMs: Double
  val summary: String
  val fromModel: Boolean
  val excluded: Boolean

  /** The conversation during the stretch: the user's turns, the agent's replies, and the agent's work. */
  val messages: js.Array[UserMessageJs]

trait UserMessageJs extends js.Object:
  val at: Double
  val seq: String

  /** "User", "Agent", or "Work" (a command or tool the agent used). */
  val speaker: String
  val text: String

trait EntryJs extends js.Object:
  val day: String
  val exactMs: Double
  val billedMinutes: Double
  val logTime: String
  val notes: String
  val notesEdited: Boolean

  /** The notes are the model's paragraph for the day, not the stretch lines joined. */
  val notesFromModel: Boolean

trait HandEnteredJs extends js.Object:
  val timeEntryId: String
  val date: String
  val minutes: Double
  val taskName: String
  val userName: String
  val notes: String
  val billedStatus: String

trait LeftOutJs extends js.Object:
  /** A `billingcore.ThreadAccount` in its wire form. */
  val thread: js.Any
  val why: String

/** A priced draft: the entries it would create, with their notes, and what stops it from being sent. */
trait PlanJs extends js.Object:
  /** A `billingcore.ThreadAccount` in its wire form. */
  val root: js.Any
  val target: Target | Null
  val timeZone: String
  val windows: js.Array[WindowJs]
  val entries: js.Array[EntryJs]
  val totalMs: Double
  val handEntered: js.Array[HandEnteredJs]
  val blockers: js.Array[String]

trait StretchStateJs extends js.Object:
  /** "unbilled", "leftOut", "inZohoByHand", "sent", "settled". */
  val kind: String
  val submissionId: js.UndefOr[String]
  val trace: js.UndefOr[ZohoTraceJs]

trait LedgerStretchJs extends js.Object:
  val key: String
  val threadId: String
  val title: String
  val day: String
  val start: Double
  val end: Double
  val durationMs: Double
  val summary: String
  val fromModel: Boolean
  val state: StretchStateJs
  val messages: js.Array[UserMessageJs]

trait RecordingJs extends js.Object:
  val day: String
  val threadId: String
  val start: Double
  val end: Double

/** The thread ledger: every stretch with its state, and what a draft on it needs. */
trait LedgerJs extends js.Object:
  /** `billingcore.ThreadAccount`s in their wire form. */
  val root: js.Any
  val covered: js.Array[js.Any]
  val leftOut: js.Array[LeftOutJs]
  val target: Target | Null
  val timeZone: String
  val stretches: js.Array[LedgerStretchJs]
  val blocks: js.Array[Block]

  /** `billingcore.SubmissionRecord`s in their wire form. */
  val history: js.Array[js.Any]
  val handEntered: js.Array[HandEnteredJs]
  val handEnteredError: String | Null
  val summariesAvailable: Boolean
  val billableThreadIds: js.Array[String]
  val recording: js.Array[RecordingJs]

trait SubmissionsResult extends js.Object:
  val timeZone: String

  /** `billingcore.SubmissionRecord`s in their wire form. */
  val submissions: js.Array[js.Any]

trait CatalogTask extends js.Object:
  val taskId: String
  val taskName: String

trait CatalogProject extends js.Object:
  val projectId: String
  val projectName: String
  val customerId: String
  val customerName: String
  val tasks: js.Array[CatalogTask]

trait CatalogResult extends js.Object:
  val fetchedAt: Double
  val projects: js.Array[CatalogProject]

trait OwnMapping extends js.Object:
  val kind: String
  val target: js.UndefOr[Target]
  val problem: js.UndefOr[String]

/** Where a thread's billing came from: `{ kind: "thread", threadId }` or `{ kind: "project" }`. */
trait SourceRef extends js.Object:
  val kind: String
  val threadId: js.UndefOr[String]

trait ResolvedMapping extends js.Object:
  val billable: Boolean
  val target: Target | Null
  val source: SourceRef | Null

trait MappingResult extends js.Object:
  val threadId: String
  val title: String
  val own: OwnMapping
  val resolved: ResolvedMapping

trait ReportLine extends js.Object:
  val threadId: String
  val title: String
  val minutes: Double
  val days: js.Array[String]
  val project: String
  val task: String

trait ReportResult extends js.Object:
  val customerId: String
  val customerName: String
  val from: String
  val to: String
  val totalMinutes: Double
  val markdown: String
  val lines: js.Array[ReportLine]

/** The latest message on the "billing" realtime channel; `seq` grows with each one. */
trait RealtimeEventJs extends js.Object:
  val seq: Int
  val payload: js.Any

trait ReviewPageProps extends js.Object:
  val call: RpcCall
  val subPath: String
  val navigate: js.Function1[String, Unit]
  val openThread: js.Function1[String, Unit]
  val event: RealtimeEventJs | Null

trait AssignPanelProps extends js.Object:
  val threadId: String
  val call: RpcCall

/** The pending `send-thread` interaction: the plan the backend showed, and how to answer it. */
trait SendThreadFormProps extends js.Object:
  val plan: PlanJs
  val submit: js.Function1[js.Any, Unit]
  val cancel: js.Function0[Unit]

def errorMessage(e: Throwable): String =
  e match
    case js.JavaScriptException(err) =>
      val d = err.asInstanceOf[js.Dynamic]
      val m = d.message.asInstanceOf[js.UndefOr[Any]].fold("")(_.toString)
      if m.nonEmpty then m else err.toString
    case other                       => Option(other.getMessage).getOrElse(other.toString)

def nullable[A](value: A | Null): Option[A] = Option(value.asInstanceOf[A])

/** `HH:mm` of an instant in the given zone. */
def clock(ms: Double, timeZone: String): String =
  val format = js.Dynamic.newInstance(js.Dynamic.global.Intl.DateTimeFormat)(
    "en-US",
    js.Dynamic.literal("timeZone" -> timeZone, "hourCycle" -> "h23", "hour" -> "2-digit", "minute" -> "2-digit")
  )
  format.format(new js.Date(ms)).asInstanceOf[String]

/** `Oct 18` of an instant in the given zone. */
def shortDay(ms: Double, timeZone: String): String =
  val format = js.Dynamic.newInstance(js.Dynamic.global.Intl.DateTimeFormat)(
    "en-US",
    js.Dynamic.literal("timeZone" -> timeZone, "month" -> "short", "day" -> "numeric")
  )
  format.format(new js.Date(ms)).asInstanceOf[String]

/** `Oct 18, 14:05` of an instant in the given zone. */
def stamp(ms: Double, timeZone: String): String =
  val format = js.Dynamic.newInstance(js.Dynamic.global.Intl.DateTimeFormat)(
    "en-US",
    js.Dynamic.literal(
      "timeZone"  -> timeZone,
      "hourCycle" -> "h23",
      "month"     -> "short",
      "day"       -> "numeric",
      "hour"      -> "2-digit",
      "minute"    -> "2-digit"
    )
  )
  format.format(new js.Date(ms)).asInstanceOf[String]

def hoursMinutes(ms: Double): String = billingcore.Durations.hoursMinutes(ms.toLong)

/** The account or record behind a value of the wire; a value the codec does not know is dropped. */
def accountOf(value: js.Any): Option[ThreadAccount]             = ThreadAccount.decode(WireJs.fromJs(value))
def accounts(values: js.Array[js.Any]): Vector[ThreadAccount]   = values.toVector.flatMap(accountOf)
def recordOf(value: js.Any): Option[SubmissionRecord]           = SubmissionRecord.decode(WireJs.fromJs(value))
def records(values: js.Array[js.Any]): Vector[SubmissionRecord] = values.toVector.flatMap(recordOf)

trait SessionRefJs extends js.Object:
  val start: Double
  val end: Double
  val threads: Int

  /** Unbilled time on threads that bill, in this session. */
  val unbilledMs: Double

/** The sessions screen's reply: the recent sessions, and the stretches of every thread in the current one. */
trait SessionScreenJs extends js.Object:
  val timeZone: String
  val now: Double
  val sessions: js.Array[SessionRefJs]

  /** Index into `sessions` of the one whose stretches follow, or -1 when there are none. */
  val current: Int

  /** `billingcore.ThreadAccount`s in their wire form. */
  val threads: js.Array[js.Any]
  val stretches: js.Array[LedgerStretchJs]
