package billing

import activitycore.{AgentSpan, Window, Session, Sessions}

import bbplugin.Dyn.{number, str}
import bbplugin.{BbApi, BbSettingsHandle}
import billingcore.*

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

/** What bb knows about a thread that billing shows. */
final case class ThreadSummary(
  title: String,
  projectId: String,
  parentThreadId: Option[String],
  createdAt: Long,
  archivedAt: Option[Long],
  deleted: Boolean)

/** Whether a bb SDK failure is bb answering that the thread does not exist (it was deleted). */
def isThreadNotFound(error: Throwable): Boolean = error match
  case js.JavaScriptException(e) => str(e.asInstanceOf[js.Dynamic].code) == "thread_not_found"
  case _                         => false

/** The "To bill" view: every thread with recorded attention since `since`, resolved, with what is unbilled. */
final case class Overview(
  since: Long,
  now: Long,
  calendar: IntlCalendar,
  zoneSource: ZoneSource,
  threads: Vector[ThreadAccount],
  projectRules: Map[String, ProjectRule],
  warnings: Vector[String])

/** The sessions screen: the recent sessions, and for one of them every thread's stretches in their ledger state. */
final case class SessionScreenResult(
  now: Long,
  calendar: IntlCalendar,
  sessions: Vector[Session],
  /** What each session still has to bill: unbilled time on threads that bill, by the session's start. */
  unbilledMs: Map[Long, Long],
  chosen: Option[Session],
  threads: Vector[ThreadAccount],
  stretches: Vector[LedgerLine],
  titles: Map[String, String],
  /** When each submission was made, by its id, for "sent on" and "settled on". */
  submissionStamps: Map[String, Long],
  /** The billing roots whose summaries are being written now. */
  summarizing: Set[String],
  /** The recorded signals of the chosen session's threads that start in it. */
  signals: Vector[SignalMark])

/** A recorded signal where it starts: its kind is the activity recorder's name for it (`speech`, `message`, …). */
final case class SignalMark(threadId: String, kind: String, at: Long)

/** A window as the submission panel shows it: with its day, clock times, and one-line summary. */
final case class WindowLine(
  window: Window,
  summary: String,
  fromModel: Boolean,
  /** The conversation during the window, both sides, so the line can be checked against what happened. */
  messages: Vector[Message])

/** A dated Zoho entry a submission would create. */
/** A dated Zoho entry a submission would create. `notesFromModel` says the notes are the model's paragraph for the day;
  * otherwise they are the day's stretch lines joined, unless the user edited them (`notesEdited`).
  */
final case class EntryLine(
  day: String,
  exactMs: Long,
  billedMinutes: Long,
  notes: String,
  notesEdited: Boolean,
  notesFromModel: Boolean)

/** A hand-entered Zoho entry on one of the submission's dates that this plugin did not create. */
final case class HandEntered(entry: ZohoExistingEntry)

/** A thread that takes part in a shared stretch, with what went on in it during the stretch. */
final case class BlockThread(threadId: String, title: String, projectName: String, messages: Vector[Message])

final case class PlanResult(
  root: ThreadAccount,
  covered: Vector[ThreadAccount],
  /** Threads under the root left out because they bill elsewhere or are not client work. */
  leftOut: Vector[(ThreadAccount, String)],
  target: Option[ZohoTarget],
  calendar: IntlCalendar,
  plan: SubmissionPlan,
  lines: Vector[WindowLine],
  entries: Vector[EntryLine],
  handEntered: Vector[HandEntered],
  handEnteredError: Option[String],
  /** Zoho timer runs and entries made from the covered threads on the plan's days, by thread. */
  zohoTraces: Vector[(String, ZohoTrace)],
  /** Every thread of every shared stretch, with its conversation during that stretch, keyed by block key then thread.
    */
  blockThreads: Map[String, Vector[BlockThread]],
  summariesAvailable: Boolean,
  /** Draftable stretches of the covered threads the draft leaves out: what stays unbilled if this is sent as is. */
  later: Vector[WindowLine],
  /** Every submission that covers any of the covered threads, newest first, so the panel can say what was sent. */
  history: Vector[SubmissionRecord],
  titles: Map[String, String])

/** One stretch of the ledger as the page shows it: its state, its line, and the conversation during it. */
final case class LedgerLine(stretch: LedgerStretch, summary: String, fromModel: Boolean, messages: Vector[Message]):
  def key: String = stretch.key

/** The thread ledger: every stretch of the recording of `root` and of the subthreads that bill with it, each with its
  * state, plus everything a draft on it needs. The draft is a set of stretch keys; `draft` prices it.
  */
final case class LedgerResult(
  root: ThreadAccount,
  covered: Vector[ThreadAccount],
  leftOut: Vector[(ThreadAccount, String)],
  target: Option[ZohoTarget],
  calendar: IntlCalendar,
  stretches: Vector[LedgerLine],
  /** Shared stretches touching any draftable stretch, decided or not. */
  blocks: Vector[(OverlapBlock, Option[Allocation])],
  blockThreads: Map[String, Vector[BlockThread]],
  history: Vector[SubmissionRecord],
  handEntered: Vector[HandEntered],
  handEnteredError: Option[String],
  summariesAvailable: Boolean,
  titles: Map[String, String],
  /** The day-split recording of every thread involved, for the shared-time arithmetic of a draft. */
  recording: Vector[DayInterval],
  billability: String => Billability,
  projectId: String):
  def coveredIds: Vector[String] = covered.map(_.threadId)

final class Service(
  bb: BbApi,
  settings: BbSettingsHandle,
  store: Store,
  recorder: Recorder,
  mappings: Mappings,
  projectMappings: ProjectMappings,
  zoho: Zoho,
  summaries: Summaries,
  conversation: Conversation,
  changes: ChangeFeed
)(using ExecutionContext):
  changes.keep(dropAffected)
  private val cache = mutable.Map.empty[String, ThreadSummary]

  /** A thread's conversation is read from bb's timeline once and kept while nothing new can be in it: until the thread
    * has recorded activity (attention, or an agent turn the user set going) that ended after the read, or at most
    * [[ConversationTtlMs]], which bounds what the recorder does not see. A stretch is cut on recorded activity, so a
    * conversation read after the thread's last activity ended already holds everything said inside any stretch. An
    * archived thread's is kept until a thread is unarchived (see [[forgetArchived]]): nothing is said in an archived
    * thread.
    */
  private val conversations         = mutable.Map.empty[String, (Double, Future[Vector[Message]])]
  private val archivedConversations = mutable.Map.empty[String, Future[Vector[Message]]]
  private val ConversationTtlMs     = 10 * 60_000.0

  /** How long after recorded activity ends its rows may still be written: the recorder's lag. */
  private val ActivitySettleMs = 60_000.0

  /** When each thread's latest recorded activity ended, as far as the recordings read so far tell. */
  private val activityEnd = mutable.Map.empty[String, Double]

  private def noteActivity(threadId: String, end: Long): Unit =
    if activityEnd.get(threadId).forall(_ < end) then activityEnd.update(threadId, end.toDouble)

  private def conversationOf(threadId: String): Future[Vector[Message]] =
    archivedConversations.get(threadId) match
      case Some(f) => f
      case None    =>
        val now = js.Date.now()
        conversations.get(threadId).filter((at, _) =>
          now - at < ConversationTtlMs && activityEnd.get(threadId).forall(end => at >= end + ActivitySettleMs)
        ) match
          case Some((_, f)) => f
          case None         =>
            val f = threadSummary(threadId).flatMap { summary =>
              val read = conversation.of(threadId).recoverWith { case e =>
                Future.failed(
                  new RuntimeException(s"Reading the conversation of thread $threadId failed: ${errorMessage(e)}", e)
                )
              }
              if summary.archivedAt.isDefined then
                archivedConversations.update(threadId, read)
                read.failed.foreach(_ => archivedConversations.remove(threadId))
              read
            }
            conversations.update(threadId, (now, f))
            f.failed.foreach(_ => conversations.remove(threadId))
            f

  /** Drops what is kept for archived threads (their summaries and conversations), one of which may have just been
    * unarchived.
    */
  def forgetArchived(): Unit =
    cache.clear()
    archivedConversations.clear()

  /** How long a window may run before it is cut at a message boundary. */
  private val WindowMaxMs = 60 * 60_000L

  def incrementMinutes(): Future[Int] =
    settings.get().toFuture.map(s =>
      str(s.billingIncrementMinutes).toDoubleOption.map(_.toInt).filter(_ >= 1).getOrElse(1)
    )

  def calendar(): Future[(IntlCalendar, ZoneSource)] =
    zoho.timeZone().map((zone, source) => (IntlCalendar(zone), source))

  def recorderStatus(): Future[RecorderStatus] = recorder.status()

  def summariesAvailable(): Future[Boolean] = summaries.available()

  /** How long one listing of the open threads answers for all of them. */
  private val OpenThreadsTtlMs = 5_000.0
  private val OpenThreadsPage  = 500
  private var openThreadsRead: Option[(Double, Future[Map[String, ThreadSummary]])] = None

  /** Every thread that is not archived, hidden ones included, by id: one listing, kept a few seconds. A screen looks up
    * hundreds of threads at once, and bb has no lookup of many threads by id. A failed listing is an empty map, so each
    * thread is then looked up alone.
    */
  private def openThreads(): Future[Map[String, ThreadSummary]] =
    val now = js.Date.now()
    openThreadsRead.filter((at, _) => now - at < OpenThreadsTtlMs) match
      case Some((_, f)) => f
      case None         =>
        def page(offset: Int, acc: Vector[js.Dynamic]): Future[Vector[js.Dynamic]] =
          bb.sdk.threads
            .list(js.Dynamic.literal("includeHidden" -> true, "limit" -> OpenThreadsPage, "offset" -> offset))
            .toFuture
            .flatMap { rows =>
              val got = rows.asInstanceOf[js.Array[js.Dynamic]].toVector
              if got.size < OpenThreadsPage then Future.successful(acc ++ got) else page(offset + got.size, acc ++ got)
            }
        val f = page(0, Vector.empty)
          .map(_.map(t => str(t.id) -> summaryOf(str(t.id), t)).toMap)
          .recover { case _ => Map.empty }
        openThreadsRead = Some((now, f))
        f

  private def summaryOf(threadId: String, thread: js.Dynamic): ThreadSummary =
    val title =
      Option(str(thread.title)).filter(_.nonEmpty).orElse(Option(str(thread.titleFallback)).filter(_.nonEmpty))
    ThreadSummary(
      title.getOrElse(threadId),
      str(thread.projectId),
      Option(str(thread.parentThreadId)).filter(_.nonEmpty),
      number(thread.createdAt).fold(0L)(_.toLong),
      number(thread.archivedAt).map(_.toLong),
      deleted = false
    )

  /** A thread's title, project, parent, and dates. An open thread's come from [[openThreads]]; an archived thread's
    * are cached for the plugin's lifetime once it has a title (an open thread's title and archive state can still
    * change).
    */
  def threadSummary(threadId: String): Future[ThreadSummary] =
    cache.get(threadId) match
      case Some(t) => Future.successful(t)
      case None    =>
        openThreads().flatMap { open =>
          open.get(threadId).fold(readThreadSummary(threadId))(Future.successful)
        }

  private def readThreadSummary(threadId: String): Future[ThreadSummary] =
    bb.sdk.threads
      .get(js.Dynamic.literal("threadId" -> threadId))
      .toFuture
      .map { thread =>
        val summary = summaryOf(threadId, thread)
        if summary.title != threadId && summary.archivedAt.isDefined then cache.update(threadId, summary)
        summary
      }
      .recover {
        case e if isThreadNotFound(e) =>
          val deleted = ThreadSummary(threadId, "", None, 0L, None, deleted = true)
          cache.update(threadId, deleted)
          deleted
        case _                        => ThreadSummary(threadId, "", None, 0L, None, deleted = false)
      }

  /** How long a read of the recording answers narrower reads: one screen reads it for several ledgers at once. */
  private val RecordingReuseMs = 5_000.0

  /** A read of the recording: when it began, the range `[from, to)` asked for, and the intervals, clipped to it. */
  private final case class RecordingRead(at: Double, from: Long, to: Long, intervals: Future[Either[String, Vector[Interval]]])
  private var recordingRead: Option[RecordingRead] = None

  /** The recorded intervals from `from` to `to`, clipped to it as the recorder clips them, each thread's latest noted
    * for [[conversationOf]]. A read begun in the last [[RecordingReuseMs]] answers it when it reached as far back and
    * as far on, or on to when it was made (nothing is recorded after then).
    */
  private def recordedIntervals(from: Long, to: Long): Future[Either[String, Vector[Interval]]] =
    val now = js.Date.now()
    recordingRead.filter(r => now - r.at < RecordingReuseMs && r.from <= from && (r.to >= to || r.to >= r.at)) match
      case Some(wider) =>
        wider.intervals.map(_.map(_.flatMap { i =>
          val start = math.max(i.start, from)
          val end   = math.min(i.end, to)
          Option.when(start < end)(Interval(i.threadId, start, end))
        }))
      case None        =>
        val read = recorder.intervals(from, to).map(_.map { all =>
          all.foreach(i => noteActivity(i.threadId, i.end))
          all
        })
        recordingRead = Some(RecordingRead(now, from, to, read))
        read

  /** The threads among `ids` that were deleted. */
  private def deletedAmong(ids: Vector[String]): Future[Set[String]] =
    Future.traverse(ids.distinct)(t => threadSummary(t).map(sm => Option.when(sm.deleted)(t))).map(_.flatten.toSet)

  /** The recorded intervals from `from` to `to`, without those of deleted threads. */
  private def intervalsOfLiveThreads(from: Long, to: Long): Future[Either[String, Vector[Interval]]] =
    recordedIntervals(from, to).flatMap {
      case Left(error) => Future.successful(Left(error))
      case Right(all)  => deletedAmong(all.map(_.threadId)).map(deleted => Right(all.filterNot(i => deleted(i.threadId))))
    }

  /** The agent turns the user set going from `from` to `to`, without those of deleted threads. They only decide how
    * sessions group stretches, so a recorder that cannot list them leaves them out rather than failing the screen.
    */
  private def agentSpansOfLiveThreads(from: Long, to: Long): Future[Vector[AgentSpan]] =
    agentSpans(from, to).flatMap {
      case Left(_)      => Future.successful(Vector.empty)
      case Right(spans) =>
        spans.foreach(s => noteActivity(s.threadId, s.end))
        deletedAmong(spans.map(_.threadId)).map(deleted => spans.filterNot(s => deleted(s.threadId)))
    }

  /** How far before the last read agent turns are read again: a turn that ended before then no longer changes. */
  private val AgentSpansTailMs     = 10 * 60_000L
  /** How often the agent turns are read whole anyway, for what the recorder learns about past turns later. */
  private val AgentSpansFullReadMs = 10 * 60_000L
  /** A read of agent turns: when they were last read whole, the range `[from, to)` they cover, and the turns. */
  private final case class AgentSpansRead(wholeAt: Long, from: Long, to: Long, spans: Vector[AgentSpan])
  private var agentSpansRead: Option[AgentSpansRead] = None

  /** The agent turns reaching into `[from, to)`. Thirty days of them are many thousands, so after a whole read only the
    * turns reaching past [[AgentSpansTailMs]] before the last read are asked for, and the rest kept from it.
    */
  private def agentSpans(from: Long, to: Long): Future[Either[String, Vector[AgentSpan]]] =
    def within(spans: Vector[AgentSpan]) = spans.filter(s => s.end > from && s.start < to)
    agentSpansRead match
      case Some(last) if from >= last.from && to - last.wholeAt < AgentSpansFullReadMs =>
        val cut = last.to - AgentSpansTailMs
        recorder.agentSpans(cut, to).map(_.map { tail =>
          val all = last.spans.filter(_.end <= cut) ++ tail
          agentSpansRead = Some(last.copy(to = to, spans = all))
          within(all)
        })
      case _                                                                            =>
        recorder.agentSpans(from, to).map(_.map { spans =>
          agentSpansRead = Some(AgentSpansRead(wholeAt = to, from = from, to = to, spans = spans))
          spans
        })

  def threadTitle(threadId: String): Future[String] = threadSummary(threadId).map(_.title)

  private def projectNames(): Future[Map[String, String]] =
    bb.sdk.projects
      .list(js.Dynamic.literal("includePersonal" -> true))
      .toFuture
      .map(_.toVector.map(p => str(p.id) -> str(p.name)).toMap)
      .recover { case _ => Map.empty }

  final case class Context(
    resolution: Resolution,
    summaries: Map[String, ThreadSummary],
    projectNames: Map[String, String],
    projectRules: Map[String, ProjectRule])

  /** Resolution of every listed thread with the context it needs: ancestors, summaries, project names and defaults. */
  def resolveWithContext(threadIds: Seq[String]): Future[Context] =
    for
      ancestry <- mappings.ancestors(threadIds)
      everyThread = (threadIds ++ ancestry.values.flatten).distinct.toVector
      summaries  <- Future.traverse(everyThread)(t => threadSummary(t).map(t -> _)).map(_.toMap)
      projects   <- projectNames()
      catalog    <- zoho.cachedCatalog()
      rules      <- projectMappings.rules(catalog)
      ruleOf = (t: String) => summaries.get(t).flatMap(sm => rules.get(sm.projectId))
      ruleFor = (t: String) => Mapping.projectRuleFor(t, ancestry.getOrElse(t, Vector.empty), ruleOf)
      resolution <- mappings.resolveAll(threadIds, ruleFor)
    yield Context(resolution, summaries, projects, rules)

  def resolve(threadId: String): Future[Resolved] =
    resolveWithContext(Seq(threadId)).map(_.resolution.resolved.getOrElse(
      threadId,
      Resolved(Billability.NotBillable, None)
    ))

  /** What the store says about every thread's recording: the submissions' spans and the struck-out stretches, as the
    * ledger cuts with them, the time decisions gave to other threads, and how many submissions touch each thread.
    */
  private final case class Covered(
    covering: Vector[CoveredBy],
    struck: Coverage,
    givenAway: GivenAway,
    submissionsOf: String => Int)

  private def ledgerOf(): Covered =
    val struck = Coverage.of(store.excludedWindows().toVector.flatMap { key =>
      key.split('|') match
        case Array(t, s, e) => s.toLongOption.zip(e.toLongOption).map((a, b) => Span(t, a, b))
        case _              => None
    })
    val subs   = store.submissionsOf(None)
    val counts = subs.flatMap(sub => sub.spans.map(_.threadId).distinct).groupMapReduce(identity)(_ => 1)(_ + _)
    Covered(
      subs.flatMap(sub => sub.spans.map(sp => CoveredBy(sub.id, sub.billed, sp))),
      struck,
      GivenAway.of(store.decisions()),
      t => counts.getOrElse(t, 0)
    )

  /** The totals of a thread's own recording as the ledger would cut it, without the conversation (so no stretch is told
    * apart as in Zoho by hand).
    */
  private def totalsOf(threadId: String, led: Covered, split: Seq[DayInterval]): Totals =
    Ledger.totals(Ledger.of(
      split.filter(_.threadId == threadId).map(i => Window(i.threadId, i.day, i.start, i.end)),
      led.covering,
      led.struck,
      led.givenAway,
      Map.empty
    ))

  private def account(
    threadId: String,
    ctx: Context,
    totals: Totals,
    submissions: Int,
    recording: Seq[Interval]): ThreadAccount =
    val summary = ctx.summaries.getOrElse(threadId, ThreadSummary(threadId, "", None, 0L, None, deleted = false))
    ThreadAccount(
      threadId = threadId,
      title = summary.title,
      projectId = summary.projectId,
      projectName = ctx.projectNames.getOrElse(summary.projectId, summary.projectId),
      parentThreadId = summary.parentThreadId,
      createdAt = summary.createdAt,
      archivedAt = summary.archivedAt,
      resolved = ctx.resolution.resolved.getOrElse(threadId, Resolved(Billability.NotBillable, None)),
      totals = totals,
      submissions = submissions,
      firstActivity = recording.map(_.start).minOption,
      lastActivity = recording.map(_.end).maxOption
    )

  /** Submissions as the page, the CLI and the agent see them: with the threads' titles. */
  private def records(subs: Vector[Submission]): Future[Vector[SubmissionRecord]] =
    val threads = subs.flatMap(s => s.rootThreadId +: s.spans.map(_.threadId)).distinct
    Future.traverse(threads)(t => threadTitle(t).map(t -> _)).map(_.toMap).map { titles =>
      subs.map(s =>
        SubmissionRecord(
          id = s.id,
          rootThreadId = s.rootThreadId,
          rootTitle = titles.getOrElse(s.rootThreadId, s.rootThreadId),
          spans = s.spans.map(sp =>
            SubmissionSpan(sp.threadId, titles.getOrElse(sp.threadId, sp.threadId), sp.start, sp.end)
          ),
          billed = s.billed,
          target = s.target,
          createdAt = s.createdAt.toLong,
          entries = store.entriesOf(s.id)
        )
      )
    }

  /** Every thread with recorded attention since `since`, with its account over its whole recording. Ancestors without
    * attention appear too, so a thread shows under its parent.
    */
  def overview(since: Long): Future[Either[String, Overview]] =
    val now = js.Date.now().toLong
    calendar().flatMap { (cal, zoneSource) =>
      intervalsOfLiveThreads(since, now).flatMap {
        case Left(error)   => Future.successful(Left(error))
        case Right(recent) =>
          val threadIds = recent.map(_.threadId).distinct
          resolveWithContext(threadIds).flatMap { ctx =>
            // The account counts the whole recording, from before the oldest listed thread began.
            val from = threadIds.flatMap(t => ctx.summaries.get(t).map(_.createdAt - 24 * 3600_000L)).filter(_ > 0)
              .minOption.fold(since)(math.min(_, since))
            intervalsOfLiveThreads(cal.dayStart(from), now).map(_.map { all =>
              val split = Intervals.splitByDay(all, cal)
              val led   = ledgerOf()
              Overview(
                since = since,
                now = now,
                calendar = cal,
                zoneSource = zoneSource,
                threads = ctx.summaries.keys.toVector.map(t =>
                  account(
                    t,
                    ctx,
                    totalsOf(t, led, split),
                    led.submissionsOf(t),
                    all.filter(_.threadId == t)
                  )
                ),
                projectRules = ctx.projectRules,
                warnings = ctx.resolution.warnings
              )
            })
          }
      }
    }

  private val SessionHistoryMs = 30L * 24 * 3600_000L

  /** How many conversations [[warm]] reads at once. */
  private val WarmBatch = 6

  /** Reads ahead what opening a session needs, so the first screens after a start do not wait for it: the session
    * screen's own inputs, then the conversations of the threads of every session with time still to bill, a few at a
    * time. Failures are ignored; a screen reads what it is missing.
    */
  def warm(): Future[Unit] =
    sessionScreen(None).flatMap {
      case Left(_)       => Future.unit
      case Right(screen) =>
        val ids = screen.sessions.filter(s => screen.unbilledMs.getOrElse(s.start, 0L) > 0).flatMap(_.threadIds).distinct
        ids.grouped(WarmBatch).foldLeft(Future.unit)((done, batch) =>
          done.flatMap(_ => Future.traverse(batch)(t => conversationOf(t).recover { case _ => Vector.empty }).map(_ => ()))
        )
    }.recover { case _ => () }

  /** The sessions of the last month over all recorded attention, and the session starting at `at` (else the latest)
    * with every thread's stretches in it. A stretch is a recorded interval cut at local midnight, in the ledger's
    * state, with the summary the ledger would show.
    */
  def sessionScreen(at: Option[Long]): Future[Either[String, SessionScreenResult]] =
    val now = js.Date.now().toLong
    calendar().flatMap { (cal, _) =>
      intervalsOfLiveThreads(now - SessionHistoryMs, now).zip(agentSpansOfLiveThreads(now - SessionHistoryMs, now)).flatMap {
        case (Left(error), _)       => Future.successful(Left(error))
        case (Right(all), agentSpans) =>
          val split    = Intervals.splitByDay(all, cal)
          val windows  = split.map(i => Window(i.threadId, i.day, i.start, i.end))
          val sessions = Sessions.of(windows, agentSpans)
          val allIds = windows.map(_.threadId).distinct
          for
            everyone  <- resolveWithContext(allIds)
            led0       = ledgerOf()
            billable   = (t: String) => billsTo(everyone, t).isDefined
            billing    = allIds.filter(billable)
            // What is still to bill leaves out time in Zoho by hand, so it takes the timer traces of every thread that
            // bills (their conversations are read once and kept for a couple of minutes).
            msgs0     <- Future.traverse(billing)(t => conversationOf(t).map(t -> _)).map(_.toMap)
            traces0   <- timerTraces(everyone, cal, all, billing, windows.map(_.day).distinct.sorted, msgs0)
            unbilledMs = Ledger.of(windows, led0.covering, led0.struck, led0.givenAway, traces0)
                           .filter(l => l.state == StretchState.Unbilled && billable(l.window.threadId))
                           .flatMap(l => Sessions.containing(sessions, l.window).map(_.start -> l.window.durationMs))
                           .groupMapReduce(_._1)(_._2)(_ + _)
            // The session to open: the one asked for, else the oldest with something left to bill, else the latest.
            chosen     = sessions.find(s => at.contains(s.start))
                           .orElse(sessions.find(s => unbilledMs.getOrElse(s.start, 0L) > 0))
                           .orElse(sessions.lastOption)
            result    <- chosen match
                           case None    =>
                             Future.successful(
                               SessionScreenResult(
                                 now, cal, sessions, unbilledMs, None, Vector.empty, Vector.empty, Map.empty, Map.empty, summarizing, Vector.empty
                               )
                             )
                           case Some(s) => sessionDetail(now, cal, all, split, windows, sessions, unbilledMs, traces0, s)
          yield Right(result)
      }
    }

  /** The Zoho timer traces of each thread, as the thread ledger sees them: from the timer calls in its conversation,
    * a timer left running measured by the time Zoho logged for its entry. That entry is dated the day the timer
    * started, which may be before `days`, so the Zoho entries are read from each such day on, for each project the
    * threads bill to. Read only; nothing is written to Zoho.
    */
  private def timerTraces(
    ctx: Context,
    cal: IntlCalendar,
    all: Vector[Interval],
    ids: Vector[String],
    days: Vector[String],
    msgs: Map[String, Vector[Message]]): Future[Map[String, Vector[ZohoTrace]]] =
    val raw      = ids.map(t => t -> ZohoTrace.of(msgs.getOrElse(t, Vector.empty).flatMap(_.call))).toMap
    val handDays = (days ++ raw.values.flatten.collect { case ZohoTrace.TimerLeftRunning(s, _) => cal.dayOf(s) })
      .distinct.sorted
    Future.traverse(ids.flatMap(t => billsTo(ctx, t)).distinct)(z =>
      handEntered(Some(z), handDays).map(r => z -> r._1)
    ).map(_.toMap).map { hands =>
      raw.map { (t, own) =>
        t -> ZohoTrace.lengthened(
          own,
          left =>
            billsTo(ctx, t).flatMap(hands.get).flatMap(
              _.map(_.entry).find(e =>
                e.date == cal.dayOf(left.start) && e.notes.trim.nonEmpty && left.detail.contains(e.notes.trim)
              )
            ).map(_.minutes * 60_000L),
          all.filter(_.threadId == t).map(i => (i.start, i.end)),
          cal.nextDayStart
        )
      }.withDefaultValue(Vector.empty)
    }

  /** Where a thread bills, when it does. */
  private def billsTo(ctx: Context, threadId: String): Option[ZohoTarget] =
    ctx.resolution.resolved.get(threadId).map(_.billability).collect { case Billability.Billable(z) => z }

  /** One session's threads and stretches, with the lines the ledger would show. */
  private def sessionDetail(
    now: Long,
    cal: IntlCalendar,
    all: Vector[Interval],
    split: Vector[DayInterval],
    windows: Vector[Window],
    sessions: Vector[Session],
    unbilledMs: Map[Long, Long],
    knownTraces: Map[String, Vector[ZohoTrace]],
    s: Session): Future[SessionScreenResult] =
    val inSplit = windows.filter(w => Sessions.containing(sessions, w).contains(s))
    val ids     = inSplit.map(_.threadId).distinct
    val days = inSplit.map(_.day).distinct.sorted
    for
      ctx    <- resolveWithContext(ids)
      msgs   <- Future.traverse(ids)(t => conversationOf(t).map(t -> _)).map(_.toMap)
      // The billing threads' traces are known already, read over the whole history; only the others are worked out.
      others  = ids.filterNot(knownTraces.contains)
      traces <- timerTraces(ctx, cal, all, others, days, msgs).map(t => (knownTraces ++ t).withDefaultValue(Vector.empty))
      // The dots are an aid: a recorder that cannot list signals leaves them out rather than failing the screen.
      sigs   <- recorder.signals(s.start, s.end).map(_.getOrElse(Vector.empty))
    yield
      val led       = ledgerOf()
      // Long windows are cut at the user's message times exactly as the thread ledgers cut them, so a stretch here
      // has the key the same stretch has there.
      val inSession = inSplit.flatMap(w =>
        Windows.subdivide(
          w,
          msgs.getOrElse(w.threadId, Vector.empty).filter(_.speaker == Speaker.User).map(_.at),
          WindowMaxMs
        )
      )
      val stretches = Ledger.of(inSession, led.covering, led.struck, led.givenAway, traces)
      val titles    = ctx.summaries.map((t, sm) => t -> sm.title)
      val cached    = summaries.cached(stretches.map(_.window))
      val lines     = stretches.map { st =>
        val w      = st.window
        val inside = msgs.getOrElse(w.threadId, Vector.empty).filter(m => m.at >= w.start && m.at < w.end)
        LedgerLine(
          st,
          cached.getOrElse(w.key, summaries.fallbackWindow(titles.getOrElse(w.threadId, w.threadId), inside)),
          cached.contains(w.key),
          inside
        )
      }
      SessionScreenResult(
        now,
        cal,
        sessions,
        unbilledMs,
        Some(s),
        ctx.summaries.keys.toVector.map(t =>
          account(t, ctx, totalsOf(t, led, split), led.submissionsOf(t), all.filter(_.threadId == t))
        ),
        lines,
        titles,
        store.submissionsOf(None).map(sub => sub.id -> sub.createdAt.toLong).toMap,
        summarizing,
        sigs.filter(g => ids.contains(g.threadId) && g.start >= s.start && g.start <= s.end)
          .map(g => SignalMark(g.threadId, g.kind.name, g.start))
      )

  /** The threads under `root`, every level, archived ones included. */
  private def descendants(root: String): Future[Vector[String]] =
    bb.sdk.threads.asInstanceOf[js.Dynamic]
      .experimental_listDescendants(js.Dynamic.literal(
        "threadIds"       -> js.Array(root),
        "includeArchived" -> true,
        "includeHidden"   -> true
      ))
      .asInstanceOf[js.Promise[js.Dynamic]]
      .toFuture
      .map(_.threads.asInstanceOf[js.Array[js.Dynamic]].toVector.flatMap(t =>
        t.descendantIds.asInstanceOf[js.Array[String]].toVector
      ))
      .recover { case _ => Vector.empty }

  /** The ledger of `root` and, when asked, of the subthreads that bill to the same place. */
  /** A ledger kept between requests: its result and the recording and subthreads it was computed from, and, once it is
    * computed, what it read. One still being computed (`inputs` empty) is dropped by any change, since it may have read
    * the store before it.
    */
  private final case class KeptLedger(
    sources: LedgerSources,
    result: Future[Either[String, LedgerResult]],
    inputs: Option[Inputs])

  /** What a ledger reads from outside this plugin's own store, which announces no changes: the recording, read from
    * `from` on, and the root's subthreads.
    */
  private final case class LedgerSources(from: Long, recording: Vector[Interval], subthreads: Vector[String]):
    /** True when nothing the ledger reading `inputs` used has moved since `earlier`. */
    def sameFor(inputs: Inputs, earlier: LedgerSources): Boolean =
      def of(r: Vector[Interval]) = r.filter(i => inputs.threadIds(i.threadId))
      subthreads == earlier.subthreads && of(recording) == of(earlier.recording)

  private val keptLedgers = mutable.Map.empty[(String, Boolean), KeptLedger]

  /** Drops every kept ledger the changes touch. */
  private def dropAffected(batch: Vector[Change]): Unit =
    keptLedgers.filterInPlace((_, k) => k.inputs.exists(i => !i.affectedByAny(batch)))

  /** What a ledger read: its threads, those it shares stretches with, and the Zoho project it bills to. */
  private def inputsOf(l: LedgerResult): Inputs =
    Inputs(
      (l.root.threadId +: (l.coveredIds ++ l.leftOut.map(_._1.threadId) ++ l.blocks.flatMap(_._1.threadIds))).toSet,
      l.target.map(_.projectId).toSet
    )

  /** The recording from `from` on and the root's subthreads, as a ledger would read them now. */
  private def ledgerSources(root: String, subthreads: Boolean, from: Long): Future[Either[String, LedgerSources]] =
    for
      (cal, _) <- calendar()
      subs     <- if subthreads then descendants(root) else Future.successful(Vector.empty)
      read     <- recordedIntervals(from, cal.nextDayStart(js.Date.now().toLong))
    yield read.map(LedgerSources(from, _, subs.sorted))

  /** Where a ledger of `root` reads the recording from: the day before its oldest covered thread began. */
  private def ledgerFrom(l: LedgerResult): Long =
    l.calendar.dayStart(l.covered.map(_.createdAt - 24 * 3600_000L).minOption.getOrElse(0L))

  /** The thread's ledger: the one kept, while no change has touched what it read and its recording and subthreads are
    * as they were, else computed again and kept. A kept one also lets Zoho be read again behind it, so entries made in
    * Zoho by hand are noticed (see [[zohoEntries]]).
    */
  def ledger(root: String, subthreads: Boolean): Future[Either[String, LedgerResult]] =
    val key = (root, subthreads)
    keptLedgers.get(key) match
      case Some(k @ KeptLedger(_, result, Some(inputs))) =>
        result.value.flatMap(_.toOption).flatMap(_.toOption) match
          case Some(l) =>
            ledgerSources(root, subthreads, k.sources.from).flatMap {
              case Right(now) if now.sameFor(inputs, k.sources) =>
                val _ = handEntered(l.target, l.stretches.map(_.stretch.window.day).distinct)
                result
              case _                                            => computeAndKeep(key)
            }
          case None    => computeAndKeep(key)
      case Some(computing)                               => computing.result
      case None                                          => computeAndKeep(key)

  private def computeAndKeep(key: (String, Boolean)): Future[Either[String, LedgerResult]] =
    val (root, subthreads) = key
    // The sources are read before the ledger, so a kept ledger is never newer than what it is checked against.
    val computed = computeLedger(root, subthreads).flatMap {
      case Left(e)  => Future.successful((Left(e), None))
      case Right(l) => ledgerSources(root, subthreads, ledgerFrom(l)).map(s => (Right(l), s.toOption))
    }
    val result   = computed.map(_._1)
    val entry    = KeptLedger(LedgerSources(0L, Vector.empty, Vector.empty), result, None)
    keptLedgers.update(key, entry)
    computed.onComplete { done =>
      if keptLedgers.get(key).exists(_ eq entry) then
        done.toOption match
          case Some((Right(l), Some(sources))) => keptLedgers.update(key, KeptLedger(sources, result, Some(inputsOf(l))))
          case _                               => keptLedgers -= key
    }
    result

  private def computeLedger(root: String, subthreads: Boolean): Future[Either[String, LedgerResult]] =
    for
      (cal, _) <- calendar()
      subs     <- if subthreads then descendants(root) else Future.successful(Vector.empty)
      candidates = root +: subs
      ctx <- resolveWithContext(candidates)
      led        = ledgerOf()
      rootRes    = ctx.resolution.resolved.getOrElse(root, Resolved(Billability.NotBillable, None))
      target     = rootRes.billability match
                     case Billability.Billable(t) => Some(t)
                     case Billability.NotBillable => None
      coveredIds = candidates.filter { t =>
                     ctx.resolution.resolved.get(t).map(_.billability) == target.map(Billability.Billable.apply)
                   }
      // The recording is read from before the oldest covered thread began up to now, on whole local days: stretches
      // and shared-stretch keys are cut on the recording itself, so a submission's edge or a cut inside a day never
      // moves a boundary, and cached lines and decisions keep matching.
      from = coveredIds.flatMap(t => ctx.summaries.get(t).map(_.createdAt - 24 * 3600_000L)).minOption.getOrElse(0L)
      intervals <- recordedIntervals(cal.dayStart(from), cal.nextDayStart(js.Date.now().toLong))
      result    <- intervals match
                     case Left(error) => Future.successful(Left(error))
                     case Right(raw)  =>
                       val split                              = Intervals.splitByDay(raw, cal)
                       val billability: String => Billability =
                         t => ctx.resolution.resolved.get(t).map(_.billability).getOrElse(Billability.NotBillable)
                       Future.traverse(coveredIds)(t => conversationOf(t).map(t -> _)).map(_.toMap).flatMap { messages =>
                         // Long windows are cut at the user's message times, so each line is one stretch of work.
                         val withSplit = split.flatMap { i =>
                           if coveredIds.contains(i.threadId) then
                             Windows.subdivide(
                               Window(i.threadId, i.day, i.start, i.end),
                               messages.getOrElse(i.threadId, Vector.empty).filter(_.speaker == Speaker.User).map(_.at),
                               WindowMaxMs
                             ).map(w => DayInterval(w.day, w.threadId, w.start, w.end))
                           else Vector(i)
                         }
                         val windows   = withSplit.filter(i => coveredIds.contains(i.threadId)).map(i =>
                           Window(i.threadId, i.day, i.start, i.end)
                         )
                         val days      = windows.map(_.day).distinct.sorted
                         val rawTraces =
                           messages.map((t, ms) => t -> ZohoTrace.of(ms.flatMap(_.call))).withDefaultValue(Vector.empty)
                         for
                           hand <- handEntered(target, days)
                           // A timer the thread started and never stopped ran over the thread's recorded activity for
                           // as long as Zoho's entry for it logs: logged time, not clock time, since it may have paused.
                           traces    = rawTraces.map((t, trs) =>
                                         t -> ZohoTrace.lengthened(
                                           trs,
                                           left =>
                                             hand._1.map(_.entry).find(e =>
                                               e.date == cal.dayOf(left.start) && e.notes.trim.nonEmpty &&
                                                 left.detail.contains(e.notes.trim)
                                             ).map(_.minutes * 60_000L),
                                           raw.filter(_.threadId == t).map(i => (i.start, i.end)),
                                           cal.nextDayStart
                                         )
                                       ).withDefaultValue(Vector.empty)
                           stretches = Ledger.of(windows, led.covering, led.struck, led.givenAway, traces)
                           // Shared stretches that touch anything a draft could hold or a decision gave away, so every
                           // decision on the thread's open time can be seen and changed.
                           open      = stretches.filter(l =>
                                         l.state.draftable || l.state.isInstanceOf[StretchState.GivenAway]
                                       ).map(_.window)
                           // Shared stretches are cut on the recording as recorded, not as this ledger cut its own
                           // threads' long windows, so every ledger holding a stretch gives it the same key.
                           shared    = Submissions.plan(coveredIds, open, split, store.decisions(), billability)
                           blocks    = shared.undecided.map(_ -> None) ++ shared.decided.map((b, a) => b -> Some(a))
                           others    = blocks.map(_._1).flatMap(_.threadIds).distinct.filterNot(coveredIds.contains)
                           otherSummaries     <- Future.traverse(others)(t => threadSummary(t).map(t -> _)).map(_.toMap)
                           // Whether the other threads of the shared stretches are client work: a stretch shared with
                           // another client thread has two client threads, not one.
                           othersCtx          <- resolveWithContext(others)
                           everyBillability   = (t: String) =>
                                                  ctx.resolution.resolved.get(t).orElse(othersCtx.resolution.resolved.get(t))
                                                    .map(_.billability).getOrElse(Billability.NotBillable)
                           otherConversations <- Future.traverse(others)(t => conversationOf(t).map(t -> _)).map(_.toMap)
                           everyConversation = messages ++ otherConversations
                           titles            = (ctx.summaries ++ otherSummaries).map((t, s) => t -> s.title)
                           blockThreads      = blocks.map { (b, _) =>
                                                 b.key.text -> b.threadIds.toVector.sorted.map(t =>
                                                   BlockThread(
                                                     t,
                                                     titles.getOrElse(t, t),
                                                     (ctx.summaries ++ otherSummaries).get(t).map(_.projectId)
                                                       .flatMap(ctx.projectNames.get).getOrElse(""),
                                                     everyConversation.getOrElse(t, Vector.empty)
                                                       .filter(m => m.at >= b.start && m.at < b.end)
                                                   )
                                                 )
                                               }.toMap
                           cached            = summaries.cached(stretches.map(_.window))
                           lines             = stretches.map { st =>
                                                 val w      = st.window
                                                 val inside = messages.getOrElse(w.threadId, Vector.empty)
                                                   .filter(m => m.at >= w.start && m.at < w.end)
                                                 LedgerLine(
                                                   st,
                                                   cached.getOrElse(
                                                     w.key,
                                                     summaries.fallbackWindow(titles.getOrElse(w.threadId, w.threadId), inside)
                                                   ),
                                                   cached.contains(w.key),
                                                   inside
                                                 )
                                               }
                           available <- summaries.available()
                           history <- records(coveredIds.flatMap(t => store.submissionsOf(Some(t))).distinctBy(_.id)
                                        .sortBy(-_.createdAt))
                           // A covered thread's totals come from the ledger's own stretches (so in-Zoho time is told
                           // apart); any other thread's from its raw recording.
                           accountOf = (t: String) =>
                                         account(
                                           t,
                                           ctx,
                                           if coveredIds.contains(t) then
                                             Ledger.totals(stretches.filter(_.window.threadId == t))
                                           else totalsOf(t, led, split),
                                           led.submissionsOf(t),
                                           raw.filter(_.threadId == t)
                                         )
                         yield Right(
                           LedgerResult(
                             root = accountOf(root),
                             covered = coveredIds.map(accountOf),
                             leftOut = subs.filterNot(coveredIds.contains).map(t =>
                               accountOf(t) -> (ctx.resolution.resolved.get(t).map(_.billability) match
                                 case Some(Billability.Billable(z)) =>
                                   s"bills to ${z.customerName}: ${z.projectName} / ${z.taskName}"
                                 case _                             => "not client work"
                               )
                             ),
                             target = target,
                             calendar = cal,
                             stretches = lines,
                             blocks = blocks,
                             blockThreads = blockThreads,
                             history = history,
                             handEntered = hand._1,
                             handEnteredError = hand._2,
                             summariesAvailable = available,
                             titles = titles,
                             recording = split,
                             billability = everyBillability,
                             projectId = ctx.summaries.get(root).map(_.projectId).getOrElse("")
                           )
                         )
                       }
    yield result

  /** The lines of a draft: the ledger's draftable stretches whose keys are ticked. */
  private def draftLines(l: LedgerResult, keys: Set[String]): Vector[LedgerLine] =
    l.stretches.filter(ln => ln.stretch.state.draftable && keys(ln.key))

  /** The stretches of a draft as the summarizer sees them, and the days it would write notes for. */
  private def toDescribe(l: LedgerResult, keys: Set[String]): (Vector[Stretch], Vector[summaries.DayToNote]) =
    val chosen = draftLines(l, keys)
    val p = Submissions.plan(l.coveredIds, chosen.map(_.stretch.window), l.recording, store.decisions(), l.billability)
    val stretches =
      chosen.map(ln => Stretch(ln.stretch.window, l.titles.getOrElse(ln.stretch.window.threadId, ""), ln.messages))
    (
      stretches,
      p.totalsByDay.keys.toVector.sorted.map(day => summaries.DayToNote(day, stretches.filter(_.window.day == day)))
    )

  /** One model run that writes the missing lines of the draft `select` picks on the ledger and the notes of its days.
    */
  def summarize(root: String, subthreads: Boolean, pick: DraftPick): Future[Either[String, Unit]] =
    ledger(root, subthreads).flatMap {
      case Left(e)  => Future.successful(Left(e))
      case Right(l) =>
        val (stretches, days) = toDescribe(l, pick.keysOn(l.stretches.map(_.stretch)))
        summaries.describe(l.projectId, stretches, days, Text.stamp(l.calendar))
          .map { _ =>
            changes(Change.Summarized(stretches.map(_.window.threadId).toSet + root))
            Right(())
          }
          .recover { case e => Left(errorMessage(e)) }
    }

  private var summarizing = Set.empty[String]

  /** Starts `summarize` for the page and answers at once; `BillingEvent` on the realtime channel says how it ended. A
    * run already going for the thread is left to finish.
    */
  def startSummaries(root: String, subthreads: Boolean, pick: DraftPick): Unit =
    if !summarizing(root) then
      summarizing += root
      summarize(root, subthreads, pick).map(_.fold(
        e => BillingEvent.SummariesFailed(root, e),
        _ => BillingEvent.SummariesWritten(root)
      ))
        .recover { case e => BillingEvent.SummariesFailed(root, errorMessage(e)) }
        .foreach { event =>
          summarizing -= root
          publish(event)
        }

  def publish(event: BillingEvent): Unit = bb.realtime.publish("billing", WireJs.toJs(event.encode))

  /** Prices a draft on the ledger: totals per day, the entries with their notes, and what is left for later. */
  def draft(l: LedgerResult, keys: Set[String]): Future[PlanResult] =
    val root   = l.root.threadId
    val chosen = draftLines(l, keys)
    val p = Submissions.plan(l.coveredIds, chosen.map(_.stretch.window), l.recording, store.decisions(), l.billability)
    val cached  = summaries.cached(chosen.map(_.stretch.window))
    val line    = (ln: LedgerLine) =>
      WindowLine(ln.stretch.window, cached.getOrElse(ln.key, ln.summary), cached.contains(ln.key), ln.messages)
    val lineVec = chosen.map(line)
    for
      increment <- incrementMinutes()
      overrides = store.notesOverrides(root)
      rootTitle = l.titles.getOrElse(root, root)
      entries   = p.totalsByDay.toVector.sortBy(_._1).map { (day, ms) =>
                    overrides.get(day) match
                      case Some(text) =>
                        EntryLine(
                          day,
                          ms,
                          ZohoEntries.billedMinutes(ms, increment),
                          text,
                          notesEdited = true,
                          notesFromModel = false
                        )
                      case None       =>
                        val dayLines  = lineVec.filter(_.window.day == day)
                        // The model's paragraph for exactly these lines, else the lines joined.
                        val fromModel = Option.when(dayLines.forall(_.fromModel))(
                          summaries.dayKey(day, dayLines.map(l => l.window.key -> l.summary))
                        ).flatMap(store.dayNotes)
                        val joined    = dayLines.map(_.summary).distinct match
                          case Vector() => rootTitle
                          case ls       => ls.mkString("; ").take(500)
                        EntryLine(
                          day,
                          ms,
                          ZohoEntries.billedMinutes(ms, increment),
                          fromModel.getOrElse(joined),
                          notesEdited = false,
                          notesFromModel = fromModel.isDefined
                        )
                  }
    yield PlanResult(
      root = l.root,
      covered = l.covered,
      leftOut = l.leftOut,
      target = l.target,
      calendar = l.calendar,
      plan = p,
      lines = lineVec,
      entries = entries,
      handEntered = l.handEntered.filter(h => entries.exists(_.day == h.entry.date)),
      handEnteredError = l.handEnteredError,
      zohoTraces = l.stretches.flatMap(ln =>
        ln.stretch.state match
          case StretchState.InZohoByHand(t) if entries.exists(e => traceOnDay(t, e.day, l.calendar)) =>
            Some(ln.stretch.window.threadId -> t)
          case _                                                                                     => None
      ).distinct,
      blockThreads = l.blockThreads,
      summariesAvailable = l.summariesAvailable,
      later = l.stretches.filter(ln => ln.stretch.state.draftable && !keys(ln.key)).map(line),
      history = l.history,
      titles = l.titles
    )

  /** The priced draft `pick` names on the thread's ledger. */
  def planOf(root: String, subthreads: Boolean, pick: DraftPick): Future[Either[String, PlanResult]] =
    ledger(root, subthreads).flatMap {
      case Left(e)  => Future.successful(Left(e))
      case Right(l) => draft(l, pick.keysOn(l.stretches.map(_.stretch))).map(Right(_))
    }

  /** True when the trace starts, ends, or was made on local day `day`. */
  private def traceOnDay(t: ZohoTrace, day: String, cal: Calendar): Boolean =
    t match
      case ZohoTrace.TimerRan(s, e, _)      => cal.dayOf(s) == day || cal.dayOf(e) == day
      case ZohoTrace.TimerLeftRunning(s, _) => cal.dayOf(s) == day

  /** For one past submission: what the sent range already had in Zoho from the threads' own timer calls, and the
    * pre-existing Zoho entries on the sent days, so a double billing can be seen.
    */
  final case class Audit(
    submission: SubmissionRecord,
    calendar: Calendar,
    titles: Map[String, String],
    /** Traces inside the spans the submission covers. */
    insideSentRange: Vector[(String, ZohoTrace)],
    /** Zoho entries on the sent days that this plugin did not create. */
    preExisting: Vector[ZohoExistingEntry],
    preExistingError: Option[String])

  def audit(submissionId: String): Future[Either[String, Audit]] =
    store.submission(submissionId) match
      case None    => Future.successful(Left(s"no submission $submissionId"))
      case Some(s) =>
        val entries = store.entriesOf(s.id)
        for
          (cal, _) <- calendar()
          threads = s.spans.map(_.threadId).distinct
          titles <- Future.traverse(threads)(t => threadTitle(t).map(t -> _)).map(_.toMap)
          convs  <- Future.traverse(threads)(t => conversation.of(t).map(t -> _))
          inside = convs.flatMap { (t, ms) =>
                     val mine = s.spans.filter(_.threadId == t)
                     ZohoTrace.of(ms.flatMap(_.call)).filter(tr => mine.exists(sp => tr.touches(sp.start, sp.end)))
                       .map(t -> _)
                   }
          days   = entries.map(_.day)
          existing <- (days.minOption, days.maxOption) match
                        case (Some(from), Some(to)) if s.target.projectId.nonEmpty =>
                          zoho.clientOrFail().flatMap(_.timeEntries(from, to, s.target.projectId)).map { es =>
                            val mine = store.allEntryIds()
                            (
                              es.filterNot(e => mine(e.timeEntryId)).filter(e => days.contains(e.date)),
                              Option.empty[String]
                            )
                          }.recover { case e => (Vector.empty, Some(errorMessage(e))) }
                        case _ => Future.successful((Vector.empty, None))
          record   <- records(Vector(s)).map(_.head)
        yield Right(Audit(record, cal, titles, inside, existing._1, existing._2))

  /** How long a read of a project's Zoho entries answers without being read again. Entries made in Zoho by hand, the
    * only ones a read is for, are rare; every read counts against Zoho's daily limit on API calls.
    */
  private val ZohoEntriesFreshMs = 5 * 60_000.0

  /** One read of a Zoho project's time entries, over the days `from` to `to`, made at `at`. */
  private final case class EntriesRead(from: String, to: String, at: Double, entries: Future[Vector[ZohoExistingEntry]]):
    def covers(f: String, t: String): Boolean = from <= f && t <= to

  /** The last read of each Zoho project's entries, by project. One read serves every caller whose days fall inside it,
    * whatever thread or session asks; a caller reaching outside it reads again over both spans together.
    */
  private val zohoEntryReads   = mutable.Map.empty[String, EntriesRead]
  /** The projects read again behind an answer already given. */
  private val zohoEntryRereads = mutable.Set.empty[String]

  /** A Zoho project's time entries from one day to another. A read answers at once from the last one that covers the
    * days: within [[ZohoEntriesFreshMs]] as it is, later while Zoho is read again behind it. When that read finds
    * entries made outside this plugin that differ, that is announced as [[Change.ZohoHandEntries]]; entries this plugin
    * made never are, as everything that shows them already knows of them. `fresh` reads Zoho now, whatever was read.
    */
  private def zohoEntries(
    projectId: String,
    from: String,
    to: String,
    fresh: Boolean = false): Future[Vector[ZohoExistingEntry]] =
    def read(f: String, t: String): EntriesRead =
      val r = EntriesRead(f, t, js.Date.now(), zoho.clientOrFail().flatMap(_.timeEntries(f, t, projectId)))
      zohoEntryReads.update(projectId, r)
      r.entries.failed.foreach(_ => if zohoEntryReads.get(projectId).contains(r) then zohoEntryReads -= projectId)
      r
    def notMine(es: Vector[ZohoExistingEntry]): Set[ZohoExistingEntry] =
      val mine = store.allEntryIds()
      es.filterNot(e => mine(e.timeEntryId)).toSet
    def announceIfDiffers(previous: Vector[ZohoExistingEntry], again: EntriesRead): Unit =
      again.entries.foreach(now =>
        if notMine(now) != notMine(previous) then changes(Change.ZohoHandEntries(projectId))
      )
    val answer = zohoEntryReads.get(projectId) match
      case Some(r) if fresh                                                           =>
        val previous = r.entries.value.flatMap(_.toOption)
        val again    = read(Ordering[String].min(from, r.from), Ordering[String].max(to, r.to))
        previous.foreach(announceIfDiffers(_, again))
        again
      case Some(r) if r.covers(from, to) && js.Date.now() - r.at < ZohoEntriesFreshMs => r
      // The last read is done: answer with it, and read again behind it.
      case Some(r) if r.covers(from, to) && r.entries.value.exists(_.isSuccess)       =>
        val previous = r.entries.value.get.get
        if zohoEntryRereads.add(projectId) then
          val again = read(r.from, r.to)
          again.entries.onComplete(_ => zohoEntryRereads -= projectId)
          announceIfDiffers(previous, again)
        r
      case Some(r) if r.covers(from, to)                                              => r
      case Some(r)                                                                    =>
        read(Ordering[String].min(from, r.from), Ordering[String].max(to, r.to))
      case None                                                                       => read(from, to)
    answer.entries.map(_.filter(e => from <= e.date && e.date <= to))

  private def handEntered(
    target: Option[ZohoTarget],
    days: Seq[String]): Future[(Vector[HandEntered], Option[String])] =
    (target, days.minOption, days.maxOption) match
      case (Some(t), Some(from), Some(to)) =>
        zohoEntries(t.projectId, from, to).map { entries =>
          val mine = store.allEntryIds()
          (entries.filterNot(e => mine(e.timeEntryId)).filter(e => days.contains(e.date)).map(HandEntered.apply), None)
        }.recover { case e => (Vector.empty, Some(errorMessage(e))) }
      case _                               => Future.successful((Vector.empty, None))

  def setExcluded(windowKey: String, excluded: Boolean): Unit =
    windowKey.split('|').headOption.foreach { threadId =>
      store.setExcluded(windowKey, threadId, excluded)
      changes(Change.LeftOut(threadId))
    }

  def setNotes(root: String, day: String, notes: Option[String]): Unit =
    store.setNotesOverride(root, day, notes)
    changes(Change.NotesEdited(root))

  /** Decides a shared stretch of the thread's ledger. */
  def decide(key: BlockKey, allocation: Allocation, root: String, subthreads: Boolean): Future[Either[String, Unit]] =
    ledger(root, subthreads).map(_.flatMap { l =>
      l.blocks.map(_._1).find(_.key == key) match
        case None        => Left("that stretch is not part of this thread's ledger; reload")
        case Some(block) =>
          Allocation.validate(allocation, block).map { valid =>
            // Pieces decided before the stretch was cut whole are replaced by the decision on the whole.
            Decisions.piecesOf(key, store.decisions().keys).foreach(store.deleteDecision)
            store.saveDecision(key, valid)
            changes(Change.Decided(key))
          }
    })

  /** Takes back the stretch's decision, made on it or on pieces of it (see [[Decisions.resolve]]). */
  def undecide(key: BlockKey): Unit =
    (key +: Decisions.piecesOf(key, store.decisions().keys)).foreach(store.deleteDecision)
    changes(Change.Decided(key))

  /** The shared stretches of the ledger reaching into `period`, decided or not. */
  private def sharedWithin(l: LedgerResult, period: Option[Period]): Vector[(OverlapBlock, Option[Allocation])] =
    l.blocks.filter((b, _) => period.forall(p => b.end > p.start && b.start < p.end))

  /** The undecided shared stretches of the ledger reaching into `period`. */
  private def undecidedWithin(l: LedgerResult, period: Option[Period]): Vector[OverlapBlock] =
    sharedWithin(l, period).collect { case (b, None) => b }

  /** The ledger of each of `roots` with its subthreads, and its shared stretches reaching into `period`, decided or
    * not. The stretches come from each root's own ledger, so a decision on one is one that ledger accepts.
    */
  def sessionShared(
    roots: Vector[String],
    period: Period): Future[Either[String, Vector[(LedgerResult, Vector[(OverlapBlock, Option[Allocation])])]]] =
    val distinct = roots.distinct
    for
      (cal, _) <- calendar()
      created  <- Future.traverse(distinct)(threadSummary).map(_.map(_.createdAt))
      // One read of the recording as far back as the oldest root's ledger reaches serves every ledger.
      _        <- created.minOption.fold(Future.unit)(oldest =>
                    recordedIntervals(cal.dayStart(oldest - 24 * 3600_000L), cal.nextDayStart(js.Date.now().toLong))
                      .map(_ => ())
                  )
      ledgers  <- Future.traverse(distinct)(root => ledger(root, subthreads = true))
    yield ledgers.foldLeft(
      Right(Vector.empty): Either[String, Vector[(LedgerResult, Vector[(OverlapBlock, Option[Allocation])])]]
    ) { (acc, l) =>
      for done <- acc; one <- l yield done :+ (one -> sharedWithin(one, Some(period)))
    }

  /** Every undecided shared stretch of the ledger with exactly one client thread goes to that thread: of those reaching
    * into `within`, when given.
    */
  def decideSoleBillable(root: String, subthreads: Boolean, within: Option[Period]): Future[Either[String, Int]] =
    ledger(root, subthreads).map(_.map { l =>
      val decisions = Submissions.soleBillableDecisions(undecidedWithin(l, within), l.billability)
      decisions.foreach { d =>
        Decisions.piecesOf(d.key, store.decisions().keys).foreach(store.deleteDecision)
        store.saveDecision(d.key, d.allocation)
      }
      changes(decisions.map(d => Change.Decided(d.key))*)
      decisions.size
    })

  final case class SubmitOutcome(submission: Submission, entries: Vector[SubmissionEntry]):
    def record(titles: Map[String, String]): SubmissionRecord =
      SubmissionRecord(
        submission.id,
        submission.rootThreadId,
        titles.getOrElse(submission.rootThreadId, submission.rootThreadId),
        submission.spans.map(sp =>
          SubmissionSpan(sp.threadId, titles.getOrElse(sp.threadId, sp.threadId), sp.start, sp.end)
        ),
        submission.billed,
        submission.target,
        submission.createdAt.toLong,
        entries
      )

  /** Blockers that stop a plan from being sent. */
  def blockers(p: PlanResult): Vector[String] =
    Vector(
      Option.when(p.target.isEmpty)("the thread is not client work; choose where it bills first"),
      Option.when(p.plan.undecided.nonEmpty)(s"${p.plan.undecided.size} shared stretch(es) still need a decision"),
      Option.when(p.entries.isEmpty)("no time to send: every window is left out or nothing was recorded")
    ).flatten

  /** Sends the draft `pick` names (or records it as settled without billing). The submission records the spans of the
    * stretches it covers.
    */
  def submit(
    root: String,
    subthreads: Boolean,
    pick: DraftPick,
    billed: Boolean): Future[Either[String, SubmitOutcome]] = submitPlan(root, planOf(root, subthreads, pick), billed)

  /** Sends a priced draft as previewed: cached lines or first messages, never a fresh model run. */
  private def submitPlan(
    root: String,
    planned: Future[Either[String, PlanResult]],
    billed: Boolean): Future[Either[String, SubmitOutcome]] =
    planned.flatMap {
      case Left(error)                                => Future.successful(Left(error))
      case Right(p) if billed && blockers(p).nonEmpty => Future.successful(Left(blockers(p).mkString("; ")))
      case Right(p) if p.covered.isEmpty              => Future.successful(Left("nothing to cover"))
      case Right(p) if billed                         =>
        unseenHandEntries(p).flatMap {
          case Some(why) => Future.successful(Left(why))
          case None      => record(root, p, billed)
        }
      case Right(p)                                   => record(root, p, billed)
    }

  /** Entries made in Zoho by hand on the plan's days that the plan did not show, read from Zoho now: what was shown may
    * be minutes old (see [[ZohoEntriesFreshMs]]). Some(the reason not to send) when there are any.
    */
  private def unseenHandEntries(p: PlanResult): Future[Option[String]] =
    val days = p.entries.map(_.day)
    (p.target, days.minOption, days.maxOption) match
      case (Some(t), Some(from), Some(to)) =>
        zohoEntries(t.projectId, from, to, fresh = true).map { entries =>
          val mine   = store.allEntryIds()
          val shown  = p.handEntered.map(_.entry.timeEntryId).toSet
          val unseen = entries.filter(e => days.contains(e.date) && !mine(e.timeEntryId) && !shown(e.timeEntryId))
          Option.when(unseen.nonEmpty)(
            s"Zoho has entries on these days made outside bb that the review did not show (" +
              unseen.map(e => s"${e.date} ${e.logTime} ${e.taskName}").mkString(", ") +
              "); nothing was sent. Look at it again before sending."
          )
        }
      case _                               => Future.successful(None)

  /** Records the plan as a submission, sending its entries to Zoho when `billed`. */
  private def record(root: String, p: PlanResult, billed: Boolean): Future[Either[String, SubmitOutcome]] =
        val id                                    = s"sub-${js.Date.now().toLong}-${root.takeRight(6)}"
        val submission                            = Submission(
          id = id,
          rootThreadId = root,
          spans = p.plan.windows.map(w => Span(w.threadId, w.start, w.end)),
          billed = billed,
          target = p.target.getOrElse(ZohoTarget("", "", "", "", "", "")),
          createdAt = js.Date.now()
        )
        val send: Future[Vector[SubmissionEntry]] =
          if !billed then Future.successful(Vector.empty)
          else
            for
              client <- zoho.clientOrFail()
              userId <- zoho.userId()
              target = p.target.get
              entries <- p.entries.foldLeft(Future.successful(Vector.empty[SubmissionEntry])) { (acc, e) =>
                           acc.flatMap { done =>
                             if done.isEmpty then store.recordSubmission(submission)
                             client
                               .createTimeEntry(ZohoTimeEntryBody(
                                 target.projectId,
                                 target.taskId,
                                 userId,
                                 e.day,
                                 ZohoEntries.logTime(e.billedMinutes),
                                 ZohoEntries.notes(e.notes)
                               ))
                               .map { zohoId =>
                                 val entry =
                                   SubmissionEntry(e.day, zohoId, e.billedMinutes, ZohoEntries.notes(e.notes))
                                 store.recordEntry(id, entry)
                                 bb.log.info(s"created Zoho time entry $zohoId: ${e.day} ${ZohoEntries.logTime(
                                     e.billedMinutes
                                   )} ${target.projectName} / ${target.taskName} for thread $root")
                                 done :+ entry
                               }
                           }
                         }
            yield entries
        def finish(entries: Vector[SubmissionEntry]): SubmitOutcome =
          if !billed || entries.isEmpty then store.recordSubmission(submission)
          store.clearNotesOverrides(root)
          changes(Change.Submitted(submission.spans.map(_.threadId).toSet + root), Change.NotesEdited(root))
          SubmitOutcome(submission, entries)
        send.map(entries => Right(finish(entries))).recoverWith { case e =>
          // A failure after some entries reached Zoho: those are in Zoho and recorded here, so the submission stands with
          // its spans (sending again would bill them twice) and the error names the days that did not go.
          val sent = store.entriesOf(id)
          if sent.isEmpty then Future.failed(e)
          else
            val _       = finish(sent)
            val missing = p.entries.map(_.day).filterNot(d => sent.exists(_.day == d))
            Future.successful(Left(
              s"Sent ${sent.map(e => s"${e.day} ${Durations.hoursMinutes(e.minutes * 60_000)}").mkString(
                  ", "
                )} to Zoho, then failed on ${missing.mkString(", ")}: ${errorMessage(e)}. " +
                s"The submission stands for what was sent; undo submission $id to take it back, or enter the missing days in Zoho by hand."
            ))
        }

  /** Deletes a submission: its Zoho entries go, and the stretches it covered are unbilled again. Any submission, in any
    * order, since billed is only ever what the remaining submissions cover.
    */
  def undo(submissionId: String): Future[Either[String, Int]] =
    store.submission(submissionId) match
      case None    => Future.successful(Left(s"no submission $submissionId"))
      case Some(s) =>
        val entries   = store.entriesOf(s.id)
        val deletions =
          if entries.isEmpty then Future.successful(0)
          else
            zoho.clientOrFail().flatMap(client =>
              Future.traverse(entries)(e => client.deleteTimeEntry(e.timeEntryId)).map(_.size)
            )
        deletions.map { n =>
          store.deleteSubmission(s.id)
          changes(Change.Submitted(s.spans.map(_.threadId).toSet + s.rootThreadId))
          Right(n)
        }

  def submissions(threadId: Option[String]): Future[Vector[SubmissionRecord]] = records(store.submissionsOf(threadId))

  final case class ReportLine(
    threadId: String,
    title: String,
    minutes: Long,
    days: Vector[String],
    project: String,
    task: String)
  final case class Report(
    customerId: String,
    customerName: String,
    from: String,
    to: String,
    lines: Vector[ReportLine]):
    def totalMinutes: Long = lines.map(_.minutes).sum
    def markdown: String   =
      val header = s"# $customerName: $from to $to\n\nTotal: ${Durations.hoursMinutes(totalMinutes * 60_000)}\n"
      val rows   = lines.map(l =>
        s"- ${l.title} (${l.project} / ${l.task}): ${Durations.hoursMinutes(l.minutes * 60_000)} on ${l.days.mkString(", ")}"
      )
      (header +: rows).mkString("\n")

  /** What was sent for a customer in a day range, one line per thread under each project and task. */
  def report(customerId: String, from: String, to: String): Future[Report] =
    val rows   = store.entriesBetween(customerId, from, to)
    val groups = rows.groupBy((s, _) => (s.rootThreadId, s.target.projectId, s.target.taskId)).toVector
    Future
      .traverse(groups) { case ((threadId, _, _), es) =>
        threadTitle(threadId).map { title =>
          ReportLine(
            threadId,
            title,
            es.map(_._2.minutes).sum,
            es.map(_._2.day).distinct.sorted,
            es.head._1.target.projectName,
            es.head._1.target.taskName
          )
        }
      }
      .map(lines =>
        Report(
          customerId,
          rows.headOption.map(_._1.target.customerName).getOrElse(customerId),
          from,
          to,
          lines.sortBy(l => (l.days.headOption, l.title))
        )
      )
