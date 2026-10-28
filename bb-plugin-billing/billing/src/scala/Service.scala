package billing

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
  environmentId: Option[String])

/** The "To bill" view: every thread with recorded attention since `since`, resolved, with what is unbilled. */
final case class Overview(
  since: Long,
  now: Long,
  calendar: IntlCalendar,
  zoneSource: ZoneSource,
  threads: Vector[ThreadAccount],
  projectDefaults: Map[String, ZohoTarget],
  warnings: Vector[String])

/** The sessions screen: the recent sessions, and for one of them every thread's stretches in their ledger state. */
final case class SessionScreenResult(
  now: Long,
  calendar: IntlCalendar,
  sessions: Vector[Session],
  chosen: Option[Session],
  threads: Vector[ThreadAccount],
  stretches: Vector[LedgerLine],
  titles: Map[String, String])

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
  projectId: String,
  environmentId: Option[String]):
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
  conversation: Conversation
)(using ExecutionContext):
  private val cache = mutable.Map.empty[String, ThreadSummary]

  /** A thread's conversation is read from bb's timeline once and kept for a couple of minutes, so that ticking a
    * stretch or striking one out does not refetch every covered thread.
    */
  private val conversations     = mutable.Map.empty[String, (Double, Future[Vector[Message]])]
  private val ConversationTtlMs = 120_000.0
  private def conversationOf(threadId: String): Future[Vector[Message]] =
    val now = js.Date.now()
    conversations.get(threadId).filter((at, _) => now - at < ConversationTtlMs) match
      case Some((_, f)) => f
      case None         =>
        val f = conversation.of(threadId)
        conversations.update(threadId, (now, f))
        f.failed.foreach(_ => conversations.remove(threadId))
        f

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

  /** A thread's title, project, parent, and dates, cached for the plugin's lifetime once it has a title and is archived
    * (an open thread's title and archive state can still change).
    */
  def threadSummary(threadId: String): Future[ThreadSummary] =
    cache.get(threadId) match
      case Some(t) => Future.successful(t)
      case None    =>
        bb.sdk.threads
          .get(js.Dynamic.literal("threadId" -> threadId))
          .toFuture
          .map { thread =>
            val title   =
              Option(str(thread.title)).filter(_.nonEmpty).orElse(Option(str(thread.titleFallback)).filter(_.nonEmpty))
            val summary = ThreadSummary(
              title.getOrElse(threadId),
              str(thread.projectId),
              Option(str(thread.parentThreadId)).filter(_.nonEmpty),
              number(thread.createdAt).fold(0L)(_.toLong),
              number(thread.archivedAt).map(_.toLong),
              Option(str(thread.environmentId)).filter(_.nonEmpty)
            )
            if title.isDefined && summary.archivedAt.isDefined then cache.update(threadId, summary)
            summary
          }
          .recover { case _ => ThreadSummary(threadId, "", None, 0L, None, None) }

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
    projectDefaults: Map[String, ZohoTarget])

  /** Resolution of every listed thread with the context it needs: ancestors, summaries, project names and defaults. */
  def resolveWithContext(threadIds: Seq[String]): Future[Context] =
    for
      ancestry <- mappings.ancestors(threadIds)
      everyThread = (threadIds ++ ancestry.values.flatten).distinct.toVector
      summaries  <- Future.traverse(everyThread)(t => threadSummary(t).map(t -> _)).map(_.toMap)
      projects   <- projectNames()
      catalog    <- zoho.cachedCatalog()
      defaults   <- projectMappings.defaults(catalog)
      resolution <- mappings.resolveAll(threadIds, t => summaries.get(t).flatMap(sm => defaults.get(sm.projectId)))
    yield Context(resolution, summaries, projects, defaults)

  def resolve(threadId: String): Future[Resolved] =
    resolveWithContext(Seq(threadId)).map(_.resolution.resolved.getOrElse(
      threadId,
      Resolved(Billability.NotBillable, None)
    ))

  /** What the store says about every thread's recording: the submissions' spans and the struck-out stretches, as the
    * ledger cuts with them, and how many submissions touch each thread.
    */
  private final case class Covered(covering: Vector[CoveredBy], struck: Coverage, submissionsOf: String => Int)

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
      Map.empty
    ))

  private def account(
    threadId: String,
    ctx: Context,
    totals: Totals,
    submissions: Int,
    recording: Seq[Interval]): ThreadAccount =
    val summary = ctx.summaries.getOrElse(threadId, ThreadSummary(threadId, "", None, 0L, None, None))
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
      recorder.intervals(since, now).flatMap {
        case Left(error)   => Future.successful(Left(error))
        case Right(recent) =>
          val threadIds = recent.map(_.threadId).distinct
          resolveWithContext(threadIds).flatMap { ctx =>
            // The account counts the whole recording, from before the oldest listed thread began.
            val from = threadIds.flatMap(t => ctx.summaries.get(t).map(_.createdAt - 24 * 3600_000L)).filter(_ > 0)
              .minOption.fold(since)(math.min(_, since))
            recorder.intervals(cal.dayStart(from), now).map(_.map { all =>
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
                projectDefaults = ctx.projectDefaults,
                warnings = ctx.resolution.warnings
              )
            })
          }
      }
    }

  private val SessionHistoryMs = 30L * 24 * 3600_000L

  /** The sessions of the last month over all recorded attention, and the session starting at `at` (else the latest)
    * with every thread's stretches in it. A stretch is a recorded interval cut at local midnight, in the ledger's
    * state, with the summary the ledger would show.
    */
  def sessionScreen(at: Option[Long]): Future[Either[String, SessionScreenResult]] =
    val now = js.Date.now().toLong
    calendar().flatMap { (cal, _) =>
      recorder.intervals(now - SessionHistoryMs, now).flatMap {
        case Left(error) => Future.successful(Left(error))
        case Right(all)  =>
          val split    = Intervals.splitByDay(all, cal)
          val windows  = split.map(i => Window(i.threadId, i.day, i.start, i.end))
          val sessions = Sessions.of(windows)
          sessions.find(s => at.contains(s.start)).orElse(sessions.lastOption) match
            case None    => Future.successful(Right(SessionScreenResult(now, cal, sessions, None, Vector.empty, Vector.empty, Map.empty)))
            case Some(s) =>
              val inSplit = windows.filter(w => Sessions.containing(sessions, w).contains(s))
              val ids     = inSplit.map(_.threadId).distinct
              for
                ctx  <- resolveWithContext(ids)
                msgs <- Future.traverse(ids)(t => conversationOf(t).map(t -> _)).map(_.toMap)
              yield
                val led       = ledgerOf()
                // Long windows are cut at the user's message times exactly as the thread ledgers cut them, so a
                // stretch here has the key the same stretch has there.
                val inSession = inSplit.flatMap(w =>
                  Windows.subdivide(
                    w,
                    msgs.getOrElse(w.threadId, Vector.empty).filter(_.speaker == Speaker.User).map(_.at),
                    WindowMaxMs
                  )
                )
                val stretches = Ledger.of(inSession, led.covering, led.struck, Map.empty)
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
                Right(
                  SessionScreenResult(
                    now,
                    cal,
                    sessions,
                    Some(s),
                    ctx.summaries.keys.toVector.map(t =>
                      account(t, ctx, totalsOf(t, led, split), led.submissionsOf(t), all.filter(_.threadId == t))
                    ),
                    lines,
                    titles
                  )
                )
      }
    }

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
  def ledger(root: String, subthreads: Boolean): Future[Either[String, LedgerResult]] =
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
      intervals <- recorder.intervals(cal.dayStart(from), cal.nextDayStart(js.Date.now().toLong))
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
                                           raw.filter(_.threadId == t).map(i => (i.start, i.end))
                                         )
                                       ).withDefaultValue(Vector.empty)
                           stretches = Ledger.of(windows, led.covering, led.struck, traces)
                           // Shared stretches that touch anything a draft could hold, for the overlay and the cards.
                           draftable = stretches.filter(_.state.draftable).map(_.window)
                           shared    = Submissions.plan(coveredIds, draftable, withSplit, store.decisions(), billability)
                           blocks    = shared.undecided.map(_ -> None) ++ shared.decided.map((b, a) => b -> Some(a))
                           others    = blocks.map(_._1).flatMap(_.threadIds).distinct.filterNot(coveredIds.contains)
                           otherSummaries     <- Future.traverse(others)(t => threadSummary(t).map(t -> _)).map(_.toMap)
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
                             recording = withSplit,
                             billability = billability,
                             projectId = ctx.summaries.get(root).map(_.projectId).getOrElse(""),
                             environmentId = ctx.summaries.get(root).flatMap(_.environmentId)
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
        summaries.describe(l.projectId, l.environmentId, stretches, days, Text.stamp(l.calendar)).map(Right(_))
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

  /** Zoho entries on the plan's dates for the same project that this plugin did not create. */
  private def handEntered(
    target: Option[ZohoTarget],
    days: Seq[String]): Future[(Vector[HandEntered], Option[String])] =
    (target, days.minOption, days.maxOption) match
      case (Some(t), Some(from), Some(to)) =>
        zoho.clientOrFail().flatMap(_.timeEntries(from, to, t.projectId)).map { entries =>
          val mine = store.allEntryIds()
          (entries.filterNot(e => mine(e.timeEntryId)).filter(e => days.contains(e.date)).map(HandEntered.apply), None)
        }.recover { case e => (Vector.empty, Some(errorMessage(e))) }
      case _                               => Future.successful((Vector.empty, None))

  def setExcluded(windowKey: String, excluded: Boolean): Unit =
    windowKey.split('|').headOption.foreach(threadId => store.setExcluded(windowKey, threadId, excluded))

  def setNotes(root: String, day: String, notes: Option[String]): Unit = store.setNotesOverride(root, day, notes)

  /** Decides a shared stretch of the thread's ledger. */
  def decide(key: BlockKey, allocation: Allocation, root: String, subthreads: Boolean): Future[Either[String, Unit]] =
    ledger(root, subthreads).map(_.flatMap { l =>
      l.blocks.map(_._1).find(_.key == key) match
        case None        => Left("that stretch is not part of this thread's ledger; reload")
        case Some(block) => Allocation.validate(allocation, block).map(valid => store.saveDecision(key, valid))
    })

  def undecide(key: BlockKey): Unit = store.deleteDecision(key)

  /** Every undecided shared stretch of the ledger with exactly one client thread goes to that thread. */
  def decideSoleBillable(root: String, subthreads: Boolean): Future[Either[String, Int]] =
    ledger(root, subthreads).map(_.map { l =>
      val decisions = Submissions.soleBillableDecisions(l.blocks.collect { case (b, None) => b }, l.billability)
      decisions.foreach(d => store.saveDecision(d.key, d.allocation))
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
      case Right(p)                                   =>
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
          publish(BillingEvent.Changed)
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
          publish(BillingEvent.Changed)
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
