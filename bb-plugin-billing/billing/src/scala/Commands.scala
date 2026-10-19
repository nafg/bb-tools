package billing

import bbplugin.BbApi
import bbplugin.Dyn.str
import billingcore.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

/** The "To bill" view with what the page needs beside it. The overview itself may be missing when the recorder cannot
  * be reached; the page still shows the setup.
  */
final case class OverviewReply(
  overview: Either[String, Overview],
  since: Long,
  recorder: RecorderStatus,
  zohoMissing: List[String],
  projectZoho: Map[String, Vector[ProjectZoho]],
  catalog: Option[Catalog],
  summariesAvailable: Boolean)

/** Where a thread bills: its own entry (or why it is unreadable) and the rule that decided. */
final case class MappingReply(threadId: String, title: String, own: Either[String, MappingEntry], resolved: Resolved)

final case class SubmissionsReply(timeZone: String, submissions: Vector[SubmissionRecord])

/** Each bb project with the Zoho projects its threads may bill to. */
final case class ProjectMappingsReply(projects: Vector[(String, String, Vector[ProjectZoho])], catalog: Option[Catalog])

/** What each request of the contract answers with on the server. */
type Served[R] = R match
  case Reply.Ok              => Unit
  case Reply.Overview        => OverviewReply
  case Reply.Ledger          => LedgerResult
  case Reply.Plan            => PlanResult
  case Reply.Submission      => SubmissionRecord
  case Reply.Deleted         => Int
  case Reply.Decided         => Int
  case Reply.Submissions     => SubmissionsReply
  case Reply.Catalog         => Catalog
  case Reply.Mapping         => MappingReply
  case Reply.ProjectMappings => ProjectMappingsReply
  case Reply.Report          => Service#Report
  case Reply.Audit           => Service#Audit

/** The one door: every request of the contract, from the page, the CLI or the agent, runs here. A failure is a failed
  * future whose message is for the user.
  */
final class Commands(
  bb: BbApi,
  service: Service,
  recorder: Recorder,
  mappings: Mappings,
  projectMappings: ProjectMappings,
  zoho: Zoho,
  summaries: Summaries
)(using ExecutionContext):
  private def orFail[A](either: Either[String, A]): Future[A] =
    either.fold(m => Future.failed(new RuntimeException(m)), Future.successful)

  def run[R](rpc: BillingRpc[R]): Future[Served[R]] =
    rpc match
      case BillingRpc.Overview(since)                       =>
        for
          status      <- recorder.status()
          missing     <- zoho.missingSettings()
          projectZoho <- projectMappings.all()
          catalog     <- zoho.cachedCatalog()
          available   <- summaries.available()
          result      <- service.overview(since)
        yield OverviewReply(result, since, status, missing, projectZoho, catalog, available)
      case BillingRpc.Ledger(threadId, subthreads)          => service.ledger(threadId, subthreads).flatMap(orFail)
      case BillingRpc.Draft(threadId, subthreads, pick)     =>
        service.planOf(threadId, subthreads, pick).flatMap(orFail)
      case BillingRpc.Summarize(threadId, subthreads, pick) =>
        service.summarize(threadId, subthreads, pick).flatMap(orFail)
      case BillingRpc.SetExcluded(windowKey, excluded) => Future.successful(service.setExcluded(windowKey, excluded))
      case BillingRpc.SetNotes(threadId, day, notes)   =>
        Future.successful(service.setNotes(threadId, day.iso, notes))
      case BillingRpc.Decide(threadId, subthreads, key, a)       =>
        service.decide(key, a, threadId, subthreads).flatMap(orFail)
      case BillingRpc.Undecide(key)                              => Future.successful(service.undecide(key))
      case BillingRpc.SettleAll(threadId, subthreads)            =>
        service.decideSoleBillable(threadId, subthreads).flatMap(orFail)
      case BillingRpc.Submit(threadId, subthreads, pick, billed) =>
        for
          outcome <- service.submit(threadId, subthreads, pick, billed).flatMap(orFail)
          titles  <- Future.traverse((threadId +: outcome.submission.spans.map(_.threadId)).distinct)(t =>
                       service.threadTitle(t).map(t -> _)
                     )
        yield outcome.record(titles.toMap)
      case BillingRpc.Undo(submissionId)                         => service.undo(submissionId).flatMap(orFail)
      case BillingRpc.Audit(submissionId)                        => service.audit(submissionId).flatMap(orFail)
      case BillingRpc.Submissions                                =>
        for
          subs     <- service.submissions(None)
          (cal, _) <- service.calendar()
        yield SubmissionsReply(cal.timeZone, subs)
      case BillingRpc.Catalog(refresh)                           => zoho.catalog(refresh)
      case BillingRpc.Mapping(threadId)                          =>
        for
          own      <- bb.sdk.threads.getPluginMetadata(js.Dynamic.literal("threadId" -> threadId)).toFuture
          resolved <- service.resolve(threadId)
          title    <- service.threadTitle(threadId)
        yield MappingReply(threadId, title, mappings.entryOf(own), resolved)
      case BillingRpc.Assign(threadId, projectId, taskId)        =>
        for
          catalog <- zoho.catalog(refresh = false)
          target  <- orFail(catalog.target(projectId, taskId).toRight("unknown project or task; refresh the catalog"))
          _       <- mappings.assign(threadId, target)
        yield ()
      case BillingRpc.MarkUnbillable(threadId)                   => mappings.markUnbillable(threadId).map(_ => ())
      case BillingRpc.ClearMapping(threadId)                     => mappings.clear(threadId).map(_ => ())
      case BillingRpc.ProjectMappings                            =>
        for
          all        <- projectMappings.all()
          catalog    <- zoho.cachedCatalog()
          bbProjects <- bb.sdk.projects.list(js.Dynamic.literal("includePersonal" -> true)).toFuture
        yield ProjectMappingsReply(
          bbProjects.toVector.map(p => (str(p.id), str(p.name), all.getOrElse(str(p.id), Vector.empty))),
          catalog
        )
      case BillingRpc.SetProjectMapping(projectId, zohoList) => projectMappings.set(projectId, zohoList).map(_ => ())
      case BillingRpc.Report(customerId, from, to)           => service.report(customerId, from.iso, to.iso)
