package subscriptionscore

/** Whether a pull or merge request is still open. `Closed` means closed without being merged. */
enum Lifecycle:
  case Open, Merged, Closed

/** The combined result of every CI check on a pull or merge request's latest commit. */
enum CiResult:
  /** No checks or pipeline exist for the commit. */
  case NoCi
  case Pending, Passed, Failed, Canceled

/** What the plugin knows about a pull or merge request at one moment. `headSha` is its latest commit. */
final case class Snapshot(lifecycle: Lifecycle, ci: CiResult, headSha: String)

object Snapshot:
  def lifecycleFromGitHub(state: String): Either[String, Lifecycle] = state match
    case "OPEN"   => Right(Lifecycle.Open)
    case "MERGED" => Right(Lifecycle.Merged)
    case "CLOSED" => Right(Lifecycle.Closed)
    case other    => Left(s"unrecognized GitHub pull request state: $other")

  def lifecycleFromGitLab(state: String): Either[String, Lifecycle] = state match
    // GitLab reports "locked" briefly while a merge is in progress.
    case "opened" | "locked" => Right(Lifecycle.Open)
    case "merged"            => Right(Lifecycle.Merged)
    case "closed"            => Right(Lifecycle.Closed)
    case other               => Left(s"unrecognized GitLab merge request state: $other")

  /** One entry of GitHub's `statusCheckRollup`: a check run (`status` and `conclusion`) or a commit status (`state`).
    *
    * `name` identifies the check across attempts: a re-run or a superseded run of the same workflow job shares it.
    * `startedAt` is an ISO-8601 timestamp, absent for commit statuses.
    */
  enum GitHubCheck:
    case CheckRun(name: String, startedAt: Option[String], status: String, conclusion: Option[String])
    case StatusContext(name: String, state: String)

    def checkName: String = this match
      case CheckRun(n, _, _, _)   => n
      case StatusContext(n, _)    => n

  /** Keeps the most recently started attempt of each check, so a cancelled or failed attempt that was re-run does
    * not decide the result.
    */
  def latestAttempts(checks: Seq[GitHubCheck]): Seq[GitHubCheck] =
    checks.groupBy(_.checkName).values.map { attempts =>
      attempts.maxBy {
        case GitHubCheck.CheckRun(_, startedAt, _, _) => startedAt.getOrElse("")
        case GitHubCheck.StatusContext(_, _)          => ""
      }
    }.toSeq

  /** Combines GitHub checks, counting only the latest attempt of each. Any failure wins, then any cancellation,
    * then anything still running.
    */
  def ciFromGitHub(checks: Seq[GitHubCheck]): Either[String, CiResult] =
    val each: Seq[Either[String, CiResult]] = latestAttempts(checks).map {
      case GitHubCheck.CheckRun(_, _, "COMPLETED", Some(c)) =>
        c match
          case "SUCCESS" | "NEUTRAL" | "SKIPPED"                                         => Right(CiResult.Passed)
          case "FAILURE" | "TIMED_OUT" | "ACTION_REQUIRED" | "STARTUP_FAILURE" | "STALE" => Right(CiResult.Failed)
          case "CANCELLED"                                                               => Right(CiResult.Canceled)
          case other => Left(s"unrecognized GitHub check conclusion: $other")
      case GitHubCheck.CheckRun(_, _, "COMPLETED", None) => Left("GitHub check is completed but has no conclusion")
      case GitHubCheck.CheckRun(_, _, _, _)              => Right(CiResult.Pending)
      case GitHubCheck.StatusContext(_, state) =>
        state match
          case "SUCCESS"              => Right(CiResult.Passed)
          case "FAILURE" | "ERROR"    => Right(CiResult.Failed)
          case "PENDING" | "EXPECTED" => Right(CiResult.Pending)
          case other                  => Left(s"unrecognized GitHub commit status: $other")
    }
    each.collectFirst { case Left(e) => e } match
      case Some(e) => Left(e)
      case None    => Right(combine(each.collect { case Right(r) => r }))

  private def combine(results: Seq[CiResult]): CiResult =
    if results.isEmpty then CiResult.NoCi
    else if results.contains(CiResult.Failed) then CiResult.Failed
    else if results.contains(CiResult.Canceled) then CiResult.Canceled
    else if results.contains(CiResult.Pending) then CiResult.Pending
    else CiResult.Passed

  /** A GitLab merge request's head pipeline, as far as the CI result needs it. `ref` is the ref it ran on. */
  final case class GitLabPipeline(status: String, sha: String, ref: String)

  /** The CI result for a merge request whose latest commit is `mrSha`; `pipeline` is its head pipeline, if any.
    *
    * Right after a push, the head pipeline can still be the previous commit's. A branch pipeline for another commit
    * therefore counts as pending. Merged-results pipelines run on a temporary merge commit, so their sha never
    * matches; they are accepted as they are, which can report an older commit's result for a short while.
    */
  def ciFromGitLab(pipeline: Option[GitLabPipeline], mrSha: String): Either[String, CiResult] = pipeline match
    case None => Right(CiResult.NoCi)
    case Some(p) if p.sha != mrSha && !isMergeRequestRef(p.ref) => Right(CiResult.Pending)
    case Some(p) => ciFromGitLabStatus(p.status)

  private def isMergeRequestRef(ref: String): Boolean =
    ref.startsWith("refs/merge-requests/") && (ref.endsWith("/merge") || ref.endsWith("/train"))

  private def ciFromGitLabStatus(status: String): Either[String, CiResult] = status match
    case "success"                => Right(CiResult.Passed)
    case "failed"                 => Right(CiResult.Failed)
    case "canceled" | "canceling" => Right(CiResult.Canceled)
    // "manual" waits for someone to start a job, so it stays pending until they do.
    case "created" | "waiting_for_resource" | "preparing" | "pending" | "running" | "scheduled" | "manual" |
        "waiting_for_callback" =>
      Right(CiResult.Pending)
    // A skipped pipeline ran no jobs.
    case "skipped" => Right(CiResult.NoCi)
    case other     => Left(s"unrecognized GitLab pipeline status: $other")
