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

  /** One entry of GitHub's `statusCheckRollup`: a check run (`status` and `conclusion`) or a commit status (`state`). */
  enum GitHubCheck:
    case CheckRun(status: String, conclusion: Option[String])
    case StatusContext(state: String)

  /** Combines GitHub checks. Any failure wins, then any cancellation, then anything still running. */
  def ciFromGitHub(checks: Seq[GitHubCheck]): Either[String, CiResult] =
    val each: Seq[Either[String, CiResult]] = checks.map {
      case GitHubCheck.CheckRun("COMPLETED", Some(c)) =>
        c match
          case "SUCCESS" | "NEUTRAL" | "SKIPPED"                                         => Right(CiResult.Passed)
          case "FAILURE" | "TIMED_OUT" | "ACTION_REQUIRED" | "STARTUP_FAILURE" | "STALE" => Right(CiResult.Failed)
          case "CANCELLED"                                                               => Right(CiResult.Canceled)
          case other => Left(s"unrecognized GitHub check conclusion: $other")
      case GitHubCheck.CheckRun("COMPLETED", None) => Left("GitHub check is completed but has no conclusion")
      case GitHubCheck.CheckRun(_, _)              => Right(CiResult.Pending)
      case GitHubCheck.StatusContext(state) =>
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

  /** A GitLab merge request's head pipeline status; `None` when it has no pipeline. */
  def ciFromGitLab(pipelineStatus: Option[String]): Either[String, CiResult] = pipelineStatus match
    case None => Right(CiResult.NoCi)
    case Some(s) =>
      s match
        case "success" => Right(CiResult.Passed)
        case "failed"  => Right(CiResult.Failed)
        case "canceled" | "canceling" => Right(CiResult.Canceled)
        // "manual" waits for someone to start a job, so it stays pending until they do.
        case "created" | "waiting_for_resource" | "preparing" | "pending" | "running" | "scheduled" | "manual" |
            "waiting_for_callback" =>
          Right(CiResult.Pending)
        // A skipped pipeline ran no jobs.
        case "skipped" => Right(CiResult.NoCi)
        case other     => Left(s"unrecognized GitLab pipeline status: $other")
