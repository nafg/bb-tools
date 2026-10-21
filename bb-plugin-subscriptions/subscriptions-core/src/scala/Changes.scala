package subscriptionscore

/** Something about a subscribed pull or merge request that its thread should hear about. */
enum Change:
  case Merged
  case Closed
  case Reopened
  /** CI on `sha` reached a final result: passed, failed or canceled. */
  case CiFinished(result: CiResult, sha: String)

object Changes:
  /** What happened between two snapshots of the same pull or merge request.
    *
    * A CI result is reported once per commit, when it becomes final. Results that are still pending, or that
    * report no CI, are not reported.
    */
  def between(before: Snapshot, after: Snapshot): List[Change] =
    val lifecycle = (before.lifecycle, after.lifecycle) match
      case (a, b) if a == b         => Nil
      case (_, Lifecycle.Merged)    => List(Change.Merged)
      case (_, Lifecycle.Closed)    => List(Change.Closed)
      case (_, Lifecycle.Open)      => List(Change.Reopened)
    val ciIsFinal = Set(CiResult.Passed, CiResult.Failed, CiResult.Canceled)
    val ci =
      if ciIsFinal(after.ci) && (after.ci != before.ci || after.headSha != before.headSha) then
        List(Change.CiFinished(after.ci, after.headSha))
      else Nil
    ci ++ lifecycle

  /** True once nothing more can happen worth waiting for: the request was merged or closed. */
  def isFinished(s: Snapshot): Boolean = s.lifecycle != Lifecycle.Open

  def describe(cr: ChangeRequest, change: Change): String =
    val kind = cr.forge match
      case Forge.GitHub => "Pull request"
      case Forge.GitLab => "Merge request"
    change match
      case Change.Merged   => s"$kind ${cr.label} was merged."
      case Change.Closed   => s"$kind ${cr.label} was closed without being merged."
      case Change.Reopened => s"$kind ${cr.label} was reopened."
      case Change.CiFinished(result, sha) =>
        val verb = result match
          case CiResult.Passed   => "passed"
          case CiResult.Failed   => "failed"
          case CiResult.Canceled => "was canceled"
          case other             => other.toString.toLowerCase
        s"CI $verb on ${cr.label} at commit ${sha.take(10)}."

  def describe(s: Snapshot): String =
    val lifecycle = s.lifecycle match
      case Lifecycle.Open   => "open"
      case Lifecycle.Merged => "merged"
      case Lifecycle.Closed => "closed without being merged"
    val ci = s.ci match
      case CiResult.NoCi     => "no CI"
      case CiResult.Pending  => "CI running"
      case CiResult.Passed   => "CI passed"
      case CiResult.Failed   => "CI failed"
      case CiResult.Canceled => "CI canceled"
    s"$lifecycle, $ci at commit ${s.headSha.take(10)}"
