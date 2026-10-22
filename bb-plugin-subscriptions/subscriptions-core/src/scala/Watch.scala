package subscriptionscore

/** What the plugin last saw of a subscription's target. Its kind always matches the target's. */
enum WatchState:
  case Change(snapshot: Snapshot)
  case Branch(headSha: String)
  /** The newest tags, by name. */
  case Tags(names: List[String])

  /** A one-line form for storage. Shas, enum names and git ref names contain no spaces. */
  def encode: String = this match
    case Change(Snapshot(l, ci, sha)) => s"change $l $ci $sha"
    case Branch(sha)                  => s"branch $sha"
    case Tags(names)                  => ("tags" :: names).mkString(" ")

object WatchState:
  def decode(s: String): Either[String, WatchState] =
    s.split(' ').toList match
      case "change" :: l :: ci :: sha :: Nil =>
        scala.util.Try(Change(Snapshot(Lifecycle.valueOf(l), CiResult.valueOf(ci), sha))).toEither.left.map(_.getMessage)
      case "branch" :: sha :: Nil => Right(Branch(sha))
      case "tags" :: names        => Right(Tags(names))
      case _                      => Left(s"unreadable stored state: $s")

/** Something about a subscribed target that its thread should hear about. */
enum Event:
  case Merged
  case Closed
  case Reopened
  /** CI on `sha` reached a final result: passed, failed or canceled. */
  case CiFinished(result: CiResult, sha: String)
  case BranchMoved(from: String, to: String)
  case NewTags(names: List[String])

/** The commits a branch gained, as far as the forge could say: how many, and the subjects of the first few. */
final case class CommitsSummary(count: Int, subjects: List[String])

object Watch:
  /** What happened between two observations of the same target.
    *
    * A CI result is reported once per commit, when it becomes final. Results that are still pending, or that
    * report no CI, are not reported. Deleted tags are not reported.
    */
  def between(before: WatchState, after: WatchState): List[Event] = (before, after) match
    case (WatchState.Change(b), WatchState.Change(a)) => changeEvents(b, a)
    case (WatchState.Branch(b), WatchState.Branch(a)) => if a != b then List(Event.BranchMoved(b, a)) else Nil
    case (WatchState.Tags(b), WatchState.Tags(a)) =>
      val added = a.filterNot(b.toSet)
      if added.nonEmpty then List(Event.NewTags(added)) else Nil
    case _ => Nil

  private def changeEvents(before: Snapshot, after: Snapshot): List[Event] =
    val lifecycle = (before.lifecycle, after.lifecycle) match
      case (a, b) if a == b      => Nil
      case (_, Lifecycle.Merged) => List(Event.Merged)
      case (_, Lifecycle.Closed) => List(Event.Closed)
      case (_, Lifecycle.Open)   => List(Event.Reopened)
    val ciIsFinal = Set(CiResult.Passed, CiResult.Failed, CiResult.Canceled)
    val ci =
      if ciIsFinal(after.ci) && (after.ci != before.ci || after.headSha != before.headSha) then
        List(Event.CiFinished(after.ci, after.headSha))
      else Nil
    ci ++ lifecycle

  /** True once nothing more can happen worth waiting for: a pull or merge request was merged or closed. Branches
    * and tags never finish.
    */
  def isFinished(s: WatchState): Boolean = s match
    case WatchState.Change(snapshot) => snapshot.lifecycle != Lifecycle.Open
    case _                           => false

  private val MaxSubjects = 10

  /** `commits` describes a `BranchMoved` event; it is ignored for other events. */
  def describe(target: Target, event: Event, commits: Option[CommitsSummary] = None): String =
    def kind(cr: ChangeRequest) = cr.repo.forge match
      case Forge.GitHub => "Pull request"
      case Forge.GitLab => "Merge request"
    (target, event) match
      case (Target.Change(cr), Event.Merged)   => s"${kind(cr)} ${cr.label} was merged."
      case (Target.Change(cr), Event.Closed)   => s"${kind(cr)} ${cr.label} was closed without being merged."
      case (Target.Change(cr), Event.Reopened) => s"${kind(cr)} ${cr.label} was reopened."
      case (_, Event.CiFinished(result, sha)) =>
        val verb = result match
          case CiResult.Passed   => "passed"
          case CiResult.Failed   => "failed"
          case CiResult.Canceled => "was canceled"
          case other             => other.toString.toLowerCase
        s"CI $verb on ${target.label} at commit ${sha.take(10)}."
      case (_, Event.BranchMoved(from, to)) =>
        val moved = s"${target.label} moved from ${from.take(10)} to ${to.take(10)}."
        commits match
          case None => moved
          case Some(CommitsSummary(count, subjects)) =>
            val shown = subjects.take(MaxSubjects)
            val more  = if count > shown.size then s" and ${count - shown.size} more" else ""
            val noun  = if count == 1 then "commit" else "commits"
            s"$moved $count new $noun: ${shown.mkString("; ")}$more."
      case (Target.Tags(repo), Event.NewTags(names)) =>
        val noun = if names.size == 1 then "tag" else "tags"
        s"New $noun on ${repo.project}: ${names.mkString(", ")}."
      case (_, other) => s"${target.label}: $other."

  def describe(s: WatchState): String = s match
    case WatchState.Change(snapshot) =>
      val lifecycle = snapshot.lifecycle match
        case Lifecycle.Open   => "open"
        case Lifecycle.Merged => "merged"
        case Lifecycle.Closed => "closed without being merged"
      val ci = snapshot.ci match
        case CiResult.NoCi     => "no CI"
        case CiResult.Pending  => "CI running"
        case CiResult.Passed   => "CI passed"
        case CiResult.Failed   => "CI failed"
        case CiResult.Canceled => "CI canceled"
      s"$lifecycle, $ci at commit ${snapshot.headSha.take(10)}"
    case WatchState.Branch(sha) => s"at commit ${sha.take(10)}"
    case WatchState.Tags(names) =>
      if names.isEmpty then "no tags" else s"newest tags ${names.take(3).mkString(", ")}"

  /** What a thread subscribed to `target` will be told about. */
  def promise(target: Target): String = target match
    case Target.Change(_) =>
      "This thread will get a message when it is merged or closed, and each time CI finishes on a new result or commit."
    case Target.Branch(_, _) => "This thread will get a message each time the branch gets new commits."
    case Target.Tags(_)      => "This thread will get a message each time a new tag appears."
