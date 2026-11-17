package localgitcore

import subscriptionsource.{CheckResult, Recognition}

/** A branch of a git repository on the machine running bb, written `git:<absolute path>#<branch>`. */
final case class LocalBranch(repo: String, branch: String):
  def url: String   = s"git:$repo#$branch"
  def label: String = s"$branch in $repo"

object LocalBranch:
  private val Ref = """^git:(/[^#]*[^#/])/?#(.+)$""".r

  val supported = "a local git branch, as git:/absolute/path/to/repo#branch"

  def parse(input: String): Option[LocalBranch] = input.trim match
    case Ref(repo, branch) => Some(LocalBranch(repo, branch))
    case _                 => None

  def recognize(input: String): Recognition = parse(input) match
    case Some(b) => Recognition.Recognized(b.url, b.label)
    case None if input.trim.startsWith("git:") =>
      Recognition.Invalid("write a local branch as git:/absolute/path/to/repo#branch")
    case None => Recognition.NotMine

/** The commits a branch gained since it was last seen: how many, and the subjects of the newest few. `rewritten`
  * means the earlier commit is no longer in the branch's history (a reset or force-update), so the commits are the
  * ones the new tip has that the old one didn't.
  */
final case class Movement(count: Int, subjects: List[String], rewritten: Boolean)

object LocalBranchWatch:
  private val MaxSubjects = 10

  def encode(sha: String): String = s"local-branch $sha"

  def decode(s: String): Either[String, String] = s.split(' ').toList match
    case "local-branch" :: sha :: Nil => Right(sha)
    case _                           => Left(s"unreadable stored state: $s")

  val promise = "This thread will get a message each time the branch gets new commits, or is rewritten."

  def describe(b: LocalBranch, from: String, to: String, movement: Option[Movement]): String =
    val moved = movement match
      case Some(m) if m.rewritten => s"${b.label} was rewritten from ${from.take(10)} to ${to.take(10)}."
      case _                      => s"${b.label} moved from ${from.take(10)} to ${to.take(10)}."
    movement match
      case None | Some(Movement(0, _, _)) => moved
      case Some(Movement(count, subjects, _)) =>
        val shown = subjects.take(MaxSubjects)
        val more  = if count > shown.size then s" and ${count - shown.size} more" else ""
        val noun  = if count == 1 then "commit" else "commits"
        s"$moved $count new $noun: ${shown.mkString("; ")}$more."

  /** What to tell a subscriber since `previous` (the branch's last tip), and what to remember. `movement` describes
    * a change of tip, when git could say. A first check reports nothing.
    */
  def check(b: LocalBranch, previous: Option[String], now: String, movement: Option[Movement]): CheckResult =
    val messages = previous match
      case Some(from) if from != now => List(describe(b, from, now, movement))
      case _                         => Nil
    CheckResult(
      state = encode(now),
      label = b.label,
      summary = s"at commit ${now.take(10)}",
      promise = promise,
      link = b.url,
      messages = messages,
      finished = false
    )
