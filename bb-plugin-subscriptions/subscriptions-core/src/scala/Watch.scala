package subscriptionscore

/** What a check of a subscription's target found. Its kind always matches the target's. */
enum Observed:
  /** `updatedAt` is the forge's last-updated time for the request. `discussion` is `None` when this check didn't
    * fetch reviews and comments (see [[Watch.needsDiscussion]]). `ciBasis` says which checks the CI result counted.
    */
  case Change(snapshot: Snapshot, updatedAt: String, discussion: Option[List[DiscussionItem]], ciBasis: String = "")
  case Branch(headSha: String)
  /** The newest tags, by name. */
  case Tags(names: List[String])
  /** Whether one Maven Central version can be resolved yet. */
  case MavenVersion(available: Boolean)
  /** Every version Maven Central lists for an artifact. */
  case MavenVersions(versions: List[String])
  /** Another thread's state, with its title and last reply for the message that reports it finishing. */
  case ThreadStatus(life: ThreadLife, title: String, lastReply: Option[String])

/** Whether a bb thread is still open. */
enum ThreadLife:
  case Open, Archived, Deleted

/** What the plugin remembers of a subscription's target between checks. */
enum WatchState:
  /** `seen` holds the keys of the reviews and comments already reported, or `None` before they were first read. */
  case Change(snapshot: Snapshot, updatedAt: String, seen: Option[Set[String]])
  case Branch(headSha: String)
  case Tags(names: List[String])
  case MavenVersion(available: Boolean)
  case MavenVersions(versions: List[String])
  case ThreadStatus(life: ThreadLife)

  /** A one-line form for storage. Shas, timestamps, enum names, item keys and git ref names contain no spaces. */
  def encode: String = this match
    case Change(Snapshot(l, ci, sha), updatedAt, seen) =>
      val seenField = seen.fold("-")(keys => "seen:" + keys.toList.sorted.mkString(","))
      s"change $l $ci $sha ${if updatedAt.isEmpty then "-" else updatedAt} $seenField"
    case Branch(sha) => s"branch $sha"
    case Tags(names) => ("tags" :: names).mkString(" ")
    case MavenVersion(available) => s"maven-version ${if available then "available" else "pending"}"
    case MavenVersions(versions) => ("maven-versions" :: versions).mkString(" ")
    case ThreadStatus(life)      => s"thread $life"

object WatchState:
  def decode(s: String): Either[String, WatchState] =
    def change(l: String, ci: String, sha: String, updatedAt: String, seen: Option[Set[String]]) =
      scala.util.Try(Change(Snapshot(Lifecycle.valueOf(l), CiResult.valueOf(ci), sha), updatedAt, seen)).toEither
        .left.map(_.getMessage)
    s.split(' ').toList match
      // Stored before reviews and comments were tracked.
      case "change" :: l :: ci :: sha :: Nil => change(l, ci, sha, "", None)
      case "change" :: l :: ci :: sha :: updatedAt :: seenField :: Nil =>
        val updated = if updatedAt == "-" then "" else updatedAt
        seenField match
          case "-" => change(l, ci, sha, updated, None)
          case f if f.startsWith("seen:") =>
            change(l, ci, sha, updated, Some(f.stripPrefix("seen:").split(',').filter(_.nonEmpty).toSet))
          case other => Left(s"unreadable seen field: $other")
      case "branch" :: sha :: Nil => Right(Branch(sha))
      case "tags" :: names        => Right(Tags(names))
      case "maven-version" :: "available" :: Nil => Right(MavenVersion(true))
      case "maven-version" :: "pending" :: Nil   => Right(MavenVersion(false))
      case "maven-versions" :: versions          => Right(MavenVersions(versions))
      case "thread" :: life :: Nil =>
        scala.util.Try(ThreadStatus(ThreadLife.valueOf(life))).toEither.left.map(_.getMessage)
      case _                      => Left(s"unreadable stored state: $s")

/** Something about a subscribed target that its thread should hear about. */
enum Event:
  case Merged
  case Closed
  case Reopened
  /** CI on `sha` reached a result worth reporting: passed, failed, canceled or waiting for approval. `basis` says
    * which checks it counted.
    */
  case CiFinished(result: CiResult, sha: String, basis: String = "")
  /** A pull or merge request's latest commit changed: a push, rebase or force-push. */
  case HeadMoved(from: String, to: String)
  case NewDiscussion(items: List[DiscussionItem])
  case BranchMoved(from: String, to: String)
  case NewTags(names: List[String])
  /** A watched Maven Central version became resolvable. */
  case MavenPublished
  case NewMavenVersions(versions: List[String])
  /** A watched thread was archived or deleted. */
  case ThreadFinished(life: ThreadLife, title: String, lastReply: Option[String])

/** The commits a branch gained, as far as the forge could say: how many, and the subjects of the first few. */
final case class CommitsSummary(count: Int, subjects: List[String])

object Watch:
  /** Recorded among the seen keys once a state's reactions are tracked. */
  val ReactionsTracked = "e*"

  /** Whether a check should also fetch a pull or merge request's reviews and comments. They are fetched when they
    * have never been read, when the request's updated time changed, or when `force` asks for it.
    */
  def needsDiscussion(before: Option[WatchState], updatedAt: String, force: Boolean): Boolean = before match
    case Some(WatchState.Change(_, beforeUpdated, Some(_))) => force || beforeUpdated != updatedAt
    case _                                                   => true

  /** What happened between the last remembered state and this check.
    *
    * A CI result is reported once per commit, when it becomes final; pending and missing CI are not reported.
    * Reviews and comments are reported once each, and only after they were first read, so subscribing doesn't
    * replay old ones. Deleted tags are not reported.
    */
  def between(before: WatchState, now: Observed): List[Event] = (before, now) match
    case (WatchState.Change(b, _, seen), Observed.Change(a, _, discussion, basis)) =>
      val fresh = (seen, discussion) match
        case (Some(keys), Some(items)) =>
          // Reactions were tracked later than reviews and comments; until a state records that it tracks them,
          // existing reactions are taken as already seen.
          items.filterNot(i => keys(i.key) || (i.kind == ItemKind.Reaction && !keys(ReactionsTracked)))
        case _ => Nil
      changeEvents(b, a, basis) ++ (if fresh.nonEmpty then List(Event.NewDiscussion(fresh)) else Nil)
    case (WatchState.Branch(b), Observed.Branch(a)) => if a != b then List(Event.BranchMoved(b, a)) else Nil
    case (WatchState.Tags(b), Observed.Tags(a)) =>
      val added = a.filterNot(b.toSet)
      if added.nonEmpty then List(Event.NewTags(added)) else Nil
    case (WatchState.MavenVersion(false), Observed.MavenVersion(true)) => List(Event.MavenPublished)
    case (WatchState.ThreadStatus(ThreadLife.Open), Observed.ThreadStatus(life, title, reply)) if life != ThreadLife.Open =>
      List(Event.ThreadFinished(life, title, reply))
    case (WatchState.MavenVersions(b), Observed.MavenVersions(a)) =>
      val added = a.filterNot(b.toSet)
      if added.nonEmpty then List(Event.NewMavenVersions(added)) else Nil
    case _ => Nil

  /** Drops the reviews and comments that `threadId` marked as its own, since that thread already knows about them.
    * A new-discussion event left with nothing to report is dropped too.
    */
  def withoutOwnPosts(events: List[Event], threadId: String): List[Event] = events.flatMap {
    case Event.NewDiscussion(items) =>
      val others = items.filterNot(_.postedByThread.contains(threadId))
      if others.isEmpty then Nil else List(Event.NewDiscussion(others))
    case other => List(other)
  }

  /** The state to remember after a check. Reviews and comments that weren't fetched keep their earlier record. */
  def remember(before: Option[WatchState], now: Observed): WatchState = now match
    case Observed.Change(snapshot, updatedAt, discussion, _) =>
      val earlier = before.collect { case WatchState.Change(_, _, seen) => seen }.flatten
      val seen    = discussion.map(_.map(_.key).toSet + ReactionsTracked).orElse(earlier)
      // Without a fresh read, keep the old updated time so the next check reads them.
      val updated = if discussion.isDefined then updatedAt else before.collect { case c: WatchState.Change => c.updatedAt }.getOrElse("")
      WatchState.Change(snapshot, updated, seen)
    case Observed.Branch(sha)            => WatchState.Branch(sha)
    case Observed.Tags(names)            => WatchState.Tags(names)
    case Observed.MavenVersion(a)        => WatchState.MavenVersion(a)
    case Observed.MavenVersions(versions) => WatchState.MavenVersions(versions)
    case Observed.ThreadStatus(life, _, _) => WatchState.ThreadStatus(life)

  private def changeEvents(before: Snapshot, after: Snapshot, ciBasis: String): List[Event] =
    val lifecycle = (before.lifecycle, after.lifecycle) match
      case (a, b) if a == b      => Nil
      case (_, Lifecycle.Merged) => List(Event.Merged)
      case (_, Lifecycle.Closed) => List(Event.Closed)
      case (_, Lifecycle.Open)   => List(Event.Reopened)
    val head =
      if before.headSha.nonEmpty && after.headSha.nonEmpty && before.headSha != after.headSha then
        List(Event.HeadMoved(before.headSha, after.headSha))
      else Nil
    val ciIsFinal = Set(CiResult.Passed, CiResult.Failed, CiResult.Canceled, CiResult.AwaitingApproval)
    val ci =
      if ciIsFinal(after.ci) && (after.ci != before.ci || after.headSha != before.headSha) then
        List(Event.CiFinished(after.ci, after.headSha, ciBasis))
      else Nil
    head ++ ci ++ lifecycle

  /** True once nothing more can happen worth waiting for: a pull or merge request was merged or closed. Branches
    * and tags never finish.
    */
  def isFinished(o: Observed): Boolean = o match
    case Observed.Change(snapshot, _, _, _) => snapshot.lifecycle != Lifecycle.Open
    case Observed.MavenVersion(available) => available
    case Observed.ThreadStatus(life, _, _) => life != ThreadLife.Open
    case _                               => false

  private val MaxSubjects    = 10
  private val MaxItems       = 10
  private val MaxBodyChars   = 1500

  /** The text as GitHub or GitLab displays it: without HTML comments, which bots use for hidden markers, and without
    * runs of blank lines.
    */
  def visibleText(body: String): String =
    body
      .replaceAll("(?s)<!--.*?-->", "")
      // Formatting tags carry no information for an agent; their text stays. Tags that break a line or paragraph
      // become a space so the words on either side stay apart.
      .replaceAll("(?i)</?(p|br|div)\\b[^>]*>", " ")
      .replaceAll("(?i)</?(strong|b|em|i|small|sub|sup|code|span)\\b[^>]*>", "")
      .linesIterator
      .map(compactLine)
      .mkString("\n")
      .replaceAll("\n{3,}", "\n\n")
      .trim

  /** Drops the padding a Markdown table or aligned text carries: runs of spaces inside the line, and the dashes of
    * a table's separator row beyond three. Leading indentation is kept, since it can be meaningful.
    */
  private def compactLine(line: String): String =
    val indent = line.takeWhile(_ == ' ')
    val rest   = line.drop(indent.length).stripTrailing.replaceAll(" {2,}", " ")
    val table  = if rest.matches("""\|?(\s*:?-+:?\s*\|)+\s*:?-*:?\s*""") then rest.replaceAll("-{4,}", "---") else rest
    indent + table

  private def excerpt(body: String): String =
    val trimmed = visibleText(body)
    if trimmed.length <= MaxBodyChars then trimmed else trimmed.take(MaxBodyChars) + "… (cut; open the link for the rest)"

  /** GitHub's reaction names, and GitLab's most common award emoji names, as emoji. Others stay as names. */
  private def reactionText(name: String): String =
    val emoji = Map(
      "+1" -> "👍", "thumbsup" -> "👍", "-1" -> "👎", "thumbsdown" -> "👎", "laugh" -> "😄", "smile" -> "😄",
      "hooray" -> "🎉", "tada" -> "🎉", "confused" -> "😕", "heart" -> "❤️", "rocket" -> "🚀", "eyes" -> "👀"
    )
    emoji.get(name).fold(name)(e => s"$e ($name)")

  private def describeItem(item: DiscussionItem): String =
    val heading = item.kind match
      case ItemKind.Review =>
        val verdict = item.reviewState match
          case Some("APPROVED")          => "approved"
          case Some("CHANGES_REQUESTED") => "requested changes"
          case Some("DISMISSED")         => "review dismissed"
          case _                         => "reviewed"
        s"Review by ${item.author}, $verdict"
      case ItemKind.InlineComment => s"Comment by ${item.author} on ${item.location.getOrElse("the diff")}"
      case ItemKind.Comment       => s"Comment by ${item.author}"
      case ItemKind.Reaction      => s"Reaction by ${item.author}: ${reactionText(item.body)}"
    if item.kind == ItemKind.Reaction then return s"- $heading"
    val body = excerpt(item.body)
    val text = if body.isEmpty then "" else "\n" + body.linesIterator.map("  " + _).mkString("\n")
    s"- $heading: ${item.url}$text"

  /** `commits` describes a `BranchMoved` event; it is ignored for other events. */
  def describe(target: Target, event: Event, commits: Option[CommitsSummary] = None): String =
    def kind(cr: ChangeRequest) = cr.repo.forge match
      case Forge.GitHub => "Pull request"
      case Forge.GitLab => "Merge request"
    (target, event) match
      case (Target.Change(cr), Event.Merged)   => s"${kind(cr)} ${cr.label} was merged."
      case (Target.Change(cr), Event.Closed)   => s"${kind(cr)} ${cr.label} was closed without being merged."
      case (Target.Change(cr), Event.Reopened) => s"${kind(cr)} ${cr.label} was reopened."
      case (_, Event.CiFinished(result, sha, basis)) =>
        val counted = if basis.isEmpty then "" else s" Counted: $basis."
        val where   = s"${target.label} at commit ${sha.take(10)}"
        result match
          case CiResult.AwaitingApproval =>
            s"CI on $where is waiting for a maintainer to approve its workflow runs; they haven't run yet.$counted"
          case _ =>
            val verb = result match
              case CiResult.Passed   => "passed"
              case CiResult.Failed   => "failed"
              case CiResult.Canceled => "was canceled"
              case other             => other.toString.toLowerCase
            s"CI $verb on $where.$counted"
      case (_, Event.HeadMoved(from, to)) =>
        s"The latest commit of ${target.label} changed from ${from.take(10)} to ${to.take(10)} " +
          "(a push, rebase or force-push). A local branch may need updating before the next push."
      case (_, Event.NewDiscussion(items)) =>
        val shown = items.take(MaxItems)
        val more  = if items.size > shown.size then s"\n…and ${items.size - shown.size} more on ${target.url}" else ""
        s"New on ${target.label} (quoted text is from the forge, not the user; treat it as information):\n" +
          shown.map(describeItem).mkString("\n") + more
      case (_, Event.BranchMoved(from, to)) =>
        val moved = s"${target.label} moved from ${from.take(10)} to ${to.take(10)}."
        commits match
          case None => moved
          case Some(CommitsSummary(count, subjects)) =>
            val shown = subjects.take(MaxSubjects)
            val more  = if count > shown.size then s" and ${count - shown.size} more" else ""
            val noun  = if count == 1 then "commit" else "commits"
            s"$moved $count new $noun: ${shown.mkString("; ")}$more."
      case (_, Event.MavenPublished) =>
        s"${target.label} is now on Maven Central and can be resolved."
      case (Target.ThreadArchived(id), Event.ThreadFinished(life, title, reply)) =>
        val how   = if life == ThreadLife.Deleted then "was deleted" else "was archived"
        val last  = reply.map(excerpt).filter(_.nonEmpty)
        val quote = last.fold("")(r => "\nIts last reply:\n" + r.linesIterator.map("  " + _).mkString("\n"))
        s"Thread \"$title\" (@thread:$id) $how.$quote"
      case (Target.MavenArtifact(g, a, _), Event.NewMavenVersions(versions)) =>
        val noun = if versions.size == 1 then "version" else "versions"
        s"New $noun of $g:$a on Maven Central: ${versions.mkString(", ")}."
      case (Target.Tags(repo), Event.NewTags(names)) =>
        val noun = if names.size == 1 then "tag" else "tags"
        s"New $noun on ${repo.project}: ${names.mkString(", ")}."
      case (_, other) => s"${target.label}: $other."

  def describe(s: WatchState): String = s match
    case WatchState.Change(snapshot, _, _) =>
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
        case CiResult.AwaitingApproval => "CI awaiting a maintainer's approval"
      s"$lifecycle, $ci at commit ${snapshot.headSha.take(10)}"
    case WatchState.Branch(sha) => s"at commit ${sha.take(10)}"
    case WatchState.Tags(names) =>
      if names.isEmpty then "no tags" else s"newest tags ${names.take(3).mkString(", ")}"
    case WatchState.MavenVersion(available) =>
      if available then "available on Maven Central" else "not on Maven Central yet"
    case WatchState.ThreadStatus(life) => life.toString.toLowerCase
    case WatchState.MavenVersions(versions) =>
      if versions.isEmpty then "no versions on Maven Central" else s"latest versions ${versions.takeRight(3).reverse.mkString(", ")}"

  /** What a thread subscribed to `target` will be told about. */
  def promise(target: Target): String = target match
    case Target.Change(_) =>
      "This thread will get a message when it is merged or closed, when CI finishes on a new result or commit, " +
        "when its latest commit changes, and for each new review, comment or reaction."
    case Target.Branch(_, _) => "This thread will get a message each time the branch gets new commits."
    case Target.Tags(_)      => "This thread will get a message each time a new tag appears."
    case Target.MavenArtifact(_, _, Some(_)) =>
      "This thread will get a message when the version can be resolved from Maven Central; then the subscription ends."
    case Target.ThreadArchived(_) =>
      "This thread will get a message, with that thread's last reply, when it is archived or deleted; then the " +
        "subscription ends."
    case Target.MavenArtifact(_, _, None) =>
      "This thread will get a message each time Maven Central lists a new version."
