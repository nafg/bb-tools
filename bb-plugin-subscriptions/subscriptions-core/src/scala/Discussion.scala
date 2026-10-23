package subscriptionscore

enum ItemKind(val code: Char):
  /** A GitHub review verdict, with its summary text. */
  case Review extends ItemKind('r')
  /** A comment on a line of the diff. */
  case InlineComment extends ItemKind('i')
  /** A comment on the pull or merge request as a whole. */
  case Comment extends ItemKind('c')
  /** An emoji reaction on the pull or merge request itself; `body` holds the emoji's name, such as `+1`. */
  case Reaction extends ItemKind('e')

/** A review or comment on a pull or merge request.
  *
  * `location` is `path:line` for an inline comment. `reviewState` is a GitHub review's verdict, such as `APPROVED`.
  */
final case class DiscussionItem(
    kind: ItemKind,
    id: String,
    author: String,
    body: String,
    url: String,
    location: Option[String] = None,
    reviewState: Option[String] = None
):
  /** Unique among one pull or merge request's items. */
  def key: String = s"${kind.code}$id"

  /** The bb thread that says it posted this, from a [[DiscussionItem.threadMarker]] in the text. */
  def postedByThread: Option[String] = DiscussionItem.MarkerPattern.findFirstMatchIn(body).map(_.group(1))

object DiscussionItem:
  private[subscriptionscore] val MarkerPattern = """<!--\s*bb-thread:\s*([A-Za-z0-9_-]+)\s*-->""".r

  /** An invisible line a thread adds to the comments it posts, so its own subscriptions don't report them back. */
  def threadMarker(threadId: String): String = s"<!-- bb-thread: $threadId -->"
