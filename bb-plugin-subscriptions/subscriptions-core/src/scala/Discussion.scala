package subscriptionscore

enum ItemKind(val code: Char):
  /** A GitHub review verdict, with its summary text. */
  case Review extends ItemKind('r')
  /** A comment on a line of the diff. */
  case InlineComment extends ItemKind('i')
  /** A comment on the pull or merge request as a whole. */
  case Comment extends ItemKind('c')

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
