package subscriptionscore

enum Forge:
  case GitHub, GitLab

/** A GitHub pull request or a GitLab merge request, identified by where it lives.
  *
  * `project` is the repository path: `owner/repo` on GitHub, `group/subgroup/project` on GitLab.
  */
final case class ChangeRequest(forge: Forge, host: String, project: String, number: Int):
  def url: String = forge match
    case Forge.GitHub => s"https://$host/$project/pull/$number"
    case Forge.GitLab => s"https://$host/$project/-/merge_requests/$number"

  /** How a person would refer to it, e.g. `slick/slick#3909` or `chesednow/disp!530`. */
  def label: String = forge match
    case Forge.GitHub => s"$project#$number"
    case Forge.GitLab => s"$project!$number"

object ChangeRequest:
  private val GitHubPath = """^/([^/]+/[^/]+)/pull/(\d+)/?$""".r
  private val GitLabPath = """^/(.+?)/-/merge_requests/(\d+)/?$""".r

  /** Parses a pull request or merge request URL. Trailing tabs such as `/files` or `/diffs`, query strings and
    * fragments are ignored. Any host whose path has the GitHub shape is GitHub; any with the GitLab shape is GitLab.
    */
  def parse(url: String): Either[String, ChangeRequest] =
    val Url = """^https?://([^/?#]+)(/[^?#]*)?(?:[?#].*)?$""".r
    url.trim match
      case Url(host, rawPath) =>
        val path = Option(rawPath).getOrElse("")
          .replaceFirst("""/(files|commits|checks|changes|diffs|pipelines)/?$""", "")
        path match
          case GitHubPath(project, n) => Right(ChangeRequest(Forge.GitHub, host, project, n.toInt))
          case GitLabPath(project, n) => Right(ChangeRequest(Forge.GitLab, host, project, n.toInt))
          case _ => Left(s"not a pull request or merge request URL: $url")
      case _ => Left(s"not a URL: $url")
