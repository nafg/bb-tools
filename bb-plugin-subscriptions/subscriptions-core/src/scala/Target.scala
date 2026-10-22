package subscriptionscore

enum Forge:
  case GitHub, GitLab

/** A repository on a forge. `project` is its path: `owner/repo` on GitHub, `group/subgroup/project` on GitLab. */
final case class Repo(forge: Forge, host: String, project: String):
  def url: String = s"https://$host/$project"

  /** The prefix GitLab puts before repository pages such as `tree` and `tags`; GitHub has none. */
  def pagePrefix: String = forge match
    case Forge.GitHub => ""
    case Forge.GitLab => "/-"

/** A GitHub pull request or a GitLab merge request. */
final case class ChangeRequest(repo: Repo, number: Int):
  def url: String = repo.forge match
    case Forge.GitHub => s"${repo.url}/pull/$number"
    case Forge.GitLab => s"${repo.url}/-/merge_requests/$number"

  /** How a person would refer to it, e.g. `slick/slick#3909` or `chesednow/disp!530`. */
  def label: String = repo.forge match
    case Forge.GitHub => s"${repo.project}#$number"
    case Forge.GitLab => s"${repo.project}!$number"

/** Something a thread can subscribe to, identified by the URL of its page on the forge. */
enum Target:
  case Change(changeRequest: ChangeRequest)
  /** The latest commit on a branch. */
  case Branch(repo: Repo, name: String)
  /** The set of tags in a repository. */
  case Tags(repo: Repo)

  def url: String = this match
    case Change(cr)         => cr.url
    case Branch(repo, name) => s"${repo.url}${repo.pagePrefix}/tree/$name"
    case Tags(repo)         => s"${repo.url}${repo.pagePrefix}/tags"

  def label: String = this match
    case Change(cr)         => cr.label
    case Branch(repo, name) => s"$name on ${repo.project}"
    case Tags(repo)         => s"tags of ${repo.project}"

object Target:
  private val Url        = """^https?://([^/?#]+)(/[^?#]*)?(?:[?#].*)?$""".r
  private val GitHubPull = """^/([^/]+/[^/]+)/pull/(\d+)$""".r
  private val GitLabMr   = """^/(.+?)/-/merge_requests/(\d+)$""".r
  private val GitHubTree = """^/([^/]+/[^/]+)/(?:tree|commits)/(.+)$""".r
  private val GitLabTree = """^/(.+?)/-/(?:tree|commits)/(.+)$""".r
  private val GitHubTags = """^/([^/]+/[^/]+)/tags$""".r
  private val GitLabTags = """^/(.+?)/-/tags$""".r

  val supported =
    "a pull request or merge request URL, a branch URL (…/tree/<branch>), or a repository's tags URL (…/tags)"

  /** Parses the URL of a pull or merge request, a branch, or a repository's tags page. Trailing tabs such as
    * `/files` or `/diffs` on a pull or merge request, query strings and fragments are ignored. A host whose path has
    * the GitHub shape is GitHub; one with the GitLab shape (`/-/`) is GitLab.
    */
  def parse(url: String): Either[String, Target] =
    url.trim match
      case Url(host, rawPath) =>
        val path = Option(rawPath).getOrElse("").stripSuffix("/")
        def gh(project: String) = Repo(Forge.GitHub, host, project)
        def gl(project: String) = Repo(Forge.GitLab, host, project)
        val withoutTab = path.replaceFirst("""/(files|commits|checks|changes|diffs|pipelines)$""", "")
        (withoutTab, path) match
          case (GitLabMr(project, n), _)   => Right(Change(ChangeRequest(gl(project), n.toInt)))
          case (GitHubPull(project, n), _) => Right(Change(ChangeRequest(gh(project), n.toInt)))
          case (_, GitLabTags(project))    => Right(Tags(gl(project)))
          case (_, GitLabTree(project, b)) => Right(Branch(gl(project), b))
          case (_, GitHubTags(project))    => Right(Tags(gh(project)))
          case (_, GitHubTree(project, b)) => Right(Branch(gh(project), b))
          case _                           => Left(s"not $supported: $url")
      case _ => Left(s"not a URL: $url")
