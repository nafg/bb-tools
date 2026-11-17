package forgecore

import subscriptionsource.Recognition

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

/** A GitHub or GitLab page a thread can subscribe to, identified by its URL. */
enum ForgeTarget:
  case Change(changeRequest: ChangeRequest)
  /** The latest commit on a branch. */
  case Branch(repo: Repo, name: String)
  /** The set of tags in a repository. */
  case Tags(repo: Repo)

  def forge: Forge = this match
    case Change(cr)   => cr.repo.forge
    case Branch(r, _) => r.forge
    case Tags(r)      => r.forge

  def url: String = this match
    case Change(cr)         => cr.url
    case Branch(repo, name) => s"${repo.url}${repo.pagePrefix}/tree/$name"
    case Tags(repo)         => s"${repo.url}${repo.pagePrefix}/tags"

  def label: String = this match
    case Change(cr)         => cr.label
    case Branch(repo, name) => s"$name on ${repo.project}"
    case Tags(repo)         => s"tags of ${repo.project}"

object ForgeTarget:
  private val Url        = """^https?://([^/?#]+)(/[^?#]*)?(?:[?#].*)?$""".r
  private val GitHubPull = """^/([^/]+/[^/]+)/pull/(\d+)$""".r
  private val GitLabMr   = """^/(.+?)/-/merge_requests/(\d+)$""".r
  private val GitHubTree = """^/([^/]+/[^/]+)/(?:tree|commits)/(.+)$""".r
  private val GitLabTree = """^/(.+?)/-/(?:tree|commits)/(.+)$""".r
  private val GitHubTags = """^/([^/]+/[^/]+)/tags$""".r
  private val GitLabTags = """^/(.+?)/-/tags$""".r

  /** Hosts whose URLs are some other source's, though their paths can look like a forge's. */
  private val NotForges = Set("central.sonatype.com", "mvnrepository.com", "repo1.maven.org", "repo.maven.apache.org")

  def supported(forge: Forge): String =
    val (name, prefix) = forge match
      case Forge.GitHub => ("pull request", "")
      case Forge.GitLab => ("merge request", "/-")
    s"a $forge $name URL, a branch URL (…$prefix/tree/<branch>), or a repository's tags URL (…$prefix/tags)"

  /** Parses the URL of a pull or merge request, a branch, or a repository's tags page, on any host. Trailing tabs
    * such as `/files` or `/diffs` on a pull or merge request, query strings and fragments are ignored. A path with
    * the GitLab shape (`/-/`) is GitLab's; one with the GitHub shape is GitHub's.
    */
  def parse(url: String): Option[ForgeTarget] =
    url.trim match
      case Url(host, _) if NotForges(host) => None
      case Url(host, rawPath) =>
        val path = Option(rawPath).getOrElse("").stripSuffix("/")
        def gh(project: String) = Repo(Forge.GitHub, host, project)
        def gl(project: String) = Repo(Forge.GitLab, host, project)
        val withoutTab = path.replaceFirst("""/(files|commits|checks|changes|diffs|pipelines)$""", "")
        (withoutTab, path) match
          case (GitLabMr(project, n), _)   => Some(Change(ChangeRequest(gl(project), n.toInt)))
          case (GitHubPull(project, n), _) => Some(Change(ChangeRequest(gh(project), n.toInt)))
          case (_, GitLabTags(project))    => Some(Tags(gl(project)))
          case (_, GitLabTree(project, b)) => Some(Branch(gl(project), b))
          case (_, GitHubTags(project))    => Some(Tags(gh(project)))
          case (_, GitHubTree(project, b)) => Some(Branch(gh(project), b))
          case _                           => None
      case _ => None

  /** Whether `input` is one of `forge`'s targets, in its canonical form. */
  def recognize(input: String, forge: Forge): Recognition =
    parse(input).filter(_.forge == forge) match
      case Some(t) => Recognition.Recognized(t.url, t.label)
      case None    => Recognition.NotMine
