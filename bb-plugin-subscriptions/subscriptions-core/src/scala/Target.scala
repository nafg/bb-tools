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
  /** A Maven Central artifact: one version becoming resolvable, or with no version, each new version. */
  case MavenArtifact(group: String, artifact: String, version: Option[String])

  def url: String = this match
    case Change(cr)         => cr.url
    case Branch(repo, name) => s"${repo.url}${repo.pagePrefix}/tree/$name"
    case Tags(repo)         => s"${repo.url}${repo.pagePrefix}/tags"
    case MavenArtifact(g, a, v) => s"https://central.sonatype.com/artifact/$g/$a" + v.fold("")("/" + _)

  def label: String = this match
    case Change(cr)         => cr.label
    case Branch(repo, name) => s"$name on ${repo.project}"
    case Tags(repo)         => s"tags of ${repo.project}"
    case MavenArtifact(g, a, v) => s"$g:$a" + v.fold("")(":" + _)

object Target:
  private val Url        = """^https?://([^/?#]+)(/[^?#]*)?(?:[?#].*)?$""".r
  private val GitHubPull = """^/([^/]+/[^/]+)/pull/(\d+)$""".r
  private val GitLabMr   = """^/(.+?)/-/merge_requests/(\d+)$""".r
  private val GitHubTree = """^/([^/]+/[^/]+)/(?:tree|commits)/(.+)$""".r
  private val GitLabTree = """^/(.+?)/-/(?:tree|commits)/(.+)$""".r
  private val GitHubTags = """^/([^/]+/[^/]+)/tags$""".r
  private val GitLabTags = """^/(.+?)/-/tags$""".r

  private val Coordinates = """^([A-Za-z0-9_.-]+):([A-Za-z0-9_.-]+)(?::([A-Za-z0-9_.+-]+))?$""".r
  private val Segment     = """[A-Za-z0-9_.+-]+"""
  /** central.sonatype.com and mvnrepository.com share this path shape. */
  private val ArtifactPage = s"""^/artifact/($Segment)/($Segment)(?:/($Segment))?$$""".r
  private val RepoPath    = s"""^/maven2/(.+)/($Segment)(?:/($Segment))?$$""".r

  val supported =
    "a pull request or merge request URL, a branch URL (…/tree/<branch>), a repository's tags URL (…/tags), or a " +
      "Maven Central artifact as group:artifact[:version] or its central.sonatype.com, repo1.maven.org or " +
      "mvnrepository.com URL"

  private def maven(group: String, artifact: String, version: String | Null): Target =
    MavenArtifact(group, artifact, Option(version).map(_.nn))

  /** A repo1.maven.org path ends either with the artifact or with artifact/version. A version directory always
    * starts with a digit; an artifact name does not have to, so a trailing segment starting with a digit is taken as
    * the version.
    */
  private def fromRepoPath(groupPath: String, last2: String, last: String | Null): Target =
    Option(last).map(_.nn) match
      case Some(v) => MavenArtifact(groupPath.replace('/', '.'), last2, Some(v))
      case None =>
        val parts = groupPath.split('/').toList
        if last2.headOption.exists(_.isDigit) && parts.size >= 2 then
          MavenArtifact(parts.init.mkString("."), parts.last, Some(last2))
        else MavenArtifact(groupPath.replace('/', '.'), last2, None)

  /** Parses the URL of a pull or merge request, a branch, or a repository's tags page. Trailing tabs such as
    * `/files` or `/diffs` on a pull or merge request, query strings and fragments are ignored. A host whose path has
    * the GitHub shape is GitHub; one with the GitLab shape (`/-/`) is GitLab.
    */
  def parse(url: String): Either[String, Target] =
    url.trim match
      case Coordinates(g, a, v) => Right(maven(g, a, v))
      case Url(host, rawPath) if host == "central.sonatype.com" || host == "mvnrepository.com" =>
        Option(rawPath).getOrElse("").stripSuffix("/") match
          case ArtifactPage(g, a, v) => Right(maven(g, a, v))
          case _                    => Left(s"not a Maven Central artifact URL: $url")
      case Url(host, rawPath) if host == "repo1.maven.org" || host == "repo.maven.apache.org" =>
        Option(rawPath).getOrElse("").stripSuffix("/") match
          case RepoPath(groupPath, a, v) => Right(fromRepoPath(groupPath, a, v))
          case _                         => Left(s"not a Maven Central artifact URL: $url")
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
