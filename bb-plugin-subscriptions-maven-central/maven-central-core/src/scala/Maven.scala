package mavencore

import subscriptionsource.{CheckResult, Recognition}

/** A Maven Central artifact: with a version, that version becoming resolvable; without one, each new version. */
final case class MavenArtifact(group: String, artifact: String, version: Option[String]):
  def url: String   = s"https://central.sonatype.com/artifact/$group/$artifact" + version.fold("")("/" + _)
  def label: String = s"$group:$artifact" + version.fold("")(":" + _)

object MavenArtifact:
  /** `maven:group:artifact[:version]`. The scheme keeps coordinates from being read as some other URL. */
  private val Coordinates = """^maven:([A-Za-z0-9_.-]+):([A-Za-z0-9_.-]+)(?::([A-Za-z0-9_.+-]+))?$""".r
  private val BareCoords  = """^([A-Za-z0-9_.-]+):([A-Za-z0-9_.-]+)(?::([A-Za-z0-9_.+-]+))?$""".r
  private val Url         = """^https?://([^/?#]+)(/[^?#]*)?(?:[?#].*)?$""".r
  private val Segment     = """[A-Za-z0-9_.+-]+"""
  /** central.sonatype.com and mvnrepository.com share this path shape. */
  private val ArtifactPage = s"""^/artifact/($Segment)/($Segment)(?:/($Segment))?$$""".r
  private val RepoPath     = s"""^/maven2/(.+)/($Segment)(?:/($Segment))?$$""".r

  val supported =
    "a Maven Central artifact as maven:group:artifact[:version], or its central.sonatype.com, repo1.maven.org or " +
      "mvnrepository.com URL"

  private def of(group: String, artifact: String, version: String | Null) =
    MavenArtifact(group, artifact, Option(version).map(_.nn))

  /** A repo1.maven.org path ends either with the artifact or with artifact/version. A version directory always
    * starts with a digit; an artifact name does not have to, so a trailing segment starting with a digit is taken as
    * the version.
    */
  private def fromRepoPath(groupPath: String, last2: String, last: String | Null): MavenArtifact =
    Option(last).map(_.nn) match
      case Some(v) => MavenArtifact(groupPath.replace('/', '.'), last2, Some(v))
      case None =>
        val parts = groupPath.split('/').toList
        if last2.headOption.exists(_.isDigit) && parts.size >= 2 then
          MavenArtifact(parts.init.mkString("."), parts.last, Some(last2))
        else MavenArtifact(groupPath.replace('/', '.'), last2, None)

  /** The artifact `input` names, if it names one in a form this source accepts. */
  def parse(input: String): Option[MavenArtifact] = input.trim match
    case Coordinates(g, a, v) => Some(of(g, a, v))
    case Url(host, rawPath) if host == "central.sonatype.com" || host == "mvnrepository.com" =>
      Option(rawPath).getOrElse("").stripSuffix("/") match
        case ArtifactPage(g, a, v) => Some(of(g, a, v))
        case _                     => None
    case Url(host, rawPath) if host == "repo1.maven.org" || host == "repo.maven.apache.org" =>
      Option(rawPath).getOrElse("").stripSuffix("/") match
        case RepoPath(groupPath, a, v) => Some(fromRepoPath(groupPath, a, v))
        case _                         => None
    case _ => None

  def recognize(input: String): Recognition = parse(input) match
    case Some(a) => Recognition.Recognized(a.url, a.label)
    case None =>
      input.trim match
        // Bare coordinates could be another URL scheme (http:foo), so they need the maven: scheme spelled out.
        case BareCoords(g, a, v) => Recognition.Invalid(s"for a Maven artifact, write maven:$g:$a${Option(v).fold("")(":" + _)}")
        case Url(host, _) if Set("central.sonatype.com", "mvnrepository.com", "repo1.maven.org", "repo.maven.apache.org")(host) =>
          Recognition.Invalid(s"not a Maven Central artifact URL: $input")
        case _ => Recognition.NotMine

/** What the plugin remembers of an artifact between checks. Stored as one line. */
enum MavenState:
  /** Whether one version can be resolved yet. */
  case Version(available: Boolean)
  /** Every version Maven Central lists for an artifact. */
  case Versions(versions: List[String])

  def encode: String = this match
    case Version(available) => s"maven-version ${if available then "available" else "pending"}"
    case Versions(versions) => ("maven-versions" :: versions).mkString(" ")

object MavenState:
  def decode(s: String): Either[String, MavenState] = s.split(' ').toList match
    case "maven-version" :: "available" :: Nil => Right(Version(true))
    case "maven-version" :: "pending" :: Nil   => Right(Version(false))
    case "maven-versions" :: versions          => Right(Versions(versions))
    case _                                     => Left(s"unreadable stored state: $s")

object MavenWatch:
  def describe(s: MavenState): String = s match
    case MavenState.Version(available) => if available then "available on Maven Central" else "not on Maven Central yet"
    case MavenState.Versions(versions) =>
      if versions.isEmpty then "no versions on Maven Central"
      else s"latest versions ${versions.takeRight(3).reverse.mkString(", ")}"

  def promise(artifact: MavenArtifact): String = artifact.version match
    case Some(_) => "This thread will get a message when the version can be resolved from Maven Central; then the subscription ends."
    case None    => "This thread will get a message each time Maven Central lists a new version."

  /** What to tell a subscriber since `previous`, and what to remember. A first check (`previous` empty) reports
    * nothing.
    */
  def check(artifact: MavenArtifact, previous: Option[MavenState], now: MavenState): CheckResult =
    val messages = (previous, now) match
      case (Some(MavenState.Version(false)), MavenState.Version(true)) =>
        List(s"${artifact.label} is now on Maven Central and can be resolved.")
      case (Some(MavenState.Versions(before)), MavenState.Versions(after)) =>
        val added = after.filterNot(before.toSet)
        val noun  = if added.size == 1 then "version" else "versions"
        if added.isEmpty then Nil
        else List(s"New $noun of ${artifact.group}:${artifact.artifact} on Maven Central: ${added.mkString(", ")}.")
      case _ => Nil
    CheckResult(
      state = now.encode,
      label = artifact.label,
      summary = describe(now),
      promise = promise(artifact),
      link = artifact.url,
      messages = messages,
      finished = now == MavenState.Version(true)
    )
