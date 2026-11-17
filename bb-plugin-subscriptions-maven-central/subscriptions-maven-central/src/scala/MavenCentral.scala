package mavencentral

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

/** Maven Central answered something other than "found" or "not found". */
final class MavenCentralException(message: String) extends Exception(message)

/** Reads Maven Central's repository directly. Its search API can lag days behind what builds can resolve. */
object MavenCentral:
  private val Base = "https://repo1.maven.org/maven2"

  private def artifactUrl(group: String, artifact: String) = s"$Base/${group.replace('.', '/')}/$artifact"

  /** Fetches without the CDN's cached copy, so a version that just appeared isn't hidden by a cached "not found". */
  private def fetch(url: String, method: String): Future[js.Dynamic] =
    val busted = s"$url?bb=${js.Date.now().toLong}"
    js.Dynamic.global.fetch(busted, js.Dynamic.literal("method" -> method)).asInstanceOf[js.Promise[js.Dynamic]].toFuture

  /** Whether the version's POM can be downloaded, which is what a build resolving that version needs. */
  def isAvailable(group: String, artifact: String, version: String)(using ExecutionContext): Future[Boolean] =
    fetch(s"${artifactUrl(group, artifact)}/$version/$artifact-$version.pom", "HEAD").flatMap { r =>
      r.status.asInstanceOf[Int] match
        case 200 => Future.successful(true)
        case 404 => Future.successful(false)
        case other => Future.failed(MavenCentralException(s"Maven Central answered HTTP $other"))
    }

  /** Every version the artifact's maven-metadata.xml lists, oldest first; empty if the artifact doesn't exist yet. */
  def versions(group: String, artifact: String)(using ExecutionContext): Future[List[String]] =
    fetch(s"${artifactUrl(group, artifact)}/maven-metadata.xml", "GET").flatMap { r =>
      r.status.asInstanceOf[Int] match
        case 200 =>
          r.text().asInstanceOf[js.Promise[String]].toFuture.map { xml =>
            """<version>\s*([^<\s]+)\s*</version>""".r.findAllMatchIn(xml).map(_.group(1)).toList
          }
        case 404   => Future.successful(Nil)
        case other => Future.failed(MavenCentralException(s"Maven Central answered HTTP $other"))
    }
