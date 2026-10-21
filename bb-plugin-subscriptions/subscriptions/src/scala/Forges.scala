package subscriptions

import subscriptionscore.*
import subscriptionscore.Snapshot.GitHubCheck

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:child_process", JSImport.Namespace)
private object ChildProcess extends js.Object:
  def execFile(
      file: String,
      args: js.Array[String],
      options: js.Any,
      callback: js.Function3[js.UndefOr[js.Dynamic], String, String, Unit]
  ): js.Any = js.native

/** A forge CLI (`gh` or `glab`) could not be run, or exited with an error. */
final class ForgeCliException(message: String) extends Exception(message)

/** Reads pull and merge requests through the `gh` and `glab` CLIs, so their existing logins are used. */
object Forges:
  private def run(command: String, args: String*): Future[String] =
    val promise = Promise[String]()
    val options = js.Dynamic.literal("timeout" -> 60000, "maxBuffer" -> 20 * 1024 * 1024)
    ChildProcess.execFile(
      command,
      js.Array(args*),
      options,
      (error: js.UndefOr[js.Dynamic], stdout: String, stderr: String) =>
        error.toOption.filter(e => e != null && !js.isUndefined(e)) match
          case None => promise.success(stdout)
          case Some(e) =>
            val detail = Option(stderr).map(_.trim).filter(_.nonEmpty).getOrElse(e.message.asInstanceOf[String])
            promise.failure(ForgeCliException(s"$command ${args.headOption.getOrElse("")} failed: $detail"))
    )
    promise.future

  private def str(v: js.Dynamic): Option[String] =
    if js.isUndefined(v) || v == null then None
    else Some(v.asInstanceOf[String]).filter(_.nonEmpty)

  private def orFail[A](e: Either[String, A]): Future[A] =
    e.fold(msg => Future.failed(ForgeCliException(msg)), Future.successful)

  def snapshot(cr: ChangeRequest)(using ExecutionContext): Future[Snapshot] = cr.forge match
    case Forge.GitHub => gitHub(cr)
    case Forge.GitLab => gitLab(cr)

  private def gitHub(cr: ChangeRequest)(using ExecutionContext): Future[Snapshot] =
    run("gh", "pr", "view", cr.url, "--json", "state,headRefOid,statusCheckRollup").flatMap { out =>
      val json = js.JSON.parse(out)
      val checks = json.statusCheckRollup.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].toOption
        .filter(_ != null).map(_.toSeq).getOrElse(Nil).map { c =>
          if str(c.__typename).contains("StatusContext") then GitHubCheck.StatusContext(str(c.state).getOrElse(""))
          else GitHubCheck.CheckRun(str(c.status).getOrElse(""), str(c.conclusion))
        }
      orFail(
        for
          lifecycle <- Snapshot.lifecycleFromGitHub(str(json.state).getOrElse(""))
          ci        <- Snapshot.ciFromGitHub(checks)
        yield Snapshot(lifecycle, ci, str(json.headRefOid).getOrElse(""))
      )
    }

  private def gitLab(cr: ChangeRequest)(using ExecutionContext): Future[Snapshot] =
    val project = js.URIUtils.encodeURIComponent(cr.project)
    run("glab", "api", "--hostname", cr.host, s"projects/$project/merge_requests/${cr.number}").flatMap { out =>
      val json     = js.JSON.parse(out)
      val pipeline = json.head_pipeline
      val status   = if js.isUndefined(pipeline) || pipeline == null then None else str(pipeline.status)
      orFail(
        for
          lifecycle <- Snapshot.lifecycleFromGitLab(str(json.state).getOrElse(""))
          ci        <- Snapshot.ciFromGitLab(status)
        yield Snapshot(lifecycle, ci, str(json.sha).getOrElse(""))
      )
    }
