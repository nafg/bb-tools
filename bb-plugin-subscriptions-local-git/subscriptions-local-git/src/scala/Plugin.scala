package localgit

import bbplugin.*
import localgitcore.*
import subscriptionsource.{CheckRequest, CheckResult}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSImport}

given ExecutionContext = ExecutionContext.parasitic

@js.native
@JSImport("node:child_process", JSImport.Namespace)
private object ChildProcess extends js.Object:
  def execFile(
      file: String,
      args: js.Array[String],
      options: js.Any,
      callback: js.Function3[js.UndefOr[js.Dynamic], String, String, Unit]
  ): js.Any = js.native

/** `git` failed, or the repository or branch isn't there. */
final class GitException(message: String) extends Exception(message)

/** A finished `git` run: its exit code and output. */
private final case class GitRun(exitCode: Int, stdout: String, stderr: String)

private object Git:
  /** Runs git in `repo`. Fails only when git couldn't run at all; a non-zero exit is returned for the caller. */
  def run(repo: String, args: String*): Future[GitRun] =
    val promise = Promise[GitRun]()
    ChildProcess.execFile(
      "git",
      js.Array(("-C" +: repo +: args)*),
      js.Dynamic.literal("timeout" -> 30000, "maxBuffer" -> 10 * 1024 * 1024),
      (error: js.UndefOr[js.Dynamic], stdout: String, stderr: String) =>
        error.toOption.filter(e => e != null && !js.isUndefined(e)) match
          case None => promise.success(GitRun(0, stdout, stderr))
          case Some(e) if js.typeOf(e.code) == "number" =>
            promise.success(GitRun(e.code.asInstanceOf[Int], stdout, stderr))
          case Some(e) => promise.failure(GitException(s"git couldn't run: ${e.message}"))
    )
    promise.future

  def tip(b: LocalBranch): Future[String] =
    run(b.repo, "rev-parse", "--verify", "--quiet", s"refs/heads/${b.branch}^{commit}").flatMap { r =>
      if r.exitCode == 0 then Future.successful(r.stdout.trim)
      else
        // The subscriptions plugin already names the branch and repository; say only what went wrong.
        Future.failed(GitException(Option(r.stderr.trim).filter(_.nonEmpty).getOrElse("no such branch")))
    }

  /** How the branch moved from `from` to `to`, or None when git can't tell (the old commit is gone, for example). */
  def movement(b: LocalBranch, from: String, to: String): Future[Option[Movement]] =
    for
      ancestor <- run(b.repo, "merge-base", "--is-ancestor", from, to)
      count    <- run(b.repo, "rev-list", "--count", s"$from..$to")
      log      <- run(b.repo, "log", "--format=%s", "-n", "10", s"$from..$to")
    yield
      // merge-base answers 0 for yes and 1 for no; anything else means it couldn't tell.
      if !Set(0, 1)(ancestor.exitCode) || count.exitCode != 0 || log.exitCode != 0 then None
      else
        Some(
          Movement(
            count = count.stdout.trim.toIntOption.getOrElse(0),
            subjects = log.stdout.linesIterator.filter(_.nonEmpty).toList,
            rewritten = ancestor.exitCode == 1
          )
        )

private def check(request: CheckRequest): Future[CheckResult] =
  LocalBranch.parse(request.url) match
    case None => Future.failed(GitException(s"not a local branch: ${request.url}"))
    case Some(branch) =>
      request.previous.map(LocalBranchWatch.decode) match
        case Some(Left(e)) => Future.failed(GitException(e))
        case stored =>
          val previous = stored.flatMap(_.toOption)
          Git.tip(branch).flatMap { now =>
            val movement = previous match
              case Some(from) if from != now => Git.movement(branch, from, now)
              case _                         => Future.successful(None)
            movement.map(LocalBranchWatch.check(branch, previous, now, _))
          }

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit =
  subscriptionsourcerpc.SourceServer.register(bb, LocalBranch.supported, LocalBranch.recognize, check)
