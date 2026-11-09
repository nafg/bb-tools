package billing

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:child_process", JSImport.Namespace)
private object ChildProcess extends js.Object:
  def execFile(
    file: String,
    args: js.Array[String],
    options: js.Any,
    callback: js.Function3[js.UndefOr[js.Dynamic], String, String, Unit]): js.Any = js.native

/** `git` run on this machine. */
object Git:
  /** The output of `git args`, or a failure with git's message. */
  def apply(args: String*): Future[String] =
    val promise = Promise[String]()
    val _       = ChildProcess.execFile(
      "git",
      js.Array(args*),
      js.Dynamic.literal("timeout" -> 30_000),
      (error: js.UndefOr[js.Dynamic], stdout: String, stderr: String) =>
        error.toOption.filter(e => e != null && !js.isUndefined(e)) match
          case None    => promise.success(stdout.trim)
          case Some(e) =>
            val detail = Option(stderr).map(_.trim).filter(_.nonEmpty).getOrElse(e.message.asInstanceOf[String])
            promise.failure(new RuntimeException(s"git ${args.mkString(" ")} failed: $detail"))
    )
    promise.future
