package wbs

import bbplugin.*
import wbscore.{Html, Wbs}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

private def renderFile(source: String, depth: Option[Int]): Either[String, String] =
  Wbs.parse(source).map(Html.render(_, depth)).left.map(_.getMessage)

private def getThread(bb: BbApi, threadId: String): Future[ThreadInfo] =
  bb.sdk.threads.get(js.Dynamic.literal("threadId" -> threadId)).toFuture.map(_.asInstanceOf[ThreadInfo])

/** The host the invoking thread's environment lives on; None = the server's own host. */
private def invokingHostId(bb: BbApi, threadId: Option[String]): Future[Option[String]] =
  threadId.fold(Future.successful(None)) { tid =>
    getThread(bb, tid).flatMap { thread =>
      thread.environmentId.nonNull.fold(Future.successful(None)) { envId =>
        bb.sdk.environments
          .get(js.Dynamic.literal("environmentId" -> envId))
          .toFuture
          .map(env => Some(env.asInstanceOf[EnvironmentInfo].hostId))
      }
    }
  }

private def readHostFile(bb: BbApi, hostId: Option[String], path: String, rootPath: Option[String] = None): Future[String] =
  val args = js.Dynamic.literal("path" -> path)
  hostId.foreach(h => args.updateDynamic("hostId")(h))
  rootPath.foreach(r => args.updateDynamic("rootPath")(r))
  bb.sdk.files.read(args).toFuture.map(f => decodeContent(f.asInstanceOf[FileContent]))

private def decodeContent(file: FileContent): String =
  file.contentEncoding match
    case "base64" => js.Dynamic.global.Buffer.from(file.content, "base64").applyDynamic("toString")("utf8").asInstanceOf[String]
    case _        => file.content

/** Reads the file a `fileOpener` was given, per its source kind. */
private def readOpenedFile(bb: BbApi, path: String, source: OpenerSource): Future[String] =
  source.kind match
    case "workspace" =>
      // The opener may omit projectId; the thread knows its project.
      val projectId = source.projectId.nonNull match
        case Some(pid) => Future.successful(pid)
        case None =>
          source.threadId.nonNull.fold(Future.failed(new Exception("workspace file without project or thread")))(
            getThread(bb, _).map(_.projectId)
          )
      projectId.flatMap { pid =>
        val args = js.Dynamic.literal("projectId" -> pid, "path" -> path)
        source.environmentId.nonNull.foreach(e => args.updateDynamic("environmentId")(e))
        source.experimental_hostId.foreach(h => args.updateDynamic("hostId")(h))
        bb.sdk.projects.fileContent(args).toFuture.map(f => decodeContent(f.asInstanceOf[FileContent]))
      }
    case "host" =>
      invokingHostId(bb, source.threadId.nonNull).flatMap(readHostFile(bb, _, path))
    case "thread-storage" =>
      source.threadId.nonNull.fold(Future.failed(new Exception("thread-storage file without thread"))) { tid =>
        bb.sdk.threads.storageLocation(js.Dynamic.literal("threadId" -> tid)).toFuture.flatMap { raw =>
          val loc  = raw.asInstanceOf[StorageLocation]
          val root = loc.storageRootPath.stripSuffix("/")
          readHostFile(bb, Some(loc.hostId), s"$root/$path", rootPath = Some(root))
        }
      }
    case other => Future.failed(new Exception(s"unsupported file source: $other"))

private object Schema:
  private def make(validate: js.Function1[js.Any, js.Any]): js.Any =
    js.Dynamic.literal(
      "~standard" -> js.Dynamic.literal("version" -> 1, "vendor" -> "bb-plugin-wbs", "validate" -> validate)
    )
  val passthrough: js.Any =
    make(js.Any.fromFunction1(value => js.Dynamic.literal("value" -> value.asInstanceOf[js.Any])))

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit =
  bb.rpc.register(
    js.Dynamic.literal("render" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough)),
    js.Dynamic.literal(
      "render" -> js.Any.fromFunction1 { (raw: js.Any) =>
        val input = raw.asInstanceOf[RenderInput]
        val path  = input.path
        val depth = input.depth.toOption.map(_.toInt).filter(_ >= 1)
        readOpenedFile(bb, path, input.source)
          .map(source => renderFile(source, depth).fold(Html.errorFragment, identity))
          .recover { case e => Html.errorFragment(e.getMessage) }
          .map(html => js.Dynamic.literal("html" -> html))
          .toJSPromise
      }
    )
  )

  bb.cli.register(
    js.Dynamic.literal(
      "name"    -> "wbs",
      "summary" -> "Render a *.wbs.yaml estimate (see the wbs skill) as HTML",
      "commands" -> js.Array(
        js.Dynamic.literal(
          "name"    -> "html",
          "summary" -> "Print the estimate as an HTML fragment (inline-styled tree table with totals, plus open questions) suitable for embedding in a document or email; --depth collapses deeper items into their parent's total",
          "usage"   -> "bb wbs html <file.wbs.yaml> [--depth N]"
        )
      ),
      "run" -> ((argv: js.Array[String], ctx: js.Any) => run(bb, argv.toList, ctx.asInstanceOf[CliContext]).toJSPromise)
    )
  )

private def ok(stdout: String): js.Dynamic  = js.Dynamic.literal("exitCode" -> 0, "stdout" -> stdout)
private def err(message: String): js.Dynamic = js.Dynamic.literal("exitCode" -> 1, "stderr" -> message)

private def run(bb: BbApi, argv: List[String], ctx: CliContext): Future[js.Dynamic] =
  val result = argv match
    case "html" :: rest =>
      parseHtmlArgs(rest) match
        case Left(message) => Future.successful(err(message))
        case Right((file, depth)) =>
          val path = if file.startsWith("/") then file else ctx.cwd.fold(file)(c => s"${c.stripSuffix("/")}/$file")
          for
            hostId <- invokingHostId(bb, ctx.threadId.toOption)
            source <- readHostFile(bb, hostId, path)
          yield renderFile(source, depth).fold(err, ok)
    case other => Future.successful(err(s"unknown command: ${other.mkString(" ")}\nUsage: bb wbs html <file.wbs.yaml> [--depth N]"))
  result.recover { case e => err(e.getMessage) }

private def parseHtmlArgs(args: List[String]): Either[String, (String, Option[Int])] =
  @annotation.tailrec
  def loop(rest: List[String], file: Option[String], depth: Option[Int]): Either[String, (String, Option[Int])] =
    rest match
      case "--depth" :: n :: tail =>
        n.toIntOption.filter(_ >= 1) match
          case Some(d) => loop(tail, file, Some(d))
          case None    => Left(s"--depth must be a positive integer, got '$n'")
      case "--depth" :: Nil          => Left("--depth requires a value")
      case f :: tail if file.isEmpty => loop(tail, Some(f), depth)
      case f :: _                    => Left(s"unexpected argument '$f'")
      case Nil                       => file.toRight("Usage: bb wbs html <file.wbs.yaml> [--depth N]").map(_ -> depth)
  loop(args, None, None)
