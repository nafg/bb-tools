package subscriptions

import bbplugin.*
import subscriptionscore.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

private def messagePrefix(s: Subscription): String = s"[bb subscription #${s.id}]"

/** Checks every active subscription and tells each thread what changed since it last heard. */
final class Poller(bb: BbApi, store: Store):
  private var running = false

  def pollAll(): Future[Unit] =
    if running then Future.unit
    else
      running = true
      // Future.apply catches a synchronous throw, so the guard is always reset.
      Future(store.activeWithProblems())
        .flatMap { (subscriptions, problems) =>
          problems.foreach(bb.log.error)
          subscriptions.foldLeft(Future.unit)((prev, s) => prev.flatMap(_ => pollOne(s)))
        }
        .andThen(_ => running = false)

  private def send(threadId: String, text: String): Future[Unit] =
    bb.sdk.threads
      .send(
        js.Dynamic.literal(
          "threadId" -> threadId,
          "mode"     -> "auto",
          "input"    -> js.Array(js.Dynamic.literal("type" -> "text", "text" -> text))
        )
      )
      .toFuture
      .map(_ => ())

  private def isArchived(threadId: String): Future[Boolean] =
    bb.sdk.threads.get(js.Dynamic.literal("threadId" -> threadId)).toFuture.map { t =>
      val archivedAt = t.archivedAt
      !js.isUndefined(archivedAt) && archivedAt != null
    }

  /** An archived thread's subscriptions are skipped, so what happened meanwhile reaches it once it is unarchived. */
  private def pollOne(s: Subscription): Future[Unit] =
    isArchived(s.threadId)
      .flatMap { archived =>
        if archived then Future.unit
        else
          Forges.state(s.target).transformWith {
            case scala.util.Success(now) => report(s, now)
            case scala.util.Failure(e)   => reportCheckError(s, e.getMessage)
          }
      }
      .recover { case e => bb.log.warn(s"subscription #${s.id}: ${e.getMessage}") }

  /** A branch move is described with the commits it brought, when the forge can list them. */
  private def describe(s: Subscription, event: Event): Future[String] = (s.target, event) match
    case (Target.Branch(repo, _), Event.BranchMoved(from, to)) =>
      Forges.commitsBetween(repo, from, to)
        .map(Some(_))
        .recover { case e =>
          bb.log.warn(s"subscription #${s.id}: could not list commits $from..$to: ${e.getMessage}")
          None
        }
        .map(Watch.describe(s.target, event, _))
    case _ => Future.successful(Watch.describe(s.target, event))

  private def report(s: Subscription, now: WatchState): Future[Unit] =
    val events   = Watch.between(s.lastSeen, now)
    val finished = Watch.isFinished(now)
    Future.sequence(events.map(describe(s, _))).flatMap { described =>
      val lines =
        s.checkError.map(_ => s"Checking ${s.target.label} works again.").toList ++
          described ++
          (if finished then List("This subscription has ended.") else Nil)
      if lines.isEmpty then Future.unit
      else send(s.threadId, (messagePrefix(s) :: lines).mkString(" ") + s"\n${s.target.url}")
    }.map { _ =>
      // Recorded only after delivery, so a failed send is retried on the next check.
      store.recordSeen(s.id, now, finished)
    }

  private def reportCheckError(s: Subscription, error: String): Future[Unit] =
    val announce =
      if s.checkError.isDefined then Future.unit
      else
        send(
          s.threadId,
          s"${messagePrefix(s)} Checking ${s.target.label} failed: $error\n" +
            "The subscription stays active. This thread will hear when checking works again."
        )
    announce.map(_ => store.recordCheckError(s.id, error))

/** The actions the agent tools and the CLI share. Each returns the text to show the caller. */
final class Actions(store: Store):
  def add(threadId: String, url: String): Future[Either[String, String]] =
    Target.parse(url) match
      case Left(e) => Future.successful(Left(e))
      case Right(target) =>
        store.find(threadId, target) match
          case Some(existing) =>
            Future.successful(
              Right(s"Already subscribed as #${existing.id}. Last known: ${Watch.describe(existing.lastSeen)}.")
            )
          case None =>
            Forges.state(target).map { now =>
              if Watch.isFinished(now) then
                Right(s"${target.label} is already ${Watch.describe(now)}. Nothing to wait for, so no subscription was made.")
              else
                val id = store.add(threadId, target, now)
                Right(
                  s"Subscribed as #$id to ${target.label}. Now: ${Watch.describe(now)}. ${Watch.promise(target)} " +
                    "Checks run every minute."
                )
            }.recover { case e => Left(s"Could not read ${target.label}: ${e.getMessage}") }

  def list(subscriptions: Seq[Subscription], showThread: Boolean): String =
    if subscriptions.isEmpty then "No active subscriptions."
    else
      subscriptions.map { s =>
        val thread = if showThread then s" (thread ${s.threadId})" else ""
        val error  = s.checkError.fold("")(e => s" Checking is failing: $e")
        s"#${s.id}$thread ${s.target.label}: ${Watch.describe(s.lastSeen)}.$error\n  ${s.target.url}"
      }.mkString("\n")

  /** Cancels a subscription; `threadId` restricts it to that thread's own subscriptions. */
  def cancel(id: Int, threadId: Option[String]): Either[String, String] =
    store.get(id).filter(s => threadId.forall(_ == s.threadId)) match
      case None => Left(s"No active subscription #$id${threadId.fold("")(_ => " for this thread")}.")
      case Some(s) =>
        store.end(id)
        Right(s"Canceled subscription #$id to ${s.target.label}.")

private def textResult(text: String, isError: Boolean = false): js.Dynamic =
  js.Dynamic.literal(
    "content" -> js.Array(js.Dynamic.literal("type" -> "text", "text" -> text)),
    "isError" -> isError
  )

private def fromEither(e: Either[String, String]): js.Dynamic = e.fold(textResult(_, isError = true), textResult(_))

private def registerTools(bb: BbApi, store: Store, actions: Actions): Unit =
  bb.agents.registerTool(
    js.Dynamic.literal(
      "name" -> "subscriptions_add",
      "description" ->
        ("Subscribe this thread to something on GitHub or GitLab, given its URL, and return at once with its " +
          "current state. A pull or merge request: a message when it is merged or closed, or when CI finishes on " +
          "a new result or commit; the subscription ends when it is merged or closed. A branch (…/tree/<branch>): " +
          "a message with the new commits each time it moves. A repository's tags (…/tags): a message for each " +
          "new tag. bb checks every minute. If checking fails, this thread is told instead of hearing nothing."),
      "instructions" ->
        ("To wait for a pull or merge request to merge, close or finish CI, for new commits on a branch, or for " +
          "a new tag, call subscriptions_add instead of polling gh or glab in a loop, a background command or a " +
          "gitlab-watch. Then end your turn: a message arrives when something happens."),
      "presentation" -> js.Dynamic.literal(
        "label" -> js.Dynamic.literal("pending" -> "Subscribing", "completed" -> "Subscribed")
      ),
      "parameters" -> js.Dynamic.literal(
        "type" -> "object",
        "properties" -> js.Dynamic.literal(
          "url" -> js.Dynamic.literal(
            "type"        -> "string",
            "description" -> s"The URL: ${Target.supported}"
          )
        ),
        "required"             -> js.Array("url"),
        "additionalProperties" -> false
      ),
      "execute" -> ((input: js.Dynamic, ctx: js.Dynamic) =>
        actions.add(ctx.threadId.asInstanceOf[String], input.url.asInstanceOf[String]).map(fromEither).toJSPromise
      )
    )
  )
  bb.agents.registerTool(
    js.Dynamic.literal(
      "name"        -> "subscriptions_list",
      "description" -> "List this thread's active subscriptions with what was last seen for each.",
      "presentation" -> js.Dynamic.literal(
        "label" -> js.Dynamic.literal("pending" -> "Listing subscriptions", "completed" -> "Listed subscriptions")
      ),
      "parameters" -> js.Dynamic.literal(
        "type"                 -> "object",
        "properties"           -> js.Dynamic.literal(),
        "additionalProperties" -> false
      ),
      "execute" -> ((_: js.Dynamic, ctx: js.Dynamic) =>
        textResult(actions.list(store.activeFor(ctx.threadId.asInstanceOf[String]), showThread = false))
      )
    )
  )
  bb.agents.registerTool(
    js.Dynamic.literal(
      "name"        -> "subscriptions_cancel",
      "description" -> "Cancel one of this thread's subscriptions by its number.",
      "presentation" -> js.Dynamic.literal(
        "label" -> js.Dynamic.literal("pending" -> "Canceling subscription", "completed" -> "Canceled subscription")
      ),
      "parameters" -> js.Dynamic.literal(
        "type" -> "object",
        "properties" -> js.Dynamic.literal(
          "id" -> js.Dynamic.literal("type" -> "integer", "description" -> "The subscription number")
        ),
        "required"             -> js.Array("id"),
        "additionalProperties" -> false
      ),
      "execute" -> ((input: js.Dynamic, ctx: js.Dynamic) =>
        fromEither(actions.cancel(input.id.asInstanceOf[Double].toInt, Some(ctx.threadId.asInstanceOf[String])))
      )
    )
  )

private val Usage =
  """Usage:
    |  bb subscriptions add <url> [--thread <thread-id>]
    |    <url>: a PR or MR, a branch (…/tree/<branch>), or a repository's tags (…/tags)
    |  bb subscriptions list [--thread <thread-id> | --all]
    |  bb subscriptions cancel <id>""".stripMargin

private def cliOk(stdout: String): js.Dynamic  = js.Dynamic.literal("exitCode" -> 0, "stdout" -> (stdout + "\n"))
private def cliErr(message: String): js.Dynamic = js.Dynamic.literal("exitCode" -> 1, "stderr" -> (message + "\n"))

private def runCli(store: Store, actions: Actions, argv: List[String], ctx: js.Dynamic): Future[js.Dynamic] =
  val invokingThread = Option(ctx.threadId.asInstanceOf[js.UndefOr[String]].orNull)
  def threadArg(rest: List[String]): Either[String, (Option[String], List[String])] = rest match
    case "--thread" :: id :: tail => Right(Some(id) -> tail)
    case "--thread" :: Nil        => Left("--thread needs a thread id")
    case other                    => Right(None -> other)
  argv match
    case "add" :: rest =>
      val parsed = rest match
        case url :: tail => threadArg(tail).map((t, extra) => (url, t, extra))
        case Nil         => Left(Usage)
      parsed match
        case Left(e)                  => Future.successful(cliErr(e))
        case Right((_, _, extra)) if extra.nonEmpty => Future.successful(cliErr(s"unexpected argument: ${extra.head}"))
        case Right((url, thread, _)) =>
          thread.orElse(invokingThread) match
            case None      => Future.successful(cliErr("Run this from a bb thread, or pass --thread <thread-id>."))
            case Some(tid) => actions.add(tid, url).map(_.fold(cliErr, cliOk))
    case "list" :: "--all" :: Nil => Future.successful(cliOk(actions.list(store.active(), showThread = true)))
    case "list" :: rest =>
      Future.successful(threadArg(rest) match
        case Left(e)                              => cliErr(e)
        case Right((_, extra)) if extra.nonEmpty => cliErr(s"unexpected argument: ${extra.head}")
        case Right((thread, _)) =>
          thread.orElse(invokingThread) match
            case None      => cliOk(actions.list(store.active(), showThread = true))
            case Some(tid) => cliOk(actions.list(store.activeFor(tid), showThread = false))
      )
    case "cancel" :: id :: Nil =>
      Future.successful(id.stripPrefix("#").toIntOption match
        case None    => cliErr(s"not a subscription number: $id")
        case Some(n) => actions.cancel(n, None).fold(cliErr, cliOk)
      )
    case ("--help" | "-h" | "help") :: _ => Future.successful(cliOk(Usage))
    case _                               => Future.successful(cliErr(Usage))

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit =
  val db = bb.storage.database()
  bb.storage.migrate(db, Store.migrations)
  val store   = Store(db)
  val actions = Actions(store)
  val poller  = Poller(bb, store)

  registerTools(bb, store, actions)

  bb.cli.register(
    js.Dynamic.literal(
      "name"    -> "subscriptions",
      "summary" -> "Subscribe a thread to a PR, MR, branch or tags; it is messaged when something changes",
      "commands" -> js.Array(
        js.Dynamic.literal(
          "name"    -> "add",
          "summary" -> "Subscribe this thread (or --thread) to a GitHub or GitLab PR, MR, branch or tags URL",
          "usage"   -> "bb subscriptions add <url> [--thread <thread-id>]"
        ),
        js.Dynamic.literal(
          "name"    -> "list",
          "summary" -> "List this thread's active subscriptions, another thread's, or all of them",
          "usage"   -> "bb subscriptions list [--thread <thread-id> | --all]"
        ),
        js.Dynamic.literal(
          "name"    -> "cancel",
          "summary" -> "Cancel a subscription by number",
          "usage"   -> "bb subscriptions cancel <id>"
        )
      ),
      "run" -> ((argv: js.Array[String], ctx: js.Dynamic) =>
        runCli(store, actions, argv.toList, ctx).recover { case e => cliErr(e.getMessage) }.toJSPromise
      )
    )
  )

  bb.events.on("thread.deleted", (payload: js.Dynamic) => store.endAllFor(payload.thread.id.asInstanceOf[String]))

  bb.background.schedule("poll", "* * * * *", () => poller.pollAll().toJSPromise)
