package subscriptions

import bbplugin.*
import subscriptionscore.*
import subscriptionsource.{CheckRequest, CheckResult, Recognition}
import subscriptionsourcerpc.SourceClient

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

private def messagePrefix(s: Subscription): String = s"[bb subscription #${s.id}]"

/** A plugin that checks one kind of target. This plugin is itself the source for bb threads; the others are
  * plugins that registered the source methods (see `subscriptionsource.SourceMethods`).
  */
trait Source:
  def id: String
  /** Which targets this source watches, in the form a user writes them. */
  def description: String
  def recognize(input: String): Future[Recognition]
  def check(request: CheckRequest): Future[CheckResult]

/** Threads, read through bb itself. */
final class ThreadSource(bb: BbApi) extends Source:
  val id          = ThreadSource.Id
  val description = ThreadArchived.supported

  private def present(v: js.Dynamic): Option[js.Dynamic] = if js.isUndefined(v) || v == null then None else Some(v)

  def recognize(input: String): Future[Recognition] = Future.successful(ThreadArchived.recognize(input))

  def check(request: CheckRequest): Future[CheckResult] =
    ThreadArchived.parse(request.url) match
      case None => Future.failed(IllegalArgumentException(s"not a thread target: ${request.url}"))
      case Some(target) =>
        request.previous.map(ThreadWatch.decode) match
          case Some(Left(e)) => Future.failed(IllegalStateException(e))
          case stored =>
            val previous = stored.flatMap(_.toOption)
            bb.sdk.threads.get(js.Dynamic.literal("threadId" -> target.threadId)).toFuture.flatMap { t =>
              val life =
                if present(t.deletedAt).isDefined then ThreadLife.Deleted
                else if present(t.archivedAt).isDefined then ThreadLife.Archived
                else ThreadLife.Open
              val title = Seq(t.title, t.titleFallback).flatMap(present).map(_.asInstanceOf[String]).find(_.nonEmpty)
                .getOrElse("(untitled)")
              // The last reply is read only once the thread has finished, since only that message quotes it.
              val reply =
                if life == ThreadLife.Open then Future.successful(None)
                else
                  bb.sdk.threads.output(js.Dynamic.literal("threadId" -> target.threadId)).toFuture
                    .map(o => present(o.output).map(_.asInstanceOf[String]))
              reply.map(ThreadWatch.check(target, previous, life, title, _))
            }

object ThreadSource:
  /** The source id stored for thread subscriptions: this plugin's own. */
  val Id = "subscriptions"

/** Another plugin's source, called through its registered methods. */
final class RemoteSource(client: SourceClient) extends Source:
  def id          = client.pluginId
  def description = client.description
  def recognize(input: String): Future[Recognition] = client.recognize(input)
  def check(request: CheckRequest): Future[CheckResult] = client.check(request)

/** Every source: the built-in one for threads, and each installed plugin that registered as a source. */
final class Sources(bb: BbApi):
  private val threads = ThreadSource(bb)

  def all(): Future[List[Source]] = SourceClient.discover(bb).map(clients => threads :: clients.map(RemoteSource(_)))

  /** The source a stored subscription names. Fails, with that plugin named, when it isn't installed and enabled. */
  def byId(id: String): Future[Source] =
    if id == ThreadSource.Id then Future.successful(threads)
    else
      all().flatMap(_.find(_.id == id) match
        case Some(s) => Future.successful(s)
        case None    => Future.failed(IllegalStateException(s"the $id plugin, which checks this target, isn't installed and enabled"))
      )

  /** The source that watches `input`, with the target's canonical form. Sources are asked in turn; the first to
    * recognize it wins. When none does, the error gives every reason a source gave, or else what each accepts.
    */
  def recognize(input: String): Future[Either[String, (Source, Recognition.Recognized)]] =
    all().flatMap { sources =>
      def loop(rest: List[Source], invalid: List[String]): Future[Either[String, (Source, Recognition.Recognized)]] =
        rest match
          case Nil =>
            val reason =
              if invalid.nonEmpty then invalid.mkString("; ")
              else "accepted are " + sources.map(_.description).mkString("; ")
            Future.successful(Left(s"can't subscribe to $input: $reason"))
          case source :: tail =>
            source.recognize(input).recover { case _ => Recognition.NotMine }.flatMap {
              case r: Recognition.Recognized => Future.successful(Right(source -> r))
              case Recognition.Invalid(why)  => loop(tail, invalid :+ why)
              case Recognition.NotMine       => loop(tail, invalid)
            }
      loop(sources, Nil)
    }

/** Checks every active subscription and tells each thread what changed since it last heard. */
final class Poller(bb: BbApi, store: Store, sources: Sources):
  private var running = false
  private var runs    = 0

  /** Sources read some things only when something else changed (a forge reads reviews and comments only when the
    * request's updated time changes). Every this many runs they're asked to read everything, in case a change
    * slipped past that trigger.
    */
  private val ForceEvery = 15

  def pollAll(): Future[Unit] =
    if running then Future.unit
    else
      running = true
      runs += 1
      val force = runs % ForceEvery == 0
      // Future.apply catches a synchronous throw, so the guard is always reset.
      Future(store.active())
        .flatMap(subscriptions => subscriptions.foldLeft(Future.unit)((prev, s) => prev.flatMap(_ => pollOne(s, force))))
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

  private val RetryDelayMs = 5000

  private def delay(ms: Int): Future[Unit] =
    val p = scala.concurrent.Promise[Unit]()
    val _ = js.timers.setTimeout(ms.toDouble)(p.success(()))
    p.future

  /** Checks the target, trying once more a few seconds later if the first attempt fails. Sources fail now and then
    * for a moment (a timeout, a server error), and a thread shouldn't hear about a failure the next attempt doesn't
    * repeat.
    */
  private def checkWithRetry(s: Subscription, force: Boolean): Future[CheckResult] =
    def attempt() = sources.byId(s.source).flatMap(_.check(CheckRequest(s.url, Some(s.state), s.threadId, force)))
    attempt().recoverWith { case first =>
      bb.log.warn(s"subscription #${s.id}: check failed, retrying once: ${first.getMessage}")
      delay(RetryDelayMs).flatMap(_ => attempt())
    }

  /** An archived thread's subscriptions end. The archive event normally ends them; this catches a thread archived
    * while the plugin wasn't running.
    */
  private def pollOne(s: Subscription, force: Boolean): Future[Unit] =
    isArchived(s.threadId)
      .flatMap { archived =>
        if archived then Future.successful(store.endAllFor(s.threadId))
        else
          checkWithRetry(s, force).transformWith {
            case scala.util.Success(result) => report(s, result)
            case scala.util.Failure(e)      => reportCheckError(s, e.getMessage)
          }
      }
      .recover { case e => bb.log.warn(s"subscription #${s.id}: ${e.getMessage}") }

  private def report(s: Subscription, result: CheckResult): Future[Unit] =
    val lines =
      s.checkError.map(_ => s"Checking ${result.label} works again.").toList ++
        result.messages ++
        (if result.finished then List("This subscription has ended.") else Nil)
    val delivered =
      if lines.isEmpty then Future.unit
      else send(s.threadId, s"${messagePrefix(s)} ${lines.mkString("\n")}\n${result.link}")
    // Recorded only after delivery, so a failed send is retried on the next check.
    delivered.map(_ => store.recordCheck(s.id, result))

  private def reportCheckError(s: Subscription, error: String): Future[Unit] =
    val announce =
      if s.checkError.isDefined then Future.unit
      else
        send(
          s.threadId,
          s"${messagePrefix(s)} Checking ${s.label} failed: $error\n" +
            "The subscription stays active. This thread will hear when checking works again."
        )
    announce.map(_ => store.recordCheckError(s.id, error))

/** The actions the agent tools and the CLI share. Each returns the text to show the caller. */
final class Actions(bb: BbApi, store: Store, sources: Sources):
  private def threadTitle(threadId: String): Future[String] =
    bb.sdk.threads.get(js.Dynamic.literal("threadId" -> threadId)).toFuture
      .map { t =>
        Seq(t.title, t.titleFallback)
          .collectFirst { case v if !js.isUndefined(v) && v != null && v.asInstanceOf[String].nonEmpty => v.asInstanceOf[String] }
          .getOrElse("(untitled)")
      }
      .recover { case _ => "(thread not found)" }

  /** Threads that subscribed to `input`, now or before. Subscribing suggests, but doesn't prove, that a thread is
    * working on the target.
    */
  def who(input: String): Future[Either[String, String]] =
    sources.recognize(input).flatMap {
      case Left(e) => Future.successful(Left(e))
      case Right((_, target)) =>
        val subscribers = store.subscribersOf(target.url)
        if subscribers.isEmpty then Future.successful(Right(s"No thread has subscribed to ${target.label}."))
        else
          Future.traverse(subscribers) { s =>
            threadTitle(s.threadId).map { title =>
              val date   = new js.Date(s.since * 1000.0).toISOString().take(10)
              val status = if s.active then s"subscribed since $date" else s"subscribed on $date, since ended"
              s"@thread:${s.threadId} $title ($status)"
            }
          }.map(lines => Right(s"Threads that subscribed to ${target.label}:\n" + lines.mkString("\n")))
    }

  def add(threadId: String, input: String): Future[Either[String, String]] =
    sources.recognize(input).flatMap {
      case Left(e) => Future.successful(Left(e))
      case Right((_, target)) if target.url == ThreadArchived(threadId).url =>
        Future.successful(Left("A thread can't wait for itself."))
      case Right((source, target)) =>
        store.find(threadId, target.url) match
          case Some(existing) =>
            Future.successful(
              Right(s"Already subscribed as #${existing.id}. Last known: ${existing.summary.getOrElse("not checked yet")}.")
            )
          case None =>
            source.check(CheckRequest(target.url, None, threadId, force = true)).map { first =>
              if first.finished then
                Right(s"${first.label} is already ${first.summary}. Nothing to wait for, so no subscription was made.")
              else
                val id = store.add(threadId, source.id, target.url, first)
                Right(s"Subscribed as #$id to ${first.label}. Now: ${first.summary}. ${first.promise} Checks run every minute.")
            }.recover { case e => Left(s"Could not read ${target.label}: ${e.getMessage}") }
    }

  def list(subscriptions: Seq[Subscription], showThread: Boolean): String =
    if subscriptions.isEmpty then "No active subscriptions."
    else
      subscriptions.map { s =>
        val thread = if showThread then s" (thread ${s.threadId})" else ""
        val error  = s.checkError.fold("")(e => s" Checking is failing: $e")
        s"#${s.id}$thread ${s.label}: ${s.summary.getOrElse("not checked yet")}.$error\n  ${s.url}"
      }.mkString("\n")

  /** Cancels a subscription; `threadId` restricts it to that thread's own subscriptions. */
  def cancel(id: Int, threadId: Option[String]): Either[String, String] =
    store.get(id).filter(s => threadId.forall(_ == s.threadId)) match
      case None => Left(s"No active subscription #$id${threadId.fold("")(_ => " for this thread")}.")
      case Some(s) =>
        store.end(id)
        Right(s"Canceled subscription #$id to ${s.label}.")

/** What the tools accept. Each kind comes from a source plugin; one that isn't installed won't recognize its own. */
private val SupportedTargets =
  "What to subscribe to: a GitHub pull request or GitLab merge request URL, a branch URL (…/tree/<branch>), a " +
    "repository's tags URL (…/tags), a Maven Central artifact as maven:group:artifact[:version] or its URL, a local " +
    "git branch as git:/absolute/path/to/repo#branch, or another " +
    "bb thread being archived as bb:thread/thr_…/archived"

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
        ("Subscribe this thread to something, given its URL or reference, and return at once with its " +
          "current state. A pull or merge request: a message when it is merged or closed, when CI finishes on " +
          "a new result or commit, when its latest commit changes (push, rebase, force-push), and for each new " +
          "review or comment, with its text; the subscription ends when it is merged or closed. A branch (…/tree/<branch>): " +
          "a message with the new commits each time it moves. A repository's tags (…/tags): a message for each " +
          "new tag. A Maven Central artifact (maven:group:artifact:version or its URL): a message when that version can be " +
          "resolved; without a version, each new version. Another bb thread being archived (bb:thread/thr_…/archived): " +
          "a message with its last reply when it is archived or deleted. bb checks every minute. If checking fails, this thread is told instead of hearing nothing."),
      "instructions" ->
        ("To wait for a pull or merge request to merge, close, finish CI or get reviews and comments, for new " +
          "commits on a branch, for a Maven Central release, for another thread to finish, or for " +
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
            "description" -> SupportedTargets
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
      "name" -> "subscriptions_who",
      "description" ->
        ("List the threads that subscribed to a pull or merge request, branch or tags URL, now or before, with " +
          "their titles and links. Use it to find which thread is likely working on or waiting for something. " +
          "Subscribing suggests, but doesn't prove, that a thread is working on it."),
      "presentation" -> js.Dynamic.literal(
        "label" -> js.Dynamic.literal("pending" -> "Finding subscribed threads", "completed" -> "Found subscribed threads")
      ),
      "parameters" -> js.Dynamic.literal(
        "type" -> "object",
        "properties" -> js.Dynamic.literal(
          "url" -> js.Dynamic.literal("type" -> "string", "description" -> SupportedTargets)
        ),
        "required"             -> js.Array("url"),
        "additionalProperties" -> false
      ),
      "execute" -> ((input: js.Dynamic, _: js.Dynamic) =>
        actions.who(input.url.asInstanceOf[String]).map(fromEither).toJSPromise
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
    |    <url>: a PR or MR, a branch (…/tree/<branch>), a repository's tags (…/tags), or a Maven Central
    |           artifact as maven:group:artifact[:version] or its URL, a local git branch
    |           (git:/absolute/path#branch), or another thread being archived
    |           (bb:thread/thr_…/archived)
    |  bb subscriptions list [--thread <thread-id> | --all]
    |  bb subscriptions who <url>
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
    case "who" :: url :: Nil => actions.who(url).map(_.fold(cliErr, cliOk))
    case "cancel" :: id :: Nil =>
      Future.successful(id.stripPrefix("#").toIntOption match
        case None    => cliErr(s"not a subscription number: $id")
        case Some(n) => actions.cancel(n, None).fold(cliErr, cliOk)
      )
    case ("--help" | "-h" | "help") :: _ => Future.successful(cliOk(Usage))
    case _                               => Future.successful(cliErr(Usage))

/** The queries other plugins use to read and change which threads wait for which. Discoverable, so a caller can
  * tell this plugin being absent from nothing waiting.
  */
private def registerRpc(bb: BbApi, store: Store, actions: Actions): Unit =
  def threadId(description: String) = js.Dynamic.literal("type" -> "string", "description" -> description)
  val edgeProperties = js.Dynamic.literal(
    "subscriberThreadId" -> threadId("The thread that will be told"),
    "targetThreadId"     -> threadId("The thread it waits on")
  )
  val edge = js.Dynamic.literal(
    "type"       -> "object",
    "required"   -> js.Array("subscriberThreadId", "targetThreadId"),
    "properties" -> edgeProperties
  )
  val message = StandardSchema.describedBy(
    js.Dynamic.literal(
      "type"       -> "object",
      "required"   -> js.Array("message"),
      "properties" -> js.Dynamic.literal("message" -> js.Dynamic.literal("type" -> "string"))
    )
  )
  def edgeOf(input: js.Dynamic): Either[String, (String, String)] =
    def str(v: js.Dynamic) = if js.typeOf(v) == "string" then Some(v.asInstanceOf[String]) else None
    for
      subscriber <- str(input.subscriberThreadId).toRight("subscriberThreadId is required")
      target     <- str(input.targetThreadId).toRight("targetThreadId is required")
    yield subscriber -> target
  def answer(result: Future[Either[String, String]]): js.Promise[js.Dynamic] =
    result
      .flatMap(_.fold(e => Future.failed(js.JavaScriptException(js.Error(e))), m => Future.successful(js.Dynamic.literal("message" -> m))))
      .toJSPromise
  bb.rpc.register(
    js.Dynamic.literal(
      "threadWaits" -> js.Dynamic.literal(
        "input"  -> StandardSchema.describedBy(js.Dynamic.literal("type" -> "null")),
        "output" -> StandardSchema.describedBy(
          js.Dynamic.literal(
            "type"       -> "object",
            "required"   -> js.Array("edges"),
            "properties" -> js.Dynamic.literal("edges" -> js.Dynamic.literal("type" -> "array", "items" -> edge))
          )
        ),
        "experimental_description" ->
          "Every active subscription of one thread to another thread being archived or deleted (bb:thread/<id>/archived)."
      ),
      "waitOnThread" -> js.Dynamic.literal(
        "input"  -> StandardSchema.describedBy(edge),
        "output" -> message,
        "experimental_description" ->
          ("Subscribes subscriberThreadId to targetThreadId being archived or deleted, as `bb subscriptions add` does. " +
            "Makes no subscription when the target has already finished, and none twice.")
      ),
      "stopWaitingOnThread" -> js.Dynamic.literal(
        "input"  -> StandardSchema.describedBy(edge),
        "output" -> message,
        "experimental_description" ->
          "Cancels subscriberThreadId's subscription to targetThreadId being archived or deleted, if it has one."
      )
    ),
    js.Dynamic.literal(
      "threadWaits" -> js.Any.fromFunction1 { (_: js.Dynamic) =>
        val edges = store.threadWaits().map { (subscriber, watched) =>
          js.Dynamic.literal("subscriberThreadId" -> subscriber, "targetThreadId" -> watched)
        }
        js.Dynamic.literal("edges" -> js.Array(edges*))
      },
      "waitOnThread" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        answer(edgeOf(input) match
          case Left(e)                   => Future.successful(Left(e))
          case Right((subscriber, target)) => actions.add(subscriber, ThreadArchived(target).url)
        )
      },
      "stopWaitingOnThread" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        answer(Future.successful(edgeOf(input).flatMap { (subscriber, target) =>
          store.find(subscriber, ThreadArchived(target).url) match
            case None    => Right(s"@thread:$subscriber was not waiting on @thread:$target.")
            case Some(s) => actions.cancel(s.id, Some(subscriber))
        }))
      }
    ),
    js.Dynamic.literal(
      "experimental_discoverable" -> true,
      "experimental_description"  ->
        "Subscriptions of threads to events; threadWaits lists which threads wait for which, waitOnThread and stopWaitingOnThread change that."
    )
  )

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit =
  val db = bb.storage.database()
  bb.storage.migrate(db, Store.migrations)
  val store   = Store(db)
  val sources = Sources(bb)
  val actions = Actions(bb, store, sources)
  val poller  = Poller(bb, store, sources)

  registerTools(bb, store, actions)

  bb.cli.register(
    js.Dynamic.literal(
      "name"    -> "subscriptions",
      "summary" -> "Subscribe a thread to a PR, MR, branch, tags, Maven Central artifact or another thread; it is messaged when something changes",
      "commands" -> js.Array(
        js.Dynamic.literal(
          "name"    -> "add",
          "summary" -> "Subscribe this thread (or --thread) to a GitHub or GitLab PR, MR, branch or tags URL, or a Maven artifact",
          "usage"   -> "bb subscriptions add <url> [--thread <thread-id>]"
        ),
        js.Dynamic.literal(
          "name"    -> "list",
          "summary" -> "List this thread's active subscriptions, another thread's, or all of them",
          "usage"   -> "bb subscriptions list [--thread <thread-id> | --all]"
        ),
        js.Dynamic.literal(
          "name"    -> "who",
          "summary" -> "List the threads that subscribed to a URL, now or before, with titles and links",
          "usage"   -> "bb subscriptions who <url>"
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

  registerRpc(bb, store, actions)

  bb.events.on("thread.deleted", (payload: js.Dynamic) => store.endAllFor(payload.thread.id.asInstanceOf[String]))
  bb.events.on("thread.archived", (payload: js.Dynamic) => store.endAllFor(payload.thread.id.asInstanceOf[String]))

  bb.background.schedule("poll", "* * * * *", () => poller.pollAll().toJSPromise)
