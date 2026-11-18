package billing

import bbplugin.*
import billingcore.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit =
  val db = bb.storage.database()
  bb.storage.migrate(db, Store.migrations)

  val settings = bb.settings.define(
    js.Dynamic.literal(
      "clientId"     -> js.Dynamic.literal("type" -> "string", "label" -> "Zoho OAuth client id", "default" -> ""),
      "clientSecret" -> js.Dynamic.literal("type" -> "string", "label" -> "Zoho OAuth client secret", "secret" -> true),
      "organizationId"          -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "Zoho Books organization id",
        "default" -> ""
      ),
      "refreshToken"            -> js.Dynamic.literal(
        "type"        -> "string",
        "label"       -> "Zoho OAuth refresh token",
        "description" -> "A refresh token for the client above with the ZohoBooks.fullaccess.all scope",
        "secret"      -> true
      ),
      "summaryModel"            -> js.Dynamic.literal(
        "type"  -> "string",
        "label" -> "Summary model",
        "description" -> "The model that describes each stretch of work, as provider/model from `bb billing models` (for example claude-code/haiku); empty uses bb's default model",
        "default" -> ""
      ),
      "summaryReasoningLevel"   -> js.Dynamic.literal(
        "type"        -> "string",
        "label"       -> "Summary reasoning level",
        "description" -> "The reasoning level for the summary model (low is plenty); empty uses the model's default",
        "default"     -> ""
      ),
      "billingIncrementMinutes" -> js.Dynamic.literal(
        "type"  -> "number",
        "label" -> "Billing increment (minutes)",
        "description" -> "Each dated entry sent to Zoho is rounded up to a multiple of this; the review shows exact minutes",
        "default" -> 1
      )
    )
  )

  val store           = Store(db)
  val recorder        = Recorder(bb)
  val changes         = ChangeFeed(bb)
  val mappings        = Mappings(bb, changes)
  val zoho            = Zoho(bb, settings)
  val projectMappings = ProjectMappings(bb, changes)
  val summaries       = Summaries(bb, settings, store)
  val service         =
    Service(bb, settings, store, recorder, mappings, projectMappings, zoho, summaries, Conversation(bb, store), changes)

  val commands = Commands(bb, service, recorder, mappings, projectMappings, zoho, summaries)

  Rpc.register(bb, service, commands)
  Cli.register(bb, service, commands, projectMappings, zoho, summaries)
  AgentTools.register(bb, service, commands)

  // A page open across a reload refreshes, so it stops waiting on summary runs the old plugin took with it.
  changes(Change.Restarted)
  val _ = service.warm()

  // An archived thread must get a billing decision. An event without the thread's id could be any thread.
  def archived(e: js.Dynamic): Change =
    Option(bbplugin.Dyn.str(e.threadId)).filter(_.nonEmpty).fold(Change.Restarted)(Change.Archived.apply)
  bb.events.on("thread.archived", (e: js.Dynamic) => changes(archived(e)))
  bb.events.on(
    "thread.unarchived",
    (e: js.Dynamic) =>
      service.forgetArchived()
      changes(archived(e))
  )

  zoho.missingSettings().foreach { missing =>
    if missing.nonEmpty then
      bb.status.needsConfiguration(
        s"Set the Zoho settings (${missing.mkString(", ")}) to send time entries; choosing where threads bill works without them."
      )
  }

private object Rpc:
  private val method: js.Any =
    js.Dynamic.literal("input" -> StandardSchema.passthrough, "output" -> StandardSchema.passthrough)

  private def orFail[A](either: Either[String, A]): Future[A] =
    either.fold(m => Future.failed(new RuntimeException(m)), Future.successful)

  /** Registers every `BillingRpc` method: the input is decoded by the shared contract and run through the one door; the
    * reply is encoded once per reply kind.
    */
  def register(bb: BbApi, service: Service, commands: Commands): Unit =
    val contract = js.Dynamic.literal()
    val handlers = js.Dynamic.literal()
    BillingRpc.methods.foreach { name =>
      contract.updateDynamic(name)(method)
      handlers.updateDynamic(name)(js.Any.fromFunction1 { (raw: js.Dynamic) =>
        orFail(BillingRpc.decode(name, WireJs.fromJs(raw))).flatMap {
          // The run outlives the request: a model run can take minutes, longer than a request may stay open.
          case BillingRpc.Summarize(threadId, subthreads, pick) =>
            service.startSummaries(threadId, subthreads, pick)
            Future.successful(Json.ok)
          case rpc                                              => run(service, commands, rpc)
        }.toJSPromise
      })
    }
    bb.rpc.register(contract, handlers)

  private def run[R](service: Service, commands: Commands, rpc: BillingRpc[R]): Future[js.Any] =
    commands.run(rpc).map(out => Json.reply(service, rpc, out))
