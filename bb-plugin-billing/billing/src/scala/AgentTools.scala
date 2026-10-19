package billing

import bbplugin.BbApi
import bbplugin.Dyn.str
import billingcore.DraftPick

import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** Native tools: only sending needs one, because it waits for the user's confirmation and a shell command would time
  * out waiting. Everything else is `bb billing …`.
  */
object AgentTools:
  def register(bb: BbApi, service: Service, commands: Commands)(using ExecutionContext): Unit =
    bb.agents.registerTool(
      js.Dynamic.literal(
        "name"         -> "billing_send_thread",
        "description"  ->
          ("Send a thread's unbilled recorded time (and its subthreads' that bill to the same place) to Zoho Books: " +
            "every unbilled stretch, or the stretch keys given; one dated time entry per calendar day. Shows the user the stretches, the dated entries with " +
            "their notes, and any time already in Zoho by hand, in this thread, and sends only if they confirm. " +
            "Returns what happened. Run `bb billing plan <thread>` first and settle shared stretches; sending is refused while any is unsettled."),
        "instructions" ->
          ("Use billing_send_thread to send time to Zoho Books; never try to send another way. The user confirms in the " +
            "thread before anything is written. Prepare with `bb billing pending`, `bb billing plan <thread>`, and `bb billing settle …`."),
        "presentation" -> js.Dynamic.literal(
          "label" -> js.Dynamic.literal(
            "pending"   -> "Asking you to confirm sending time to Zoho Books",
            "completed" -> "Finished sending time to Zoho Books"
          )
        ),
        "parameters"   -> js.Dynamic.literal(
          "type"                 -> "object",
          "properties"           -> js.Dynamic.literal(
            "threadId"   -> js.Dynamic.literal(
              "type" -> "string",
              "description" -> "The thread to bill (its subthreads that bill to the same place are included unless subthreads is false)"
            ),
            "keys"       -> js.Dynamic.literal(
              "type"        -> "array",
              "items"       -> js.Dynamic.literal("type" -> "string"),
              "description" -> "Stretch keys from `bb billing plan` to send; omitted means every unbilled stretch"
            ),
            "subthreads" -> js.Dynamic.literal("type" -> "boolean", "description" -> "Include subthreads; default true")
          ),
          "required"             -> js.Array("threadId"),
          "additionalProperties" -> false
        ),
        "execute"      -> js.Any.fromFunction2 { (input: js.Dynamic, toolCtx: js.Dynamic) =>
          val root       = str(input.threadId)
          val pick       = input.keys.asInstanceOf[js.UndefOr[js.Array[String]]].toOption
            .fold[DraftPick](DraftPick.AllUnbilled)(ks => DraftPick.Keys(ks.toSet))
          val subthreads = input.subthreads.asInstanceOf[js.UndefOr[Boolean]].getOrElse(true)
          SendConfirmation.run(
            bb,
            service,
            commands,
            str(toolCtx.threadId),
            root,
            pick,
            subthreads,
            toolCtx.signal
          ).map {
            case Right(text) => text: js.Any
            case Left(error) => js.Dynamic.literal(
                "content" -> js.Array(js.Dynamic.literal("type" -> "text", "text" -> error)),
                "isError" -> true
              ): js.Any
          }.toJSPromise
        }
      )
    )
