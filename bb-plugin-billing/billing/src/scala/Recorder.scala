package billing

import activitycore.ActivityJson
import bbplugin.BbApi
import billingcore.Interval

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

/** How the activity recorder plugin answered, or why it could not. */
final case class RecorderStatus(available: Boolean, error: Option[String], status: Option[js.Dynamic])

/** The activity recorder plugin (`activity-recorder`), reached through bb's cross-plugin RPC. Any failure to reach it
  * is reported as unavailable with the message bb gave, since bb has no way to say whether a plugin is installed.
  */
final class Recorder(bb: BbApi)(using ExecutionContext):
  val PluginId = "activity-recorder"

  private val passthroughSchema: js.Any =
    js.Dynamic.literal("parse" -> js.Any.fromFunction1((v: js.Any) => v))

  private def call(method: String, input: js.Any): Future[js.Dynamic] =
    bb.sdk.plugins
      .callRpc(
        js.Dynamic.literal(
          "pluginId"     -> PluginId,
          "method"       -> method,
          "input"        -> input,
          "outputSchema" -> passthroughSchema
        )
      )
      .toFuture

  def status(): Future[RecorderStatus] =
    call("status", null)
      .map(s => RecorderStatus(available = true, error = None, status = Some(s)))
      .recover { case e => RecorderStatus(available = false, error = Some(errorMessage(e)), status = None) }

  /** Every recorded signal reaching into `[from, to)`, of every kind. */
  def signals(from: Long, to: Long): Future[Either[String, Vector[activitycore.Signal]]] =
    call("signals", js.Dynamic.literal("from" -> from.toDouble, "to" -> to.toDouble))
      .map(result =>
        ActivityJson.signals(result).left.map(problem => s"the activity recorder's signals output could not be read: $problem")
      )
      .recover { case e => Left(errorMessage(e)) }

  /** Attention intervals in `[from, to)`, with the recorder's default kinds (attention only, never `turn`). An output
    * the shared parser rejects is an error, never a shorter list: a shape change must show up, not lower totals.
    */
  def intervals(from: Long, to: Long): Future[Either[String, Vector[Interval]]] =
    call("intervals", js.Dynamic.literal("from" -> from.toDouble, "to" -> to.toDouble))
      .map { result =>
        ActivityJson
          .intervals(result)
          .left
          .map(problem => s"the activity recorder's intervals output could not be read: $problem")
          .map(_.collect { case i if i.end > i.start => Interval(i.threadId, i.start, i.end) })
      }
      .recover { case e => Left(errorMessage(e)) }
