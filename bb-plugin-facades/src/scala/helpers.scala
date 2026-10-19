package bbplugin

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

// Plain Scala helpers every plugin backend in this repository needs, next to
// the facades so a plugin gets both with one dependency.

/** Reading loosely typed values the host hands over as `js.Dynamic`. */
object Dyn:
  /** The value as text; empty for undefined or null. */
  def str(value: js.Dynamic): String =
    value.asInstanceOf[js.UndefOr[Any]].fold("")(v => if v == null then "" else v.toString)

  /** The value when it is a number other than NaN. */
  def number(value: js.Dynamic): Option[Double] =
    if js.typeOf(value) == "number" && !value.asInstanceOf[Double].isNaN then Some(value.asInstanceOf[Double])
    else None

  def isDefined(value: js.Dynamic): Boolean = !js.isUndefined(value) && value != null

/** Standard Schema v1 values for `bb.rpc.register`, where bb's RPC layer
  * needs only `~standard.validate`, and discoverable RPC also needs
  * `~standard.jsonSchema`. Neither validates: the handler checks its input.
  */
object StandardSchema:
  private def make(jsonSchema: Option[js.Dynamic]): js.Any =
    val standard = js.Dynamic.literal(
      "version"  -> 1,
      "vendor"   -> "bb-tools",
      "validate" -> js.Any.fromFunction1((value: js.Any) => js.Dynamic.literal("value" -> value))
    )
    jsonSchema.foreach { schema =>
      val exported = js.Any.fromFunction1((_: js.Any) => schema)
      standard.updateDynamic("jsonSchema")(js.Dynamic.literal("input" -> exported, "output" -> exported))
    }
    js.Dynamic.literal("~standard" -> standard)

  /** Accepts anything and publishes no shape. */
  val passthrough: js.Any = make(None)

  /** Accepts anything and publishes `jsonSchema` (JSON Schema draft 2020-12) as its shape. */
  def describedBy(jsonSchema: js.Dynamic): js.Any = make(Some(jsonSchema))

/** Runs `work` for a key at most once at a time. A request while a run is
  * under way queues exactly one more run after it, however many requests
  * arrive; a failed run is recovered by `onFailure` so the next can start.
  */
final class OneInFlight[K](work: K => Future[Unit], onFailure: (K, Throwable) => Unit)(using ExecutionContext):
  private val running = mutable.Set.empty[K]
  private val queued  = mutable.Set.empty[K]

  def request(key: K): Future[Unit] =
    if running.contains(key) then
      queued += key
      Future.unit
    else
      running += key
      Future
        .delegate(work(key))
        .recover { case e => onFailure(key, e) }
        .flatMap { _ =>
          running -= key
          if queued.remove(key) then request(key) else Future.unit
        }
