package subscriptionsourcerpc

import bbplugin.*
import subscriptionsource.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

private def str(v: js.Dynamic): Option[String] = if js.typeOf(v) == "string" then Some(v.asInstanceOf[String]) else None

private def recognitionToJs(r: Recognition): js.Dynamic = r match
  case Recognition.Recognized(url, label) => js.Dynamic.literal("kind" -> "recognized", "url" -> url, "label" -> label)
  case Recognition.Invalid(reason)        => js.Dynamic.literal("kind" -> "invalid", "reason" -> reason)
  case Recognition.NotMine                => js.Dynamic.literal("kind" -> "notMine")

private def recognitionFromJs(j: js.Dynamic): Recognition = str(j.kind) match
  case Some("recognized") => Recognition.Recognized(str(j.url).getOrElse(""), str(j.label).getOrElse(""))
  case Some("invalid")    => Recognition.Invalid(str(j.reason).getOrElse("invalid"))
  case _                  => Recognition.NotMine

private def resultToJs(r: CheckResult): js.Dynamic =
  js.Dynamic.literal(
    "state"    -> r.state,
    "label"    -> r.label,
    "summary"  -> r.summary,
    "promise"  -> r.promise,
    "link"     -> r.link,
    "messages" -> r.messages.toJSArray,
    "finished" -> r.finished
  )

private def resultFromJs(j: js.Dynamic): CheckResult =
  CheckResult(
    state = str(j.state).getOrElse(""),
    label = str(j.label).getOrElse(""),
    summary = str(j.summary).getOrElse(""),
    promise = str(j.promise).getOrElse(""),
    link = str(j.link).getOrElse(""),
    messages = j.messages.asInstanceOf[js.UndefOr[js.Array[String]]].toOption.filter(_ != null).map(_.toList).getOrElse(Nil),
    finished = j.finished.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false)
  )

/** A source plugin's side: registers the two methods the subscriptions plugin calls. */
object SourceServer:
  /** `description` says which targets this source watches, in the form a user writes them; the subscriptions plugin
    * shows it when no source recognizes a target.
    */
  def register(
      bb: BbApi,
      description: String,
      recognize: String => Recognition,
      check: CheckRequest => Future[CheckResult]
  )(using ExecutionContext): Unit =
    // Discoverable methods must publish a schema; an empty one accepts anything, and the handlers check their input.
    val any = StandardSchema.describedBy(js.Dynamic.literal())
    bb.rpc.register(
      js.Dynamic.literal(
        SourceMethods.Recognize -> js.Dynamic.literal("input" -> any, "output" -> any),
        SourceMethods.Check     -> js.Dynamic.literal("input" -> any, "output" -> any)
      ),
      js.Dynamic.literal(
        SourceMethods.Recognize -> js.Any.fromFunction1((input: js.Dynamic) =>
          recognitionToJs(recognize(str(input.input).getOrElse("")))
        ),
        SourceMethods.Check -> js.Any.fromFunction1 { (input: js.Dynamic) =>
          val request = CheckRequest(
            url = str(input.url).getOrElse(""),
            previous = str(input.previous),
            subscriberThreadId = str(input.subscriberThreadId).getOrElse(""),
            force = input.force.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false)
          )
          check(request).map(resultToJs).toJSPromise
        }
      ),
      js.Dynamic.literal("experimental_discoverable" -> true, "experimental_description" -> description)
    )

/** A source as the subscriptions plugin sees it: another plugin, called through its two methods. */
final class SourceClient(bb: BbApi, val pluginId: String, val description: String)(using ExecutionContext):
  private def call(method: String, input: js.Any): Future[js.Dynamic] =
    bb.sdk.plugins
      .callRpc(
        js.Dynamic.literal(
          "pluginId"     -> pluginId,
          "method"       -> method,
          "input"        -> input,
          "outputSchema" -> js.Dynamic.literal("parse" -> js.Any.fromFunction1((result: js.Any) => result))
        )
      )
      .toFuture
      .map(_.asInstanceOf[js.Dynamic])

  /** bb reports a failed call as "BbHttpError: HTTP 500: <the source's message>"; the source's message is what
    * a thread needs to read.
    */
  private def unwrapped[A](f: Future[A]): Future[A] =
    f.recoverWith { case e =>
      val message = Option(e.getMessage).getOrElse("").replaceFirst("""^(?:\w+Error: )?HTTP \d+: """, "")
      Future.failed(Exception(message))
    }

  def recognize(input: String): Future[Recognition] =
    unwrapped(call(SourceMethods.Recognize, js.Dynamic.literal("input" -> input)).map(recognitionFromJs))

  def check(request: CheckRequest): Future[CheckResult] =
    unwrapped(call(
      SourceMethods.Check,
      js.Dynamic.literal(
        "url"                -> request.url,
        "previous"           -> request.previous.orNull,
        "subscriberThreadId" -> request.subscriberThreadId,
        "force"              -> request.force
      )
    ).map(resultFromJs))

object SourceClient:
  /** Every installed plugin that registered the source methods. */
  def discover(bb: BbApi)(using ExecutionContext): Future[List[SourceClient]] =
    bb.sdk.plugins
      .experimental_discoverRpc(js.Dynamic.literal("method" -> SourceMethods.Check))
      .toFuture
      .map(_.toList.flatMap { m =>
        str(m.pluginId).map(id => SourceClient(bb, id, str(m.registrationDescription).getOrElse(id)))
      }.distinctBy(_.pluginId))
