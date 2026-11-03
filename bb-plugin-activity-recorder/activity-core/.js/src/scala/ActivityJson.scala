package activitycore

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** The JSON shapes of [[ActivityRecorderRpc]] values, for the recorder and
  * for the plugins reading them.
  */
object ActivityJson:
  def signal(value: Signal): js.Dynamic =
    js.Dynamic.literal(
      "id"       -> value.id,
      "threadId" -> value.threadId,
      "kind"     -> value.kind.name,
      "start"    -> value.start.toDouble,
      "end"      -> value.end.toDouble,
      "source"   -> value.source
    )

  def interval(value: Interval): js.Dynamic =
    js.Dynamic.literal(
      "threadId" -> value.threadId,
      "start"    -> value.start.toDouble,
      "end"      -> value.end.toDouble,
      "kinds"    -> value.kinds.toVector.map(_.name).sorted.toJSArray
    )

  def agentSpan(value: AgentSpan): js.Dynamic =
    js.Dynamic.literal(
      "threadId"  -> value.threadId,
      "start"     -> value.start.toDouble,
      "end"       -> value.end.toDouble,
      "startedBy" -> (value.startedBy match
        case Starter.User         => js.Dynamic.literal("kind" -> "user")
        case Starter.Threads(ids) => js.Dynamic.literal("kind" -> "threads", "ids" -> ids.toVector.sorted.toJSArray))
    )

  /** The `agentSpans` output, or a message describing what is wrong with it. */
  def agentSpans(output: js.Dynamic): Either[String, Vector[AgentSpan]] =
    array(output.agentSpans).flatMap(_.traverse(readAgentSpan))

  private def readAgentSpan(value: js.Dynamic): Either[String, AgentSpan] =
    for
      threadId  <- string(value.threadId, "threadId")
      start     <- long(value.start, "start")
      end       <- long(value.end, "end")
      startedBy <- readStarter(value.startedBy)
    yield AgentSpan(threadId, start, end, startedBy)

  private def readStarter(value: js.Dynamic): Either[String, Starter] =
    string(value.kind, "startedBy.kind").flatMap {
      case "user"    => Right(Starter.User)
      case "threads" => array(value.ids).flatMap(_.traverse(id => string(id, "startedBy.ids"))).map(ids => Starter.Threads(ids.toSet))
      case other     => Left(s"unknown starter kind: $other")
    }

  /** The `intervals` output, or a message describing what is wrong with it. */
  def intervals(output: js.Dynamic): Either[String, Vector[Interval]] =
    array(output.intervals).flatMap(_.traverse(readInterval))

  /** The `signals` output, or a message describing what is wrong with it. */
  def signals(output: js.Dynamic): Either[String, Vector[Signal]] =
    array(output.signals).flatMap(_.traverse(readSignal))

  private def readInterval(value: js.Dynamic): Either[String, Interval] =
    for
      threadId <- string(value.threadId, "threadId")
      start    <- long(value.start, "start")
      end      <- long(value.end, "end")
      kinds    <- array(value.kinds).flatMap(_.traverse(kind => string(kind, "kinds").flatMap(parseKind)))
    yield Interval(threadId, start, end, kinds.toSet)

  private def readSignal(value: js.Dynamic): Either[String, Signal] =
    for
      id       <- string(value.id, "id")
      threadId <- string(value.threadId, "threadId")
      kind     <- string(value.kind, "kind").flatMap(parseKind)
      start    <- long(value.start, "start")
      end      <- long(value.end, "end")
      source   <- string(value.source, "source")
    yield Signal(id, threadId, kind, start, end, source)

  private def parseKind(name: String): Either[String, SignalKind] =
    SignalKind.parse(name).toRight(s"unknown signal kind: $name")

  private def string(value: js.Dynamic, field: String): Either[String, String] =
    if js.typeOf(value) == "string" then Right(value.asInstanceOf[String]) else Left(s"$field is not a string")

  private def long(value: js.Dynamic, field: String): Either[String, Long] =
    if js.typeOf(value) == "number" then Right(value.asInstanceOf[Double].toLong) else Left(s"$field is not a number")

  private def array(value: js.Dynamic): Either[String, Vector[js.Dynamic]] =
    if js.Array.isArray(value) then Right(value.asInstanceOf[js.Array[js.Dynamic]].toVector)
    else Left("expected an array")

  extension [A](values: Vector[A])
    private def traverse[B](f: A => Either[String, B]): Either[String, Vector[B]] =
      values.foldLeft[Either[String, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
        for (bs <- acc; b <- f(a)) yield bs :+ b
      }
