package conversecore

final case class UtteranceRoute(threadId: String)

object UtteranceRoute:
  def latch(viewedThreadId: Option[String], sessionThreadId: Option[String]): Option[UtteranceRoute] =
    viewedThreadId.orElse(sessionThreadId).filter(_.nonEmpty).map(UtteranceRoute.apply)
