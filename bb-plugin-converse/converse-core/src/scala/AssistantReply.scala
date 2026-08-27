package conversecore

final case class AssistantMessage(id: String, text: String, sourceSeqEnd: Long)

object AssistantReply:
  def freshSince(
      previousAssistantMessageId: Option[String],
      messages: Iterable[AssistantMessage]
  ): Option[AssistantMessage] =
    messages.maxByOption(_.sourceSeqEnd).filterNot(message => previousAssistantMessageId.contains(message.id))
