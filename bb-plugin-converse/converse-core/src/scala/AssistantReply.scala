package conversecore

final case class AssistantMessage(id: String, text: String, sourceSeqEnd: Long)

/** What a thread's conversation shows when it goes idle, for a user waiting
  * to hear a reply.
  */
enum IdleOutcome:
  /** A reply the user has not heard. `latestUnanswered`: a message of the
    * user's came after it and has no reply yet, as when speech arrived while
    * the turn was finishing and the agent answers it in a turn of its own.
    */
  case Reply(message: AssistantMessage, latestUnanswered: Boolean)

  /** Nothing new to read aloud, but the user's latest message has no reply. */
  case Unanswered

  /** Nothing new, and nothing of the user's left unanswered. */
  case NoReply

object AssistantReply:
  def freshSince(
      previousAssistantMessageId: Option[String],
      messages: Iterable[AssistantMessage]
  ): Option[AssistantMessage] =
    messages.maxByOption(_.sourceSeqEnd).filterNot(message => previousAssistantMessageId.contains(message.id))

  /** `latestUserMessageSeq` is where the user's newest message sits in the
    * thread, if it has one.
    */
  def afterIdle(
      previousAssistantMessageId: Option[String],
      messages: Iterable[AssistantMessage],
      latestUserMessageSeq: Option[Long]
  ): IdleOutcome =
    val latestReplySeq = messages.map(_.sourceSeqEnd).maxOption
    val unanswered     = latestUserMessageSeq.exists(user => latestReplySeq.forall(_ < user))
    freshSince(previousAssistantMessageId, messages) match
      case Some(reply)        => IdleOutcome.Reply(reply, unanswered)
      case None if unanswered => IdleOutcome.Unanswered
      case None               => IdleOutcome.NoReply
