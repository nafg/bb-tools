package conversecore

class AssistantReplyTest extends munit.FunSuite:
  test("does not treat the previous assistant message as a reply to a completed turn"):
    val previous = AssistantMessage("assistant-1", "previous response", sourceSeqEnd = 42)

    assertEquals(AssistantReply.freshSince(Some(previous.id), Vector(previous)), None)

  test("message identity permits a legitimate reply with repeated text"):
    val previous = AssistantMessage("assistant-1", "same response", sourceSeqEnd = 42)
    val fresh    = AssistantMessage("assistant-2", "same response", sourceSeqEnd = 51)

    assertEquals(
      AssistantReply.freshSince(Some(previous.id), Vector(fresh, previous)),
      Some(fresh)
    )

  test("the first reply is fresh when the thread had no assistant message"):
    val fresh = AssistantMessage("assistant-1", "first response", sourceSeqEnd = 7)

    assertEquals(AssistantReply.freshSince(None, Vector(fresh)), Some(fresh))

  // The 03:00 case: a second question steered in while the reply to the first
  // was being written; the turn ended, and the agent answered it in a new turn.
  test("a reply written before the user's next message is read aloud, and the next message still awaits its own"):
    val earlier = AssistantMessage("assistant-1", "earlier answer", sourceSeqEnd = 2890)
    val reply   = AssistantMessage("assistant-2", "No, I don't think it's worth it.", sourceSeqEnd = 2914)

    assertEquals(
      AssistantReply.afterIdle(Some(earlier.id), Vector(earlier, reply), latestUserMessageSeq = Some(2915)),
      IdleOutcome.Reply(reply, latestUnanswered = true)
    )

  test("a reply after the user's latest message answers it"):
    val reply = AssistantMessage("assistant-2", "It's a closer call than I'd like.", sourceSeqEnd = 2938)

    assertEquals(
      AssistantReply.afterIdle(Some("assistant-1"), Vector(reply), latestUserMessageSeq = Some(2915)),
      IdleOutcome.Reply(reply, latestUnanswered = false)
    )

  test("with nothing new and the user's latest message unanswered, the reply is still to come"):
    val heard = AssistantMessage("assistant-2", "already read aloud", sourceSeqEnd = 2914)

    assertEquals(
      AssistantReply.afterIdle(Some(heard.id), Vector(heard), latestUserMessageSeq = Some(2915)),
      IdleOutcome.Unanswered
    )

  test("with nothing new and nothing unanswered, the agent finished without a reply"):
    val heard = AssistantMessage("assistant-2", "already read aloud", sourceSeqEnd = 2914)

    assertEquals(
      AssistantReply.afterIdle(Some(heard.id), Vector(heard), latestUserMessageSeq = Some(2900)),
      IdleOutcome.NoReply
    )

  test("when the steered message was answered within the same turn, nothing is left waiting"):
    val reply = AssistantMessage("assistant-2", "Answering both questions.", sourceSeqEnd = 2920)

    assertEquals(
      AssistantReply.afterIdle(Some("assistant-1"), Vector(reply), latestUserMessageSeq = Some(2915)),
      IdleOutcome.Reply(reply, latestUnanswered = false)
    )
