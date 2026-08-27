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
