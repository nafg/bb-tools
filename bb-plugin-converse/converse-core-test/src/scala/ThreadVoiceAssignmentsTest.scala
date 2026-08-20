package conversecore

class ThreadVoiceAssignmentsTest extends munit.FunSuite:
  test("no enabled voices leaves a thread unassigned"):
    assertEquals(ThreadVoiceAssignments.chooseVoice("thread-1", Vector.empty, Map.empty), None)

  test("chooses an enabled voice"):
    val result = ThreadVoiceAssignments.chooseVoice("thread-1", Vector("alba", "anna"), Map.empty)
    assert(result.exists(Set("alba", "anna")))

  test("normalizes duplicate and blank voice names"):
    val result = ThreadVoiceAssignments.chooseVoice("thread-1", Vector(" alba ", "", "alba"), Map.empty)
    assertEquals(result, Some("alba"))

  test("prefers the least-used voice"):
    val result = ThreadVoiceAssignments.chooseVoice(
      "thread-1",
      Vector("alba", "anna", "charles"),
      Map("alba" -> 2, "anna" -> 0, "charles" -> 1)
    )
    assertEquals(result, Some("anna"))

  test("selection is stable when usage counts are tied"):
    val first  = ThreadVoiceAssignments.chooseVoice("thread-1", Vector("alba", "anna"), Map.empty)
    val second = ThreadVoiceAssignments.chooseVoice("thread-1", Vector("alba", "anna"), Map.empty)
    assertEquals(second, first)
