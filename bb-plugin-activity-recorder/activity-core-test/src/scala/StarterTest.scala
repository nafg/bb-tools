package activitycore

class StarterTest extends munit.FunSuite:
  private val ordinary = TurnRequest("user", None, "", Set.empty, "please fix it", startsThread = false, None)

  test("the user's own message"):
    assertEquals(Starter.of(ordinary), Some(Starter.User))

  test("a thread the user spawned"):
    assertEquals(Starter.of(ordinary.copy(startsThread = true)), Some(Starter.User))

  test("a thread another thread spawned is started by its parent"):
    assertEquals(Starter.of(ordinary.copy(startsThread = true, parentThreadId = Some("p"))), Some(Starter.Threads(Set("p"))))

  test("a message from another thread"):
    val r = ordinary.copy(initiator = "agent", senderThreadId = Some("t"), text = "[bb message from thread:t]\n\nhi")
    assertEquals(Starter.of(r), Some(Starter.Threads(Set("t"))))

  test("a child's outcome is started by the child, found in the subject or the text"):
    val subject = ordinary.copy(initiator = "system", systemMessageKind = "child-completed", subjectThreadIds = Set("c"), text = "[bb system]")
    assertEquals(Starter.of(subject), Some(Starter.Threads(Set("c"))))
    val batch = ordinary.copy(initiator = "system", systemMessageKind = "child-outcome-batch", text = "[bb system]\n\n- @thread:thr_a1 completed.\n- @thread:thr_b2 completed.")
    assertEquals(Starter.of(batch), Some(Starter.Threads(Set("thr_a1", "thr_b2"))))

  test("automations, subscriptions and other bb notices are not the user's work"):
    assert(Starter.of(ordinary.copy(text = "[bb automation due:auto_x]")).isEmpty)
    assert(Starter.of(ordinary.copy(text = "[bb subscription #3] CI passed")).isEmpty)
    assert(Starter.of(ordinary.copy(initiator = "system", systemMessageKind = "ownership-assigned", text = "[bb system]")).isEmpty)
    assert(Starter.of(ordinary.copy(initiator = "system", text = "Please continue.")).isEmpty)

  test("a starter survives being kept as text"):
    val all = Seq(None, Some(Starter.User), Some(Starter.Threads(Set("b", "a"))))
    all.foreach(s => assertEquals(Starter.decode(Starter.encode(s)), s))
