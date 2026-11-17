package goalscore

class LabelsTest extends munit.FunSuite:
  test("goal letters parse case-insensitively and reject anything else"):
    assertEquals(Goal.parse("B"), Some(Goal.B))
    assertEquals(Goal.parse("f"), Some(Goal.F))
    assertEquals(Goal.parse("X"), None)
    assertEquals(Goal.parse(""), None)

  test("a goal write accepts a letter or an explicit none"):
    assertEquals(ThreadLabels.parseGoalWrite("S"), Right(Some(Goal.S)))
    assertEquals(ThreadLabels.parseGoalWrite("none"), Right(None))
    assert(ThreadLabels.parseGoalWrite("Q").isLeft)

  test("a value write accepts 1 to 3 or none"):
    assertEquals(ThreadLabels.parseValueWrite("2"), Right(Some(2)))
    assertEquals(ThreadLabels.parseValueWrite("none"), Right(None))
    assert(ThreadLabels.parseValueWrite("0").isLeft)
    assert(ThreadLabels.parseValueWrite("4").isLeft)
    assert(ThreadLabels.parseValueWrite("two").isLeft)

  test("thread ids must look like bb thread ids"):
    assertEquals(ThreadLabels.parseThreadId("thr_abc123"), Right("thr_abc123"))
    assert(ThreadLabels.parseThreadId("abc").isLeft)
    assert(ThreadLabels.parseThreadId("thr_ABC").isLeft)

  test("stored metadata is read tolerantly: bad fields are dropped, not fatal"):
    assertEquals(ThreadLabels.fromStored(goal = Some("P"), value = Some(7.0)), ThreadLabels(Some(Goal.P), None))
    assertEquals(ThreadLabels.fromStored(Some("?"), Some(2.0)), ThreadLabels(None, Some(2)))

  test("a legacy stored blockedBy keeps only distinct thread ids"):
    assertEquals(ThreadLabels.legacyBlockedBy(Seq("thr_a1", 3, "thr_a1", "bad")), List("thr_a1"))

  test("value without goal survives storage but reads as unlabeled"):
    assertEquals(ThreadLabels(None, Some(1)).ownLabel, None)
    assertEquals(ThreadLabels(Some(Goal.M), None).ownLabel, Some(Effective(Goal.M, None, LabelSource.Own)))

  test("an edit leaves absent fields alone, clears on none, and rejects self-blocking"):
    assertEquals(LabelEdit.parse("thr_me", None, None, None), Right(LabelEdit(None, None, None)))
    assertEquals(
      LabelEdit.parse("thr_me", Some("none"), Some("1"), Some(List("thr_a", " thr_a ", ""))),
      Right(LabelEdit(Some(None), Some(Some(1)), Some(List("thr_a"))))
    )
    assertEquals(LabelEdit.parse("thr_me", None, None, Some(List("thr_me"))), Left("a thread cannot block itself"))
    assertEquals(LabelEdit.parse("thr_me", None, None, Some(List("nope"))), Left("blockedBy 'nope': not a thread id"))
    assert(LabelEdit.parse("thr_me", Some("Z"), None, None).isLeft)

  test("my move parses, clears on none, and is read tolerantly from storage"):
    assertEquals(ThreadLabels.parseMyMoveWrite("Focus"), Right(Some(MyMove.Focus)))
    assertEquals(ThreadLabels.parseMyMoveWrite("none"), Right(None))
    assert(ThreadLabels.parseMyMoveWrite("soon").isLeft)
    val stored = ThreadLabels.fromStored(None, None, myMove = Some("quick"), myMoveAt = Some(1000.0))
    assertEquals(stored.myMove, Some(MyMove.Quick))
    assertEquals(stored.myMoveAt, Some(1000L))
    assertEquals(ThreadLabels.fromStored(None, None, myMove = Some(3), myMoveAt = Some("x")).myMove, None)

  test("a user message clears a move set before it, not one set after"):
    assert(ThreadLabels.clearedBy(Some(1000L), messageAt = 2000L))
    assert(!ThreadLabels.clearedBy(Some(3000L), messageAt = 2000L))
    assert(ThreadLabels.clearedBy(None, messageAt = 2000L))
