package goalscore

class InheritanceTest extends munit.FunSuite:
  private val b3 = ThreadLabels(Some(Goal.B), Some(3), Nil)
  private val s1 = ThreadLabels(Some(Goal.S), Some(1), Nil)

  test("an own goal wins over any ancestor"):
    val own       = Map("child" -> s1, "root" -> b3)
    val ancestors = Map("child" -> List("root"))
    assertEquals(Inheritance.effective("child", own, ancestors), Some(Effective(Goal.S, Some(1), LabelSource.Own)))

  test("without an own goal, the nearest labeled ancestor supplies goal and value together"):
    val own       = Map("root" -> b3, "mid" -> ThreadLabels(None, Some(1), Nil), "leaf" -> ThreadLabels.Empty)
    val ancestors = Map("leaf" -> List("mid", "root"), "mid" -> List("root"))
    // mid has a value but no goal, so it is skipped entirely: the value is not mixed in.
    assertEquals(Inheritance.effective("leaf", own, ancestors), Some(Effective(Goal.B, Some(3), LabelSource.Inherited("root"))))
    assertEquals(Inheritance.effective("mid", own, ancestors), Some(Effective(Goal.B, Some(3), LabelSource.Inherited("root"))))

  test("the nearest labeled ancestor is the closest one, not the root"):
    val own       = Map("root" -> b3, "mid" -> s1)
    val ancestors = Map("leaf" -> List("mid", "root"))
    assertEquals(Inheritance.effective("leaf", own, ancestors), Some(Effective(Goal.S, Some(1), LabelSource.Inherited("mid"))))

  test("no labeled ancestor means unlabeled"):
    assertEquals(Inheritance.effective("leaf", Map.empty, Map("leaf" -> List("root"))), None)
    assertEquals(Inheritance.effective("orphan", Map.empty, Map.empty), None)
