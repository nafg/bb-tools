package goalscore

class LendingTest extends munit.FunSuite:
  private def eff(goal: Goal, value: Option[Int]) = Effective(goal, value, LabelSource.Own)

  test("a blocked thread lends its goal and value to its blocker"):
    val lent = Lending.lent(
      blockedBy = Map("a" -> List("b")),
      effective = Map("a" -> eff(Goal.B, Some(1))).get,
      open = Set("a", "b")
    )
    assertEquals(lent, Map("b" -> List(Lent("a", Goal.B, Some(1)))))

  test("lending is transitive along the chain of blockers"):
    val lent = Lending.lent(
      blockedBy = Map("a" -> List("b"), "b" -> List("c")),
      effective = Map("a" -> eff(Goal.B, Some(1)), "b" -> eff(Goal.S, Some(3))).get,
      open = Set("a", "b", "c")
    )
    assertEquals(lent("b"), List(Lent("a", Goal.B, Some(1))))
    assertEquals(lent("c").toSet, Set(Lent("a", Goal.B, Some(1)), Lent("b", Goal.S, Some(3))))

  test("cycles terminate and a thread never lends to itself"):
    val lent = Lending.lent(
      blockedBy = Map("a" -> List("b"), "b" -> List("a")),
      effective = Map("a" -> eff(Goal.B, Some(1)), "b" -> eff(Goal.P, Some(2))).get,
      open = Set("a", "b")
    )
    assertEquals(lent, Map("b" -> List(Lent("a", Goal.B, Some(1))), "a" -> List(Lent("b", Goal.P, Some(2)))))

  test("only open, labeled threads lend"):
    val lent = Lending.lent(
      blockedBy = Map("closed" -> List("b"), "unlabeled" -> List("b")),
      effective = Map("closed" -> eff(Goal.B, Some(1))).get,
      open = Set("unlabeled", "b")
    )
    assertEquals(lent, Map.empty[String, List[Lent]])

  test("importance is the best rank among own and lent, with an unvalued goal ranking as 2"):
    assertEquals(Lending.rank(None), 2)
    assertEquals(Lending.importance(None, Nil), None)
    assertEquals(Lending.importance(Some(eff(Goal.F, None)), Nil), Some(2))
    assertEquals(Lending.importance(Some(eff(Goal.F, Some(3))), List(Lent("x", Goal.B, Some(1)))), Some(1))
    assertEquals(Lending.importance(None, List(Lent("x", Goal.B, Some(3)))), Some(3))

  test("a self-block in stored data lends nothing to itself"):
    val lent = Lending.lent(
      blockedBy = Map("a" -> List("a", "b")),
      effective = Map("a" -> eff(Goal.B, Some(1))).get,
      open = Set("a", "b")
    )
    assertEquals(lent, Map("b" -> List(Lent("a", Goal.B, Some(1)))))

  test("lending passes through a closed thread in the chain, which itself lends nothing"):
    val lent = Lending.lent(
      blockedBy = Map("a" -> List("closed"), "closed" -> List("b")),
      effective = Map("a" -> eff(Goal.B, Some(1)), "closed" -> eff(Goal.F, Some(3))).get,
      open = Set("a", "b")
    )
    assertEquals(lent("b"), List(Lent("a", Goal.B, Some(1))))
    assertEquals(lent("closed"), List(Lent("a", Goal.B, Some(1))))
