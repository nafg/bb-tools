package goalscore

class OverviewTest extends munit.FunSuite:
  private def t(id: String, parent: Option[String] = None) = ThreadInfo(id, parent, projectId = "p", title = id)

  test("roots are grouped by effective goal; children nest under parents whatever their own goal"):
    val threads = List(t("r1"), t("c1", Some("r1")), t("r2"), t("c2", Some("r2")), t("lone"))
    val own     = Map(
      "r1" -> ThreadLabels(Some(Goal.B), Some(1)),
      "c1" -> ThreadLabels(Some(Goal.S), Some(2)),
      "r2" -> ThreadLabels(Some(Goal.P), None)
    )
    val ancestors = Map("c1" -> List("r1"), "c2" -> List("r2"))
    val seconds   = Map("r1" -> 100.0, "c1" -> 50.0, "c2" -> 10.0)
    val overview  = Overview.build(threads, own, ancestors, Map.empty, seconds)

    val groups = overview.groups.map(g => g.goal -> g.threads.map(_.id)).toMap
    assertEquals(groups(Some(Goal.B)), List("r1"))
    assertEquals(groups(Some(Goal.P)), List("r2"))
    assertEquals(groups(None), List("lone"))
    val r1 = overview.groups.flatMap(_.threads).find(_.id == "r1").get
    assertEquals(r1.children.map(_.id), List("c1"))
    assertEquals(r1.children.head.label.map(_.goal), Some(Goal.S))
    val c2 = overview.groups.flatMap(_.threads).flatMap(_.children).find(_.id == "c2").get
    assertEquals(c2.label, Some(Effective(Goal.P, None, LabelSource.Inherited("r2"))))

  test("per-goal attention sums every thread with that effective goal, in full, wherever it sits"):
    val threads = List(t("r1"), t("c1", Some("r1")), t("r2"), t("c2", Some("r2")))
    val own     = Map(
      "r1" -> ThreadLabels(Some(Goal.B), Some(1)),
      "c1" -> ThreadLabels(Some(Goal.S), Some(2)),
      "r2" -> ThreadLabels(Some(Goal.P), None)
    )
    val ancestors = Map("c1" -> List("r1"), "c2" -> List("r2"))
    val seconds   = Map("r1" -> 100.0, "c1" -> 50.0, "c2" -> 10.0)
    val overview  = Overview.build(threads, own, ancestors, Map.empty, seconds)
    val byGoal    = overview.groups.map(g => g.goal -> g.seconds).toMap
    assertEqualsDouble(byGoal(Some(Goal.B)), 100.0, 1e-9)
    assertEqualsDouble(byGoal(Some(Goal.S)), 50.0, 1e-9)
    assertEqualsDouble(byGoal(Some(Goal.P)), 10.0, 1e-9)
    assertEquals(byGoal.get(None), None)
    assertEquals(overview.groups.map(_.goal), List(Some(Goal.B), Some(Goal.P), Some(Goal.S)))

  test("a child whose parent is not in the open set becomes a root"):
    val threads  = List(t("c", Some("archived-parent")))
    val overview = Overview.build(threads, Map.empty, Map("c" -> List("archived-parent")), Map.empty, Map.empty)
    assertEquals(overview.groups.flatMap(_.threads).map(_.id), List("c"))

  test("blocking lends across the whole overview and importance follows"):
    val threads = List(t("blocked"), t("blocker"))
    val own = Map("blocked" -> ThreadLabels(Some(Goal.B), Some(1)), "blocker" -> ThreadLabels(Some(Goal.F), Some(3)))
    val all = Overview.build(threads, own, Map.empty, Map("blocked" -> List("blocker")), Map.empty).groups.flatMap(_.threads)
    val blocker = all.find(_.id == "blocker").get
    assertEquals(blocker.lent, List(Lent("blocked", Goal.B, Some(1))))
    assertEquals(blocker.importance, Some(1))
    assertEquals(all.find(_.id == "blocked").get.importance, Some(1))

  test("resolve gives every open thread its label, lent labels, and importance without building the tree"):
    val threads  = List(t("blocked"), t("blocker"), t("child", Some("blocked")))
    val own      = Map("blocked" -> ThreadLabels(Some(Goal.B), Some(1)))
    val resolved = Overview.resolve(threads, own, Map("child" -> List("blocked")), Map("blocked" -> List("blocker")))
    assertEquals(resolved("child").label, Some(Effective(Goal.B, Some(1), LabelSource.Inherited("blocked"))))
    assertEquals(resolved("blocker").lent, List(Lent("blocked", Goal.B, Some(1))))
    assertEquals(resolved("blocker").importance, Some(1))
    assertEquals(resolved.keySet, Set("blocked", "blocker", "child"))

  test("a blocker that is closed is reported so the picker can say so"):
    val threads  = List(t("a"))
    val own      = Map("a" -> ThreadLabels(Some(Goal.B), Some(1)))
    val resolved = Overview.resolve(threads, own, Map.empty, Map("a" -> List("gone", "a")))
    assertEquals(resolved("a").closedBlockers, List("gone"))

  test("blocking lends through a closed thread"):
    val threads = List(t("a"), t("b"))
    val own     = Map("a" -> ThreadLabels(Some(Goal.B), Some(1)))
    val waits   = Map("a" -> List("closed"), "closed" -> List("b"))
    val all     = Overview.build(threads, own, Map.empty, waits, Map.empty).groups.flatMap(_.threads)
    assertEquals(all.find(_.id == "b").get.lent, List(Lent("a", Goal.B, Some(1))))

  test("floor weights give every thread at least the minimum share of the drawn area and keep real proportions"):
    // 100 px² minimum in 10 000 px²: no thread may fall under 1% of the total weight.
    val weights = Treemap.floorWeights(List(900.0, 100.0, 0.0, 0.0), areaPx = 10_000, minPx = 100)
    val total   = weights.sum
    weights.foreach(w => assert(w / total >= 0.01 - 1e-9, weights))
    assertEqualsDouble(weights(0) / weights(1), 9.0, 1e-9)
    // All zero: equal shares.
    assertEquals(Treemap.floorWeights(List(0.0, 0.0), areaPx = 100, minPx = 10).distinct.size, 1)
    // Too many threads for the minimum to fit: still equal-floor, never negative or NaN.
    val crowded = Treemap.floorWeights(List.fill(200)(0.0) :+ 50.0, areaPx = 1000, minPx = 100)
    assert(crowded.forall(w => w > 0 && !w.isNaN))

  test("a thread is active in a window when it was last active then, got attention, or carries a my-move mark, archived or not"):
    val old    = ThreadInfo("old", None, "p", "old", activeAt = 100L, archived = true)
    val fresh  = ThreadInfo("fresh", None, "p", "fresh", activeAt = 900L, archived = true)
    val marked = ThreadLabels(None, None, myMove = Some(MyMove.Focus))
    val since  = 500L
    assert(!Overview.active(old, None, seconds = 0.0, since))
    assert(Overview.active(fresh, None, seconds = 0.0, since))
    assert(Overview.active(old, None, seconds = 30.0, since))
    assert(Overview.active(old, Some(marked), seconds = 0.0, since))
