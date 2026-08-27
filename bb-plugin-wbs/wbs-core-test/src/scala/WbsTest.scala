package wbscore

class WbsTest extends munit.FunSuite:
  private def parsed(src: String): List[Node] = Wbs.parse(src).fold(e => fail(e.getMessage), identity)

  test("plain numbers and nesting sum up"):
    val nodes = parsed("""Authentication:
                         |  Login form: 2
                         |  Password reset: 3.5
                         |Deployment: 2
                         |""".stripMargin)
    assertEquals(Wbs.estimate(nodes).text, "7h 30m")
    assertEquals(Wbs.estimate(nodes.head).text, "5h 30m")

  test("ranges propagate and collapse when equal"):
    val nodes = parsed("""A: [1, 3]
                         |B: 2
                         |C: [2, 2]
                         |""".stripMargin)
    assertEquals(Wbs.estimate(nodes).text, "5h–7h")
    assertEquals(Wbs.estimate(nodes(2)).text, "2h")

  test("placeholders contribute nothing and mark incomplete"):
    val nodes = parsed("""A: 2
                         |B: scope not confirmed
                         |C:
                         |  D: waiting on client
                         |  E: 1
                         |""".stripMargin)
    assertEquals(Wbs.estimate(nodes).text, "3h + 2 unestimated")
    assertEquals(Wbs.placeholders(nodes), List(List("B") -> "scope not confirmed", List("C", "D") -> "waiting on client"))

  test("a then/else fork counts exactly one side"):
    val nodes = parsed("""if Twilio URL plays unauthenticated:
                         |  then:
                         |    Use URL directly: 0.5
                         |  else:
                         |    Signed route: 3
                         |    Tests for it: 1
                         |Deploy: 0.5
                         |""".stripMargin)
    assertEquals(Wbs.estimate(nodes).text, "1h–4h 30m")
    assertEquals(Wbs.propositions(nodes).map(_._2), List("Twilio URL plays unauthenticated"))

  test("then-only and else-only forks are optional work"):
    assertEquals(Wbs.estimate(parsed("Base: 3\nif client wants SSO:\n  then:\n    SSO: 2\n")).text, "3h–5h")
    assertEquals(Wbs.estimate(parsed("Base: 3\nif block applies to every event:\n  else:\n    Per-event flag: 1\n")).text, "3h–4h")

  test("the same proposition is assigned consistently across branches"):
    val nodes = parsed("""X:
                         |  if new  renderer:
                         |    then:
                         |      Cheap: 1
                         |    else:
                         |      Dear: 10
                         |Y:
                         |  if New renderer:
                         |    then:
                         |      Dear: 10
                         |    else:
                         |      Cheap: 1
                         |""".stripMargin)
    // Either assignment yields 11; independent choices would give 2–20.
    assertEquals(Wbs.estimate(nodes).text, "11h")
    assertEquals(Wbs.propositions(nodes).map(_._2), List("new  renderer"))

  test("fork shape is enforced"):
    assert(Wbs.parse("if X:\n  then: 2\n").isLeft, "branch must be a group")
    assert(Wbs.parse("if X:\n  maybe:\n    A: 1\n").isLeft, "only then/else")
    assert(Wbs.parse("if X:\n  then:\n    A: 1\n  extra:\n    B: 1\n").isLeft, "no extra keys")
    assert(Wbs.parse("if X: 2\n").isLeft, "fork value must be a mapping")
    assert(Wbs.parse("if X: {}\n").isLeft, "needs then or else")

  test("quoted keys and comments"):
    val nodes = parsed("""'Fix logout (#6)': 1  # trailing comment
                         |# full-line comment
                         |"A: B": 2
                         |Note: it's #1 priority
                         |""".stripMargin)
    assertEquals(nodes.collect { case Node.Item(n, _) => n }, List("Fix logout (#6)", "A: B", "Note"))
    // ` #` starts a comment even inside a plain scalar, as the skill warns.
    assertEquals(nodes(2), Node.Item("Note", Node.Value.Leaf(Leaf.Placeholder("it's"))))

  test("format errors name the path"):
    assertEquals(Wbs.parse("A:\n  B: [3, 1]\n").left.map(_.getMessage), Left("A / B: range low (3h) exceeds high (1h)"))
    assert(Wbs.parse("A: [1, 2, 3]\n").isLeft)
    assert(Wbs.parse("A:\n").isLeft, "null value")
    assert(Wbs.parse("A: 1\n   B: 2\n").isLeft)

  test("hours formatting"):
    assertEquals(Estimate.formatHours(2.0), "2h")
    assertEquals(Estimate.formatHours(2.5), "2h 30m")
    assertEquals(Estimate.formatHours(1.25), "1h 15m")
    assertEquals(Estimate.formatHours(1.0 / 3), "20m")
    assertEquals(Estimate.formatHours(0.0), "0")

  test("html collapses to depth, forks are transparent, and escapes"):
    val nodes = parsed("""A <b>:
                         |  B:
                         |    C: 1
                         |  D: 2
                         |if X:
                         |  then:
                         |    E:
                         |      F: 4
                         |G: needs scope
                         |""".stripMargin)
    val full = Html.render(nodes, None)
    assert(!full.contains("<html"), "fragment, not a document")
    assert(full.startsWith("<table"))
    assert(full.contains("A &lt;b&gt;"))
    assert(full.contains(">C<"))
    assert(full.contains("if X"))
    val shallow = Html.render(nodes, Some(1))
    assert(!shallow.contains(">B<"))
    assert(shallow.contains(">E<"), "fork does not consume a level")
    assert(!shallow.contains(">F<"))
    assert(shallow.contains(">3h</td>"))
    assert(shallow.contains("Not yet estimated"))
