package billingcore

class SessionViewTest extends munit.FunSuite:
  private def min(m: Int): Long = m * 60_000L
  private def stretch(thread: String, from: Int, to: Int): SessionStretch =
    SessionStretch(Window(thread, "2026-10-19", min(from), min(to)), StretchState.Unbilled, None)
  private def thread(id: String, parent: Option[String] = None, created: Int = -1000, archived: Option[Int] = None) =
    ThreadInfo(id, id, parent, Billing.Bills, min(created), archived.map(min))
  private val session = Session(min(0), min(600), Set.empty)

  test("a run of tiny stretches close together is one cluster, a long one stands alone"):
    val ms = SessionView.marks(Seq(stretch("a", 0, 1), stretch("a", 3, 4), stretch("a", 6, 7), stretch("a", 60, 90)))
    assertEquals(ms.length, 2)
    assert(ms.head.isInstanceOf[Mark.Cluster])
    assertEquals(ms.head.members.length, 3)
    assert(ms(1).isInstanceOf[Mark.One])

  test("tiny stretches far apart do not cluster"):
    assertEquals(SessionView.marks(Seq(stretch("a", 0, 1), stretch("a", 30, 31))).length, 2)

  test("marks come out in time order whatever the input order"):
    val ms = SessionView.marks(Seq(stretch("a", 60, 90), stretch("a", 0, 30)))
    assertEquals(ms.map(_.start), Vector(min(0), min(60)))

  test("a parent with only child activity appears as an empty container"):
    val forest = SessionView.forest(session, Seq(thread("p"), thread("c", Some("p"))), Seq(stretch("c", 10, 40)))
    assertEquals(forest.map(_.info.id), Vector("p"))
    assertEquals(forest.head.own, Vector.empty)
    assertEquals(forest.head.children.map(_.info.id), Vector("c"))

  test("a parent's own marks never include its children's stretches"):
    val forest = SessionView.forest(
      session,
      Seq(thread("p"), thread("c", Some("p"))),
      Seq(stretch("p", 0, 30), stretch("c", 10, 40))
    )
    assertEquals(forest.head.own.flatMap(_.members).map(_.window.threadId).distinct, Vector("p"))

  test("a thread with no activity and no active descendant is left out"):
    val forest = SessionView.forest(session, Seq(thread("a"), thread("idle")), Seq(stretch("a", 0, 30)))
    assertEquals(forest.map(_.info.id), Vector("a"))

  test("a thread created mid-session and archived mid-session has real edges; one older and alive is clipped"):
    val forest = SessionView.forest(
      session,
      Seq(thread("young", created = 100, archived = Some(200)), thread("old")),
      Seq(stretch("young", 110, 150), stretch("old", 0, 30))
    )
    val young = forest.find(_.info.id == "young").get
    val old   = forest.find(_.info.id == "old").get
    assertEquals((young.top, young.bottom), (BoxEdge.Real(min(100)), BoxEdge.Real(min(200))))
    assertEquals((old.top, old.bottom), (BoxEdge.ClippedAtSession, BoxEdge.ClippedAtSession))

  test("siblings and roots are ordered by first activity"):
    val forest = SessionView.forest(
      session,
      Seq(thread("late"), thread("early")),
      Seq(stretch("late", 100, 130), stretch("early", 10, 40))
    )
    assertEquals(forest.map(_.info.id), Vector("early", "late"))
