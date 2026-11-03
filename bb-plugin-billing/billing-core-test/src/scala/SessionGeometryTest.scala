package billingcore

import activitycore.{Window, Session}

class SessionGeometryTest extends munit.FunSuite:
  private def min(m: Int): Long = m * 60_000L
  private def stretch(thread: String, from: Int, to: Int): SessionStretch =
    SessionStretch(Window(thread, "2026-10-19", min(from), min(to)), StretchState.Unbilled, None)
  private def thread(id: String, parent: Option[String] = None) =
    ThreadInfo(id, id, parent, Billing.Bills, min(-1000), None)
  private def layout(threads: Seq[ThreadInfo], stretches: Seq[SessionStretch], end: Int = 600) =
    val session = Session(min(0), min(end), Set.empty)
    SessionGeometry.place(session, SessionView.forest(session, threads, stretches))

  private def allMarks(p: PlacedBox): Vector[PlacedMark] = p.own ++ p.children.flatMap(allMarks)

  test("every mark is at least as tall as its minimum"):
    val l = layout(Seq(thread("a")), Seq(stretch("a", 0, 1), stretch("a", 100, 160), stretch("a", 300, 480)))
    l.roots.flatMap(allMarks).foreach { pm =>
      assert(pm.yBottom - pm.yTop >= SessionGeometry.minHeight(pm.mark) - 1e-9, pm)
    }

  test("heights only increase down the page"):
    val l = layout(Seq(thread("a")), Seq(stretch("a", 0, 30), stretch("a", 100, 130)))
    val ys = l.axis.map(_._1)
    assertEquals(ys, ys.sorted)
    assert(ys.zip(ys.drop(1)).forall((a, b) => b > a))

  test("two threads at the same moment are level"):
    val l = layout(Seq(thread("a"), thread("b")), Seq(stretch("a", 10, 40), stretch("b", 10, 20)))
    val tops = l.roots.flatMap(_.own).map(_.yTop).distinct
    assertEquals(tops.length, 1)

  test("many tiny marks overlapping a long one do not stretch the long one"):
    val tiny = (0 until 20).map(i => stretch("b", i * 5, i * 5 + 1))
    val l    = layout(Seq(thread("a"), thread("b")), stretch("a", 0, 100) +: tiny)
    val long = l.roots.find(_.box.info.id == "a").get.own.head
    // the long bar's minimum is that of 100 minutes; tiny marks beside it must not push it far beyond that
    assert(long.yBottom - long.yTop < SessionGeometry.minHeight(long.mark) * 3, long)

  test("a long quiet spell becomes a labelled break and a short one does not"):
    val l = layout(Seq(thread("a")), Seq(stretch("a", 0, 10), stretch("a", 200, 210)), end = 210)
    assertEquals(l.quiet.map(_.durationMs), Vector(min(190)))
    val short = layout(Seq(thread("a")), Seq(stretch("a", 0, 10), stretch("a", 40, 50)), end = 50)
    assertEquals(short.quiet, Vector.empty)

  test("children sit beside their parent's strip and the box spans them all"):
    val l = layout(
      Seq(thread("p"), thread("c1", Some("p")), thread("c2", Some("p"))),
      Seq(stretch("p", 0, 30), stretch("c1", 10, 40), stretch("c2", 20, 50))
    )
    val p = l.roots.head
    assertEquals((p.column, p.span), (0, 3))
    assertEquals(p.children.map(_.column), Vector(1, 2))
    assertEquals(l.columns, 3)

  test("threads never active together share a column, threads active together do not"):
    val l = layout(
      Seq(thread("a"), thread("b"), thread("c")),
      Seq(stretch("a", 0, 30), stretch("b", 40, 70), stretch("c", 20, 50))
    )
    val cols = l.roots.map(r => r.box.info.id -> r.column).toMap
    assertEquals(cols("a"), cols("b"))
    assertNotEquals(cols("a"), cols("c"))
    assertEquals(l.columns, 2)

  test("axis labels are never closer than the minimum gap"):
    val l  = layout(Seq(thread("a")), (0 until 30).map(i => stretch("a", i * 3, i * 3 + 1)))
    val ys = l.axis.map(_._1)
    assert(ys.zip(ys.drop(1)).forall((a, b) => b - a >= SessionGeometry.AxisMinGapPx - 1e-9))

  test("a mark with text to show is taller than the same mark without"):
    val plain  = Mark.One(stretch("a", 0, 1))
    val texted = Mark.One(stretch("a", 0, 1).copy(text = Some("Wrote the schema")))
    assertEquals(SessionGeometry.minHeight(texted) - SessionGeometry.minHeight(plain), SessionGeometry.TextExtraPx)
    val blank = Mark.One(stretch("a", 0, 1).copy(text = Some("   ")))
    assertEquals(SessionGeometry.minHeight(blank), SessionGeometry.minHeight(plain))
