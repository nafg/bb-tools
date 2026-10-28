package billingcore

class SessionsTest extends munit.FunSuite:
  private def min(m: Int): Long                           = m * 60_000L
  private def w(thread: String, from: Int, to: Int): Window = Window(thread, "2026-10-19", min(from), min(to))

  test("no stretches, no sessions"):
    assertEquals(Sessions.of(Nil), Vector.empty)

  test("stretches closer together than the gap are one session"):
    val s = Sessions.of(Seq(w("a", 0, 10), w("b", 70, 80), w("a", 200, 210)))
    assertEquals(s, Vector(Session(min(0), min(210), Set("a", "b"))))

  test("a gap of exactly the session gap ends the session"):
    val gap = Sessions.DefaultGapMs / 60_000
    val s   = Sessions.of(Seq(w("a", 0, 10), w("a", 10 + gap.toInt, 20 + gap.toInt)))
    assertEquals(s.length, 2)

  test("input order does not matter and sessions come oldest first"):
    val late  = w("b", 1000, 1010)
    val early = w("a", 0, 10)
    assertEquals(Sessions.of(Seq(late, early)).map(_.start), Vector(min(0), min(1000)))

  test("a long stretch covering later ones does not end the session early"):
    val s = Sessions.of(Seq(w("a", 0, 900), w("b", 100, 110), w("c", 1000, 1010)))
    assertEquals(s.length, 1)

  test("a stretch belongs to the session it starts in"):
    val ws = Seq(w("a", 0, 10), w("a", 2000, 2010))
    val ss = Sessions.of(ws)
    assertEquals(Sessions.containing(ss, ws(1)), ss.lastOption)

  test("height never drops below the minimum and grows with time, slowly"):
    assertEquals(StretchHeight.px(0), StretchHeight.MinPx)
    assert(StretchHeight.px(min(180)) > StretchHeight.px(min(30)))
    assert(StretchHeight.px(min(60)) < 2 * StretchHeight.px(min(30)))
