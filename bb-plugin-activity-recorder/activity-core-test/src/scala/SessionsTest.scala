package activitycore

class SessionsTest extends munit.FunSuite:
  private def min(m: Int): Long                             = m * 60_000L
  private def w(thread: String, from: Int, to: Int): Window = Window(thread, "2026-10-19", min(from), min(to))
  private def span(thread: String, from: Int, to: Int, by: Starter = Starter.User): AgentSpan =
    AgentSpan(thread, min(from), min(to), by)

  test("no stretches, no sessions"):
    assertEquals(Sessions.of(Nil), Vector.empty)

  test("stretches closer together than the gap are one session"):
    val s = Sessions.of(Seq(w("a", 0, 10), w("b", 70, 80), w("a", 170, 180)))
    assertEquals(s, Vector(Session(min(0), min(180), Set("a", "b"))))

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

  test("an agent turn the user set going keeps the session open until it ends"):
    // Attention 0-10, the agent works until minute 400, the user returns at 450: 50 minutes after it finished.
    val ws = Seq(w("a", 0, 10), w("a", 450, 460))
    assertEquals(Sessions.of(ws).length, 2)
    val s = Sessions.of(ws, Seq(span("a", 5, 400)))
    assertEquals(s, Vector(Session(min(0), min(460), Set("a"))))

  test("the gap counts from the end of the agent's work"):
    val s = Sessions.of(Seq(w("a", 0, 10), w("a", 600, 610)), Seq(span("a", 5, 400)))
    assertEquals(s.map(_.start), Vector(min(0), min(600)))
    assertEquals(s.head.end, min(400))

  test("a turn another thread started counts when that thread is in the session"):
    val ws = Seq(w("parent", 0, 10), w("parent", 500, 510))
    val s  = Sessions.of(ws, Seq(span("child", 5, 400, Starter.Threads(Set("parent")))))
    assertEquals(s.length, 1)
    assertEquals(s.head.threadIds, Set("parent", "child"))

  test("turns chain: a thread the session's thread woke can wake another"):
    val ws    = Seq(w("a", 0, 10), w("a", 500, 510))
    val spans = Seq(span("b", 5, 100, Starter.Threads(Set("a"))), span("c", 90, 400, Starter.Threads(Set("b"))))
    assertEquals(Sessions.of(ws, spans).length, 1)

  test("a turn started by a thread outside the session is ignored"):
    val ws = Seq(w("a", 0, 10), w("a", 500, 510))
    assertEquals(Sessions.of(ws, Seq(span("x", 5, 400, Starter.Threads(Set("stranger"))))).length, 2)

  test("an unqualified span cannot start a session of its own"):
    assertEquals(Sessions.of(Nil, Seq(span("x", 0, 50, Starter.Threads(Set("stranger"))))), Vector.empty)

  test("a span of the user's own can start a session"):
    assertEquals(Sessions.of(Nil, Seq(span("a", 0, 50))), Vector(Session(min(0), min(50), Set("a"))))
