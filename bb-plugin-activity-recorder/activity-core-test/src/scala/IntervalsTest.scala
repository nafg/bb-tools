package activitycore

import SignalKind.*

class IntervalsTest extends munit.FunSuite:
  private val Gap = 5 * 60 * 1000L
  private val T0  = 1_000_000_000_000L

  private def signal(thread: String, kind: SignalKind, start: Long, end: Long, id: String = "") =
    Signal(if id.isEmpty then s"$thread-$kind-$start" else id, thread, kind, T0 + start, T0 + end, "test")

  private def derive(signals: Signal*): Vector[Interval] =
    Intervals.derive(signals, Gap, T0, T0 + 24 * 3600 * 1000L).map(i => i.copy(start = i.start - T0, end = i.end - T0))

  test("signals within the gap on one thread join into one interval"):
    assertEquals(
      derive(signal("a", Speech, 0, 10_000), signal("a", Message, 60_000, 60_000), signal("a", View, 70_000, 300_000)),
      Vector(Interval("a", 0, 300_000, Set(Speech, Message, View)))
    )

  test("a gap of exactly the threshold still joins; one longer splits and counts nothing"):
    assertEquals(
      derive(signal("a", Speech, 0, 1000), signal("a", Speech, 1000 + Gap, 2000 + Gap)),
      Vector(Interval("a", 0, 2000 + Gap, Set(Speech)))
    )
    assertEquals(
      derive(signal("a", Speech, 0, 1000), signal("a", Speech, 1001 + Gap, 2000 + Gap)),
      Vector(Interval("a", 0, 1000, Set(Speech)), Interval("a", 1001 + Gap, 2000 + Gap, Set(Speech)))
    )

  test("threads are independent and their intervals overlap freely"):
    assertEquals(
      derive(signal("b", View, 0, 30_000), signal("a", View, 0, 30_000)),
      Vector(Interval("a", 0, 30_000, Set(View)), Interval("b", 0, 30_000, Set(View)))
    )

  test("a span contained in an earlier one does not shorten the interval"):
    assertEquals(
      derive(signal("a", View, 0, 100_000), signal("a", Speech, 10_000, 20_000)),
      Vector(Interval("a", 0, 100_000, Set(View, Speech)))
    )

  test("a lone typed message is a zero-length interval"):
    assertEquals(derive(signal("a", Message, 5000, 5000)), Vector(Interval("a", 5000, 5000, Set(Message))))

  test("intervals are clipped to the window, and signals outside it still join those inside"):
    val from = T0 + 100_000
    val to   = T0 + 200_000
    val clipped = Intervals.derive(
      Seq(signal("a", View, 50_000, 150_000), signal("a", View, 150_000 + Gap, 400_000)),
      Gap,
      from,
      to
    )
    assertEquals(clipped, Vector(Interval("a", from, to, Set(View))))

  test("a point signal on the window's end edge or beyond is dropped"):
    val from = T0 + 100_000
    val to   = T0 + 200_000
    assertEquals(Intervals.derive(Seq(signal("a", Message, 200_000, 200_000)), 0, from, to), Vector.empty)
    assertEquals(Intervals.derive(Seq(signal("a", Message, 99_999, 99_999)), 0, from, to), Vector.empty)
    assertEquals(
      Intervals.derive(Seq(signal("a", Message, 100_000, 100_000)), 0, from, to),
      Vector(Interval("a", from, from, Set(Message)))
    )

  test("a span ending exactly at the window start contributes nothing"):
    assertEquals(Intervals.derive(Seq(signal("a", View, 0, 100_000)), 0, T0 + 100_000, T0 + 200_000), Vector.empty)

  test("no signals, no intervals"):
    assertEquals(derive(), Vector.empty)

class SignalKindTest extends munit.FunSuite:
  test("names round-trip and unknown names are rejected"):
    SignalKind.values.foreach(kind => assertEquals(SignalKind.parse(kind.name), Some(kind)))
    assertEquals(SignalKind.parse("typing"), None)

  test("attention excludes the agent's turns"):
    assertEquals(SignalKind.Attention, SignalKind.values.toSet - SignalKind.Turn)
