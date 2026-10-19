package activitycore

class SummaryTest extends munit.FunSuite:
  private def interval(thread: String, start: Long, end: Long) = Interval(thread, start, end, Set(SignalKind.View))

  test("totals sum each thread's intervals, most time first, ties by thread id, with a count"):
    assertEquals(
      Summary.totals(Seq(interval("a", 0, 60_000), interval("b", 0, 180_000), interval("a", 100_000, 160_000))),
      Vector(ThreadTotal("b", 180_000, 1), ThreadTotal("a", 120_000, 2))
    )
    assertEquals(
      Summary.totals(Seq(interval("b", 0, 60_000), interval("a", 0, 60_000))).map(_.threadId),
      Vector("a", "b")
    )

  test("a thread with only zero-length intervals is touched, not timed"):
    val totals = Summary.totals(Seq(interval("a", 5000, 5000)))
    assertEquals(totals, Vector(ThreadTotal("a", 0, 1)))
    assert(totals.head.touchedOnly)

  test("durations are whole minutes"):
    assertEquals(Summary.formatDuration(0), "0m")
    assertEquals(Summary.formatDuration(59_999), "<1m")
    assertEquals(Summary.formatDuration(12 * 60_000 + 30_000), "12m")
    assertEquals(Summary.formatDuration(3 * 3_600_000 + 7 * 60_000), "3h 07m")
