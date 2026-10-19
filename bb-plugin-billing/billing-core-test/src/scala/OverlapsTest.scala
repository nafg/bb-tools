package billingcore

class OverlapsTest extends munit.FunSuite:
  private val Day                                      = "2026-10-19"
  private def at(minutes: Int): Long                   = 1_000_000_000L + minutes * 60_000L
  private def span(thread: String, from: Int, to: Int) = DayInterval(Day, thread, at(from), at(to))

  test("intervals crossing midnight split into one piece per day"):
    val cal   = Calendar.Utc
    val start = cal.startOfDay("2026-10-19") - 30 * 60_000
    val end   = cal.startOfDay("2026-10-20") + 15 * 60_000
    val split = Intervals.splitByDay(Seq(Interval("a", start, end)), cal)
    assertEquals(
      split.map(d => (d.day, d.durationMs / 60_000)),
      Vector(("2026-10-18", 30L), ("2026-10-19", 1440L), ("2026-10-20", 15L))
    )

  test("a zero-length piece at midnight is dropped"):
    val cal   = Calendar.Utc
    val start = cal.startOfDay("2026-10-19") - 10 * 60_000
    val split = Intervals.splitByDay(Seq(Interval("a", start, cal.startOfDay("2026-10-19"))), cal)
    assertEquals(split.map(_.day), Vector("2026-10-18"))

  test("disjoint intervals produce no blocks and full exclusive time"):
    val intervals = Seq(span("a", 0, 10), span("b", 10, 25))
    assertEquals(Overlaps.blocks(Day, intervals), Vector.empty)

  test("a block ends whenever the set of attended threads changes"):
    // a: 0-20, b: 10-30, c: 15-25 — the attended set changes at 15, 20, and 25
    val intervals = Seq(span("a", 0, 20), span("b", 10, 30), span("c", 15, 25))
    assertEquals(
      Overlaps.blocks(Day, intervals),
      Vector(
        OverlapBlock(Day, at(10), at(15), Set("a", "b")),
        OverlapBlock(Day, at(15), at(20), Set("a", "b", "c")),
        OverlapBlock(Day, at(20), at(25), Set("b", "c"))
      )
    )

  test("single-thread stretches separate blocks"):
    val intervals = Seq(span("a", 0, 20), span("b", 10, 30), span("c", 25, 27))
    assertEquals(
      Overlaps.blocks(Day, intervals),
      Vector(OverlapBlock(Day, at(10), at(20), Set("a", "b")), OverlapBlock(Day, at(25), at(27), Set("b", "c")))
    )

  test("block keys round-trip through text"):
    val key = BlockKey(Day, at(10), at(20), Set("b", "a"))
    assertEquals(key.text, s"$Day|${at(10)}|${at(20)}|a,b")
    assertEquals(BlockKey.parse(key.text), Some(key))
