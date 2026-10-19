package billingcore

class CalendarTest extends munit.FunSuite:
  test("civil dates round-trip through epoch days"):
    for date <- List(CivilDate(1970, 1, 1), CivilDate(2000, 2, 29), CivilDate(2026, 10, 18), CivilDate(2024, 12, 31)) do
      assertEquals(CivilDate.fromEpochDay(date.epochDay), date)
    assertEquals(CivilDate(1970, 1, 1).epochDay, 0L)
    assertEquals(CivilDate(2026, 10, 18).iso, "2026-10-18")

  test("a fixed-offset calendar places instants on local days"):
    val eastern = Calendar.FixedOffset(-4L * 3600_000) // UTC-4
    val t       = 1_776_600_000_000L                   // 2026-04-19T12:00:00Z
    assertEquals(eastern.dayOf(t), "2026-04-19")
    assertEquals(eastern.dayOf(t - 10 * 3600_000), "2026-04-18") // 02:00Z is 22:00 the day before
    assertEquals(eastern.dayStart(t), 1_776_571_200_000L)        // 2026-04-19T04:00:00Z
    assertEquals(eastern.nextDayStart(t), eastern.dayStart(t) + Calendar.DayMs)
    assertEquals(eastern.startOfDay("2026-04-19"), eastern.dayStart(t))
