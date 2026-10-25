package conversecore

class CharacterTallyTest extends munit.FunSuite:
  test("characters sent in the same month add up"):
    assertEquals(CharacterTally("2026-10", 100).add("2026-10", 50), CharacterTally("2026-10", 150))

  test("a new month starts the tally over"):
    assertEquals(CharacterTally("2026-10", 3_900_000).add("2026-11", 50), CharacterTally("2026-11", 50))

  test("the month of an instant, in UTC"):
    assertEquals(CharacterTally.monthOf(0), "1970-01")
    // 2026-10-25T23:30:00Z
    assertEquals(CharacterTally.monthOf(1792971000000.0), "2026-10")
    // 2026-11-01T00:00:00Z and the millisecond before it
    assertEquals(CharacterTally.monthOf(1793491200000.0), "2026-11")
    assertEquals(CharacterTally.monthOf(1793491199999.0), "2026-10")
    // 2024-02-29T12:00:00Z, a leap day
    assertEquals(CharacterTally.monthOf(1709208000000.0), "2024-02")
