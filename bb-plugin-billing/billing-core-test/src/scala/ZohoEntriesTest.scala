package billingcore

class ZohoEntriesTest extends munit.FunSuite:
  test("billed minutes round up to whole minutes and then to the increment"):
    assertEquals(ZohoEntries.billedMinutes(0, 1), 0L)
    assertEquals(ZohoEntries.billedMinutes(1, 1), 1L)
    assertEquals(ZohoEntries.billedMinutes(60_000, 1), 1L)
    assertEquals(ZohoEntries.billedMinutes(61_000, 1), 2L)
    assertEquals(ZohoEntries.billedMinutes(7 * 60_000, 6), 12L)
    assertEquals(ZohoEntries.billedMinutes(12 * 60_000, 6), 12L)
    assertEquals(ZohoEntries.billedMinutes(90 * 60_000 + 1, 15), 105L)

  test("log_time is HH:mm"):
    assertEquals(ZohoEntries.logTime(5), "00:05")
    assertEquals(ZohoEntries.logTime(125), "02:05")

  test("notes are the thread title cut to Zoho's limit"):
    assertEquals(ZohoEntries.notes("  Fix the login bug  "), "Fix the login bug")
    val long = "x" * 600
    assertEquals(ZohoEntries.notes(long).length, 500)
    assert(ZohoEntries.notes(long).endsWith("…"))
