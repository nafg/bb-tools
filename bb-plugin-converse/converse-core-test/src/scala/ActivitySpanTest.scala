package conversecore

class ActivitySpanTest extends munit.FunSuite:
  test("a source is read back from the code it is stored under"):
    for source <- ActivitySource.values do assertEquals(ActivitySource.fromCode(source.code), Some(source))
    assertEquals(ActivitySource.fromCode("other"), None)

  test("the browser reports session and replay spans, never backfilled ones"):
    assertEquals(ActivitySource.Reported, Set(ActivitySource.Session, ActivitySource.Replay))
