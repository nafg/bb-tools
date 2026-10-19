package activitycore

class ConverseSpansTest extends munit.FunSuite:
  private def span(seq: Long, kind: String = "speech", threadId: String = "t", source: String = "session", start: Long = 1000, end: Long = 2000) =
    ConverseSpan(seq, s"id-$seq", threadId, kind, source, start, end)

  test("a page's cursor advances past items it drops"):
    val page = ConverseSpans.fold(Vector(span(1), span(2, kind = "typing"), span(3, threadId = ""), span(4, kind = "playback", end = 9000)))
    assertEquals(page.signals.map(_.id), Vector("id-1", "id-4"))
    assertEquals(page.lastSeq, Some(4L))
    assertEquals(page.newestEnd, Some(9000L))

  test("a page of only unusable items still advances the cursor and has no newest end"):
    val page = ConverseSpans.fold(Vector(span(7, kind = "typing"), span(9, threadId = "")))
    assertEquals(page.signals, Vector.empty)
    assertEquals(page.lastSeq, Some(9L))
    assertEquals(page.newestEnd, None)

  test("an empty page changes nothing"):
    assertEquals(ConverseSpans.fold(Vector.empty), ConverseSpans.Page(Vector.empty, None, None))

  test("sources are prefixed and an end before the start is clamped"):
    assertEquals(ConverseSpans.signal(span(1)).map(_.source), Some("converse"))
    assertEquals(ConverseSpans.signal(span(1, source = "replay")).map(_.source), Some("converse-replay"))
    assertEquals(ConverseSpans.signal(span(1, source = "backfill")).map(_.source), Some("converse-backfill"))
    assertEquals(ConverseSpans.signal(span(1, start = 5000, end = 4000)).map(s => (s.start, s.end)), Some((5000L, 5000L)))

  test("sequences between the cursor and the oldest span converse still holds are lost"):
    assertEquals(ConverseSpans.lost(cursorSeq = Some(10), oldestSeq = Some(50)), Some((11L, 49L)))
    assertEquals(ConverseSpans.lost(cursorSeq = None, oldestSeq = Some(50)), Some((1L, 49L)))
    assertEquals(ConverseSpans.lost(cursorSeq = Some(10), oldestSeq = Some(11)), None)
    assertEquals(ConverseSpans.lost(cursorSeq = Some(10), oldestSeq = Some(5)), None)
    assertEquals(ConverseSpans.lost(cursorSeq = None, oldestSeq = Some(1)), None)
    assertEquals(ConverseSpans.lost(cursorSeq = Some(10), oldestSeq = None), None)
