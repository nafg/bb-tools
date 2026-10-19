package conversecore

class PlaybackReportTest extends munit.FunSuite:
  test("a playback report needs a thread and an end no earlier than its start"):
    assertEquals(PlaybackReport.validate("thr_1", 1000, 2500), Right(()))
    assertEquals(PlaybackReport.validate("thr_1", 1000, 1000), Right(()))
    assert(PlaybackReport.validate("", 1000, 2500).isLeft)
    assert(PlaybackReport.validate("thr_1", 2500, 1000).isLeft)
    assert(PlaybackReport.validate("thr_1", Double.NaN, 1000).isLeft)
