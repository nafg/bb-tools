package conversecore

import TranscriptionFailure.{Other, RateLimited, SessionForgotten}

class PreviewBackoffTest extends munit.FunSuite:
  test("the first rate limit pauses previews briefly"):
    assertEquals(PreviewBackoff.pauseAfter(RateLimited, previousPauseMs = None), Some(PreviewBackoff.InitialPauseMs))

  test("each consecutive rate limit doubles the pause"):
    assertEquals(PreviewBackoff.pauseAfter(RateLimited, previousPauseMs = Some(10_000.0)), Some(20_000.0))
    assertEquals(PreviewBackoff.pauseAfter(RateLimited, previousPauseMs = Some(20_000.0)), Some(40_000.0))

  test("the pause never exceeds the maximum"):
    assertEquals(PreviewBackoff.pauseAfter(RateLimited, previousPauseMs = Some(40_000.0)), Some(PreviewBackoff.MaxPauseMs))
    assertEquals(PreviewBackoff.pauseAfter(RateLimited, previousPauseMs = Some(60_000.0)), Some(PreviewBackoff.MaxPauseMs))

  test("a session the backend forgot does not pause previews"):
    assertEquals(PreviewBackoff.pauseAfter(SessionForgotten, previousPauseMs = None), None)

  test("a failure that is not a rate limit does not pause previews, even during a rate-limit streak"):
    assertEquals(PreviewBackoff.pauseAfter(Other, previousPauseMs = Some(20_000.0)), None)
