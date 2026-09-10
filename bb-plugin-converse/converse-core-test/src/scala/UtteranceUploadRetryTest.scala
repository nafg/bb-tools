package conversecore

import TranscriptionFailure.*

class UtteranceUploadRetryTest extends munit.FunSuite:
  test("a failed upload is retried three times with growing delays, then given up"):
    assertEquals(UtteranceUploadRetry.delayBeforeRetry(1, Other), Some(1000))
    assertEquals(UtteranceUploadRetry.delayBeforeRetry(2, Other), Some(3000))
    assertEquals(UtteranceUploadRetry.delayBeforeRetry(3, Other), Some(8000))
    assertEquals(UtteranceUploadRetry.delayBeforeRetry(4, Other), None)
    assertEquals(UtteranceUploadRetry.MaxAttempts, 4)

  test("a rate limit is retried on the same schedule"):
    assertEquals(UtteranceUploadRetry.delayBeforeRetry(1, RateLimited), Some(1000))

  test("a forgotten session is not a retry case: it is re-registered instead"):
    assertEquals(UtteranceUploadRetry.delayBeforeRetry(1, SessionForgotten), None)
