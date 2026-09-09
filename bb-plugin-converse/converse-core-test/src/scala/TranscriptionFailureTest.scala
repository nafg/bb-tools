package conversecore

class TranscriptionFailureTest extends munit.FunSuite:
  test("the backend rejecting a session it no longer has is a forgotten session"):
    assertEquals(
      TranscriptionFailure.classify(TranscriptionFailure.SessionForgottenMessage),
      TranscriptionFailure.SessionForgotten
    )

  test("a 429 from the transcription endpoint is a rate limit, whatever the endpoint said"):
    assertEquals(
      TranscriptionFailure.classify(s"${TranscriptionFailure.RateLimitedMessage}: {\"error\":\"slow down\"}"),
      TranscriptionFailure.RateLimited
    )

  test("any other endpoint status is not a rate limit"):
    assertEquals(
      TranscriptionFailure.classify("transcription endpoint returned 503: upstream unavailable"),
      TranscriptionFailure.Other
    )

  test("a failure without a recognizable message is not classified"):
    assertEquals(TranscriptionFailure.classify(""), TranscriptionFailure.Other)
    assertEquals(TranscriptionFailure.classify("could not read recording"), TranscriptionFailure.Other)
