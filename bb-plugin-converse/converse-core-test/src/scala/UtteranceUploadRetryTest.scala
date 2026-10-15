package conversecore

class UtteranceUploadRetryTest extends munit.FunSuite:
  test("a failed upload is retried sooner at first, then every 30 seconds without end"):
    assertEquals((1 to 6).map(UtteranceUploadRetry.delayBeforeRetry).toList, List(1000, 3000, 8000, 15000, 30000, 30000))
    assertEquals(UtteranceUploadRetry.delayBeforeRetry(1000), 30000)
