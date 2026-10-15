package conversecore

class RetryBackoffTest extends munit.FunSuite:
  test("retries come sooner at first, then every 30 seconds without end"):
    assertEquals((1 to 6).map(RetryBackoff.delayBeforeRetry).toList, List(1000, 3000, 8000, 15000, 30000, 30000))
    assertEquals(RetryBackoff.delayBeforeRetry(1000), 30000)
