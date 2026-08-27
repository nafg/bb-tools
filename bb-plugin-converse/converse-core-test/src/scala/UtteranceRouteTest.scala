package conversecore

class UtteranceRouteTest extends munit.FunSuite:
  test("capture start latches the currently viewed thread"):
    assertEquals(
      UtteranceRoute.latch(viewedThreadId = Some("thread-b"), sessionThreadId = Some("thread-a")),
      Some(UtteranceRoute("thread-b"))
    )

  test("the session thread is the fallback when no thread is currently viewed"):
    assertEquals(
      UtteranceRoute.latch(viewedThreadId = None, sessionThreadId = Some("thread-a")),
      Some(UtteranceRoute("thread-a"))
    )

  test("a latched route does not change when navigation changes later"):
    val route = UtteranceRoute.latch(Some("thread-a"), Some("thread-a"))

    val _ = UtteranceRoute.latch(Some("thread-b"), Some("thread-a"))

    assertEquals(route, Some(UtteranceRoute("thread-a")))
