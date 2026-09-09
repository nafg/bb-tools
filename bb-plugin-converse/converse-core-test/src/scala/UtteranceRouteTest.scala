package conversecore

import UtteranceRoute.ComposeSurface

class UtteranceRouteTest extends munit.FunSuite:
  test("capture start latches the currently viewed thread"):
    assertEquals(
      UtteranceRoute.latch(None, viewedThreadId = Some("thread-b"), sessionThreadId = Some("thread-a")),
      Some(UtteranceRoute.ToThread("thread-b"))
    )

  test("the session thread is the fallback when no thread is currently viewed"):
    assertEquals(
      UtteranceRoute.latch(None, viewedThreadId = None, sessionThreadId = Some("thread-a")),
      Some(UtteranceRoute.ToThread("thread-a"))
    )

  test("a latched route does not change when navigation changes later"):
    val route = UtteranceRoute.latch(None, Some("thread-a"), Some("thread-a"))

    val _ = UtteranceRoute.latch(None, Some("thread-b"), Some("thread-a"))

    assertEquals(route, Some(UtteranceRoute.ToThread("thread-a")))

  test("the New thread screen routes to a new thread in its project, not the last viewed thread"):
    assertEquals(
      UtteranceRoute.latch(Some(ComposeSurface(Some("proj-1"))), Some("thread-a"), Some("thread-a")),
      Some(UtteranceRoute.NewThread("proj-1"))
    )

  test("the New thread screen without a project gives the utterance nowhere to go"):
    assertEquals(
      UtteranceRoute.latch(Some(ComposeSurface(None)), Some("thread-a"), Some("thread-a")),
      None
    )
