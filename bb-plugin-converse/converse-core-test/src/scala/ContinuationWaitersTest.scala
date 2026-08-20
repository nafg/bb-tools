package conversecore

import scala.concurrent.ExecutionContext

class ContinuationWaitersTest extends munit.FunSuite:
  given ExecutionContext = ExecutionContext.parasitic

  test("the next utterance completes a pending continuation wait"):
    val waiters = ContinuationWaiters()
    val result  = waiters.begin("thread-1")

    assert(waiters.deliver("thread-1", "and one more thing"))
    result.map(value => assertEquals(value, Some("and one more thing")))

  test("expiration completes a pending wait without speech"):
    val waiters = ContinuationWaiters()
    val result  = waiters.begin("thread-1")

    assert(waiters.expire("thread-1"))
    result.map(value => assertEquals(value, None))

  test("an utterance is not consumed when no agent is waiting"):
    val waiters = ContinuationWaiters()
    assert(!waiters.deliver("thread-1", "ordinary steer"))

  test("overlapping waits share one pending continuation"):
    val waiters = ContinuationWaiters()
    val first   = waiters.begin("thread-1")
    val second  = waiters.begin("thread-1")

    assert(waiters.deliver("thread-1", "shared continuation"))
    first.zip(second).map { case (firstResult, secondResult) =>
      assertEquals(firstResult, Some("shared continuation"))
      assertEquals(secondResult, Some("shared continuation"))
    }
