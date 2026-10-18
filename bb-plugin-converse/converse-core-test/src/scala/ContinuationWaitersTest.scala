package conversecore

import scala.concurrent.ExecutionContext

class ContinuationWaitersTest extends munit.FunSuite:
  given ExecutionContext = ExecutionContext.parasitic

  test("the next utterance ends a pending wait as continued"):
    val waiters = ContinuationWaiters()
    val result  = waiters.begin("thread-1")

    assert(waiters.continued("thread-1"))
    result.map(value => assert(value))

  test("expiration ends a pending wait without speech"):
    val waiters = ContinuationWaiters()
    val result  = waiters.begin("thread-1")

    assert(waiters.expire("thread-1"))
    result.map(value => assert(!value))

  test("an utterance ends no wait when no agent is waiting"):
    val waiters = ContinuationWaiters()
    assert(!waiters.continued("thread-1"))

  test("overlapping waits share one pending wait"):
    val waiters = ContinuationWaiters()
    val first   = waiters.begin("thread-1")
    val second  = waiters.begin("thread-1")

    assert(waiters.continued("thread-1"))
    first.zip(second).map((firstResult, secondResult) => assert(firstResult && secondResult))
