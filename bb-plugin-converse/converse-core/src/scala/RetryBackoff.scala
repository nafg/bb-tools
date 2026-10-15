package conversecore

/** How long to wait before trying again a step that must not be abandoned
  * just because it failed: storing a recording from the browser, and
  * transcribing and delivering it on the backend. Retries come quickly at
  * first, then at a ceiling so a long outage is not met with a request storm.
  */
object RetryBackoff:
  private val DelaysMs = List(1000, 3000, 8000, 15000)
  val MaxDelayMs: Int  = 30000

  /** `attemptsSoFar` counts the attempts already made, the first being 1. */
  def delayBeforeRetry(attemptsSoFar: Int): Int =
    DelaysMs.lift(attemptsSoFar - 1).getOrElse(MaxDelayMs)
