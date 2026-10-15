package conversecore

/** How long to wait before trying a failed utterance upload again. A
  * recording is the user's words, so it is retried for as long as the session
  * lasts: a slow mobile link can drop requests for minutes at a time. The
  * delay grows to a ceiling so a long outage is not met with a request storm.
  */
object UtteranceUploadRetry:
  private val DelaysMs   = List(1000, 3000, 8000, 15000)
  val MaxDelayMs: Int    = 30000

  /** `attemptsSoFar` counts the attempts already made, the first being 1. */
  def delayBeforeRetry(attemptsSoFar: Int): Int =
    DelaysMs.lift(attemptsSoFar - 1).getOrElse(MaxDelayMs)
