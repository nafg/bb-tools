package conversecore

/** How long to wait before trying a failed utterance upload again, if at all.
  * Uploads are retried because a recording is the user's words and a slow
  * mobile link drops requests; a forgotten session is not counted, since it
  * is re-registered and retried separately.
  */
object UtteranceUploadRetry:
  private val DelaysMs = List(1000, 3000, 8000)

  /** `attemptsSoFar` counts the attempts already made, the first being 1. */
  def delayBeforeRetry(attemptsSoFar: Int, failure: TranscriptionFailure): Option[Int] = failure match
    case TranscriptionFailure.SessionForgotten => None
    case TranscriptionFailure.RateLimited | TranscriptionFailure.Other =>
      DelaysMs.lift(attemptsSoFar - 1)

  val MaxAttempts: Int = DelaysMs.length + 1
