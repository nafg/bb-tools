package conversecore

/** How long interim previews pause after one fails. Only a provider rate
  * limit pauses them, longer on each consecutive one, so final submissions
  * keep the provider's quota; any other failure leaves the next preview point
  * to try again.
  */
object PreviewBackoff:
  val InitialPauseMs = 10_000.0
  val MaxPauseMs     = 60_000.0

  /** `previousPauseMs` is the pause applied to the preceding rate limit, None
    * once a preview has succeeded since.
    */
  def pauseAfter(failure: TranscriptionFailure, previousPauseMs: Option[Double]): Option[Double] =
    failure match
      case TranscriptionFailure.RateLimited =>
        Some(previousPauseMs.fold(InitialPauseMs)(previous => (previous * 2).min(MaxPauseMs)))
      case TranscriptionFailure.SessionForgotten | TranscriptionFailure.Other => None
