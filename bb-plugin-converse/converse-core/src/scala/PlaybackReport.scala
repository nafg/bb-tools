package conversecore

/** A playback span as the browser reports it. Accepted only when it names a
  * thread and ends no earlier than it starts.
  */
object PlaybackReport:
  def validate(threadId: String, startAt: Double, endAt: Double): Either[String, Unit] =
    if threadId.isEmpty then Left("threadId is required")
    else if startAt.isNaN || endAt.isNaN || startAt.isInfinite || endAt.isInfinite then Left("startAt and endAt must be times")
    else if endAt < startAt then Left("endAt is before startAt")
    else Right(())
