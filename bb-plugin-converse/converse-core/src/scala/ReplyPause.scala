package conversecore

/** How a paused reply ends: it carries on where it stopped, or is cut off. */
enum ReplyPauseEnd:
  case Resume, Stop

/** A reply paused because the user seemed to start speaking during it. It
  * waits for what that speech turns out to be: a recording that reaches the
  * agent cuts the reply off, while one that comes to nothing (no words, or a
  * sound that was not speech) lets it resume. Speech that starts again while
  * it waits joins the same pause.
  */
final case class ReplyPause private (
    /** Speech is still being captured, its recording not yet queued. */
    capturing: Boolean,
    /** Recordings of this speech whose outcome is not yet known. */
    recordings: Set[String]
):
  def speechStarted: ReplyPause = copy(capturing = true)

  /** The speech being captured was recorded as `id`. */
  def recordingQueued(id: String): ReplyPause =
    if capturing then ReplyPause(capturing = false, recordings + id) else this

  /** The capture ended without a recording to send: it was not speech after
    * all, or it was too long to send, in which case it was certainly speech.
    */
  def captureEnded(tooLongToSend: Boolean): Either[ReplyPauseEnd, ReplyPause] =
    if !capturing then Right(this)
    else if tooLongToSend then Left(ReplyPauseEnd.Stop)
    else settleIfDone(copy(capturing = false))

  /** A recording's outcome: `reachedAgent` when it was delivered, or would
    * have been but for a failure.
    */
  def outcome(id: String, reachedAgent: Boolean): Either[ReplyPauseEnd, ReplyPause] =
    if !recordings(id) then Right(this)
    else if reachedAgent then Left(ReplyPauseEnd.Stop)
    else settleIfDone(copy(recordings = recordings - id))

  private def settleIfDone(next: ReplyPause): Either[ReplyPauseEnd, ReplyPause] =
    if !next.capturing && next.recordings.isEmpty then Left(ReplyPauseEnd.Resume) else Right(next)

object ReplyPause:
  /** Speech has just started during a reply. */
  val forSpeech: ReplyPause = ReplyPause(capturing = true, recordings = Set.empty)
