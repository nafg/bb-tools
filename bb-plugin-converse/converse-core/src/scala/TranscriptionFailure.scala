package conversecore

/** Why a transcription RPC (final submit or interim preview) was rejected, as
  * far as the frontend can react differently. The RPC envelope carries only
  * the host's error category and the handler's message, so the kinds are told
  * apart by message text the backend and frontend share here.
  */
enum TranscriptionFailure:
  /** The backend's in-memory session registry no longer has the session, as after a plugin reload. */
  case SessionForgotten
  /** The transcription endpoint answered 429. */
  case RateLimited
  case Other

object TranscriptionFailure:
  val SessionForgottenMessage = "voice session is no longer active"
  val RateLimitedMessage      = "transcription endpoint rate limited"

  def classify(message: String): TranscriptionFailure =
    if message.contains(SessionForgottenMessage) then SessionForgotten
    else if message.contains(RateLimitedMessage) then RateLimited
    else Other
