package conversecore

/** Whether agent reply speech must wait rather than start now. A reply is held
  * while the user's utterance is being captured (including an unconfirmed
  * speech candidate) and until its recording has been stored, so whether it
  * was spoken before the reply was heard is settled before the reply plays.
  * Transcribing and delivering a stored recording does not hold replies.
  */
object ReplySpeechHold:
  def holds(captureActive: Boolean, recordingUnstored: Boolean): Boolean =
    captureActive || recordingUnstored
