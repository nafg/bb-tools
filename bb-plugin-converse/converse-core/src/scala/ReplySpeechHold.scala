package conversecore

/** Whether agent reply speech must wait rather than start now. A reply is held
  * while the user's utterance is being captured (including an unconfirmed
  * speech candidate) and until its transcript has been submitted.
  */
object ReplySpeechHold:
  def holds(captureActive: Boolean, phase: String): Boolean =
    captureActive || phase == "transcribing"
