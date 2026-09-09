package conversecore

class ReplySpeechHoldTest extends munit.FunSuite:
  test("a reply arriving while the user is speaking is held"):
    assert(ReplySpeechHold.holds(captureActive = true, phase = "recording"))

  test("a reply is held while a speech candidate is being captured before it is confirmed"):
    assert(ReplySpeechHold.holds(captureActive = true, phase = "listening"))

  test("a reply stays held while the finished utterance is being transcribed and submitted"):
    assert(ReplySpeechHold.holds(captureActive = false, phase = "transcribing"))

  test("a reply is released once the utterance is submitted and the session waits for the agent"):
    assert(!ReplySpeechHold.holds(captureActive = false, phase = "waiting"))

  test("a reply is released when the capture is abandoned"):
    assert(!ReplySpeechHold.holds(captureActive = false, phase = "listening"))

  test("a reply plays immediately when nothing is being captured"):
    assert(!ReplySpeechHold.holds(captureActive = false, phase = "speaking"))
