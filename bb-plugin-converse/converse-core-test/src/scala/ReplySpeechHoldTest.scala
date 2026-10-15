package conversecore

class ReplySpeechHoldTest extends munit.FunSuite:
  test("a reply arriving while the user is speaking, or before the recording is stored, is held"):
    assert(ReplySpeechHold.holds(captureActive = true, recordingUnstored = false))
    assert(ReplySpeechHold.holds(captureActive = false, recordingUnstored = true))

  test("a reply plays once the recording is stored, even while it is still being transcribed"):
    assert(!ReplySpeechHold.holds(captureActive = false, recordingUnstored = false))
