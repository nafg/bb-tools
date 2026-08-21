package conversecore

class SpeechDetectorSegmentStateTest extends munit.FunSuite:
  private val FrameSamples = 512
  private val SampleRate   = 16000

  private def newState() = SpeechDetectorSegmentState(
    SpeechDetectorSegmentStateConfig(
      sampleRate = SampleRate,
      frameSamples = FrameSamples,
      positiveSpeechThreshold = 0.3,
      negativeSpeechThreshold = 0.25,
      previewPauseMilliseconds = 640,
      trailingContextMilliseconds = 320
    )
  )

  test("emits one preview point per confident-silence stretch"):
    val state = newState()
    state.beginCandidate()
    state.confirmSpeech()
    assert(!state.processFrame(0.9, 0.2))

    val firstPause = List.fill(20)(state.processFrame(0.1, 0.01))
    assertEquals(firstPause.count(identity), 1)
    assert(!state.processFrame(0.1, 0.01))

    assert(!state.processFrame(0.9, 0.2))
    val secondPause = List.fill(20)(state.processFrame(0.1, 0.01))
    assertEquals(secondPause.count(identity), 1)

  test("natural endpoint removes excess trailing silence but keeps context"):
    val state = newState()
    state.beginCandidate()
    state.confirmSpeech()
    val _ = state.processFrame(0.9, 0.2)
    (1 to 93).foreach(_ => state.processFrame(0.1, 0.01))

    assertEquals(state.submittedSampleEndExclusive(60000, forcedBoundary = false), 17504)

  test("forced boundary keeps audio through the flush point"):
    val state = newState()
    state.beginCandidate()
    state.confirmSpeech()
    val _ = state.processFrame(0.9, 0.2)
    (1 to 10).foreach(_ => state.processFrame(0.1, 0.01))

    assertEquals(state.submittedSampleEndExclusive(60000, forcedBoundary = true), 60000)
