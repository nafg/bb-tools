package conversecore

class VadTest extends munit.FunSuite:
  private val config = VadConfig()

  private def pushFrames(vad: EnergyVad, rms: Double, count: Int): List[VadEvent] =
    (1 to count).toList.flatMap(_ => vad.pushFrame(rms))

  private val loud   = config.threshold * 3
  private val speech = config.threshold * 1.5
  private val quiet  = config.threshold / 2

  test("silence produces no events"):
    val vad = EnergyVad(config)
    assertEquals(pushFrames(vad, quiet, 100), Nil)

  test("candidate starts on first speech frame, speech confirms after speechStartFrames"):
    val vad = EnergyVad(config)
    assertEquals(vad.pushFrame(speech), List(VadEvent.CandidateStart))
    assertEquals(vad.pushFrame(speech), Nil)
    assertEquals(vad.pushFrame(speech), List(VadEvent.SpeechStart))
    assert(vad.isInSpeech)

  test("unconfirmed candidate is abandoned after the window"):
    val vad = EnergyVad(config)
    assertEquals(vad.pushFrame(speech), List(VadEvent.CandidateStart))
    val events = pushFrames(vad, quiet, config.candidateWindowFrames)
    assertEquals(events, List(VadEvent.CandidateAbandoned))

  test("utterance ends after sustained silence, with a partial at the shorter pause"):
    val vad = EnergyVad(config)
    val _   = pushFrames(vad, speech, config.minUtteranceFrames + 3)
    val events = pushFrames(vad, quiet, config.utteranceEndFrames)
    assertEquals(events, List(VadEvent.Partial, VadEvent.SpeechEnd(longEnough = true)))
    assert(!vad.isInSpeech)

  test("short burst ends not long enough"):
    val vad = EnergyVad(config)
    val _   = pushFrames(vad, speech, config.speechStartFrames)
    val events = pushFrames(vad, quiet, config.utteranceEndFrames)
    assertEquals(events, List(VadEvent.SpeechEnd(longEnough = false)))

  test("mid-utterance silence shorter than the endpoint emits only a partial"):
    val vad = EnergyVad(config)
    val _   = pushFrames(vad, speech, config.minUtteranceFrames)
    assertEquals(pushFrames(vad, quiet, config.utteranceEndFrames - 1), List(VadEvent.Partial))
    assertEquals(pushFrames(vad, speech, 1), Nil)
    assert(vad.isInSpeech)

  test("partial fires once per pause and again after speech resumes"):
    val vad = EnergyVad(config)
    val _   = pushFrames(vad, speech, config.minUtteranceFrames)
    assertEquals(pushFrames(vad, quiet, config.chunkSilenceFrames), List(VadEvent.Partial))
    assertEquals(pushFrames(vad, quiet, 5), Nil)
    val _ = pushFrames(vad, speech, 3)
    assertEquals(pushFrames(vad, quiet, config.chunkSilenceFrames), List(VadEvent.Partial))

  test("short bursts produce no partial"):
    val vad = EnergyVad(config)
    val _   = pushFrames(vad, speech, config.speechStartFrames)
    assertEquals(pushFrames(vad, quiet, config.chunkSilenceFrames), Nil)

  test("barge-in fires once after sustained loud frames"):
    val vad    = EnergyVad(config)
    val events = pushFrames(vad, loud, config.bargeInFrames + 10)
    assertEquals(events.count(_ == VadEvent.BargeIn), 1)

  test("barge-in can fire again after a quiet gap"):
    val vad = EnergyVad(config)
    val _   = pushFrames(vad, loud, config.bargeInFrames)
    val _   = pushFrames(vad, quiet, config.utteranceEndFrames + 5)
    val events = pushFrames(vad, loud, config.bargeInFrames)
    assertEquals(events.count(_ == VadEvent.BargeIn), 1)

  test("a new utterance can start after one ends"):
    val vad = EnergyVad(config)
    val _   = pushFrames(vad, speech, config.minUtteranceFrames)
    val _   = pushFrames(vad, quiet, config.utteranceEndFrames)
    val events = pushFrames(vad, speech, config.speechStartFrames)
    assertEquals(events, List(VadEvent.CandidateStart, VadEvent.SpeechStart))
