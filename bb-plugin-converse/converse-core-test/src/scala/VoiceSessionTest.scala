package conversecore

import SessionPhase.*

class VoiceSessionTest extends munit.FunSuite:
  private val listening = VoiceSession.starting[String].ready

  private def captured(session: VoiceSession[String], id: String): VoiceSession[String] =
    session.captureAt(CaptureStage.Speech).captureAt(CaptureStage.Finishing).recordingQueued(id, s"audio $id")

  private def uploading(session: VoiceSession[String]): VoiceSession[String] =
    session.startNextUpload.get._2

  test("a session is starting until its detector is ready, then listening"):
    assertEquals(VoiceSession.starting[String].phase, Starting)
    assertEquals(listening.phase, Listening)

  test("a speech candidate is not yet recording; speech is"):
    assertEquals(listening.captureAt(CaptureStage.Candidate).phase, Listening)
    assertEquals(listening.captureAt(CaptureStage.Speech).phase, Recording)

  test("a recording moves the session from recording to transcribing, then waiting once delivered"):
    val sent = uploading(captured(listening, "a"))
    assertEquals(sent.phase, Transcribing)
    val delivered = sent.stored("a").outcome("a", deliveredTo = Some("thr_1")).get
    assertEquals(delivered.phase, Waiting)
    assertEquals(delivered.replyCame("thr_1").phase, Listening)

  // The 21:51 and 23:11 failures: the outcome reached the page before the
  // upload's confirmation, or by sync after its signal was lost.
  test("an outcome before its storing is confirmed still settles the recording"):
    val outcomeFirst = uploading(captured(listening, "a")).outcome("a", deliveredTo = Some("thr_1")).get
    assertEquals(outcomeFirst.phase, Transcribing, "still uploading until confirmed")
    assertEquals(outcomeFirst.stored("a").phase, Waiting)

  test("an outcome reported twice is acted on once"):
    val once = uploading(captured(listening, "a")).stored("a").outcome("a", deliveredTo = None).get
    assertEquals(once.outcome("a", deliveredTo = None), None)

  test("a recording that came to nothing returns the session to listening"):
    val empty = uploading(captured(listening, "a")).stored("a").outcome("a", deliveredTo = None).get
    assertEquals(empty.phase, Listening)

  test("a playing reply shows as speaking over waiting and transcribing, and the user's speech over that"):
    val waiting = uploading(captured(listening, "a")).stored("a").outcome("a", Some("thr_1")).get
    assertEquals(waiting.playingNow(true).phase, Speaking)
    assertEquals(waiting.playingNow(true).captureAt(CaptureStage.Speech).phase, Recording)
    assertEquals(waiting.playingNow(true).playingNow(false).phase, Waiting)

  test("the backend's view of owed replies replaces the page's, unless the page changed it since asking"):
    val waiting = listening.replyAwaited("thr_1")
    val askedAt = waiting.awaitingVersion
    assertEquals(waiting.backendAwaiting(Set.empty, askedAt).phase, Listening, "a lost turn-ended signal")
    val changedSince = waiting.replyAwaited("thr_2")
    assertEquals(changedSince.backendAwaiting(Set.empty, askedAt).awaitingReplies, Set("thr_1", "thr_2"))

  test("a recording still being finished after the speech ended counts as transcribing and holds replies"):
    val finishing = listening.captureAt(CaptureStage.Speech).captureAt(CaptureStage.Finishing)
    assertEquals(finishing.phase, Transcribing)
    assert(finishing.holdsReplies)
    assert(!finishing.capturing)

  test("speech that begins while the previous recording is still being finished keeps its stage when that one is queued"):
    val overlapping = listening
      .captureAt(CaptureStage.Speech)
      .captureAt(CaptureStage.Finishing)
      .captureAt(CaptureStage.Speech)
      .recordingQueued("a", "audio a")
    assertEquals(overlapping.capture, CaptureStage.Speech)
    assertEquals(overlapping.phase, Recording)

  test("a recording too long to send ends the finishing stage without queuing anything"):
    val dropped = listening.captureAt(CaptureStage.Speech).captureAt(CaptureStage.Finishing).recordingDropped
    assertEquals(dropped.phase, Listening)
    assert(!dropped.outbox.busy)

  test("replies are held while the user may be speaking or a recording is not yet stored"):
    assert(listening.captureAt(CaptureStage.Candidate).holdsReplies)
    val queued = captured(listening, "a")
    assert(queued.holdsReplies)
    assert(!uploading(queued).stored("a").holdsReplies)

  test("an agent waiting for more speech shows as that activity over its turn's"):
    val working = listening.turnActivityNow(Some(AgentActivity.Tool))
    assertEquals(working.activity, Some(AgentActivity.Tool))
    assertEquals(working.awaitingSpeechNow(true).activity, Some(AgentActivity.AwaitingSpeech))
