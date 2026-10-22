package conversecore

import ReplyPauseEnd.*

class ReplyPauseTest extends munit.FunSuite:
  test("speech that reaches the agent cuts the reply off"):
    assertEquals(ReplyPause.forSpeech.recordingQueued("a").outcome("a", reachedAgent = true), Left(Stop))

  // The 15:05 cut-offs of 2026-10-22: near-silent sounds taken for speech,
  // whose recordings were transcribed as nothing.
  test("a recording that comes to nothing lets the reply resume"):
    assertEquals(ReplyPause.forSpeech.recordingQueued("a").outcome("a", reachedAgent = false), Left(Resume))

  test("a capture that was not speech after all lets the reply resume"):
    assertEquals(ReplyPause.forSpeech.captureEnded(tooLongToSend = false), Left(Resume))

  test("speech too long to send cuts the reply off"):
    assertEquals(ReplyPause.forSpeech.captureEnded(tooLongToSend = true), Left(Stop))

  test("the reply waits for every recording made while it was paused"):
    val two = ReplyPause.forSpeech.recordingQueued("a").speechStarted.recordingQueued("b")
    val Right(oneLeft) = two.outcome("a", reachedAgent = false): @unchecked
    assertEquals(oneLeft.outcome("b", reachedAgent = false), Left(Resume))
    assertEquals(oneLeft.outcome("b", reachedAgent = true), Left(Stop))

  test("the reply stays paused while speech that started again is still being captured"):
    val again = ReplyPause.forSpeech.recordingQueued("a").speechStarted
    val Right(stillCapturing) = again.outcome("a", reachedAgent = false): @unchecked
    assertEquals(stillCapturing.captureEnded(tooLongToSend = false), Left(Resume))

  test("outcomes of other recordings leave the pause as it is"):
    val waiting = ReplyPause.forSpeech.recordingQueued("a")
    assertEquals(waiting.outcome("z", reachedAgent = true), Right(waiting))
