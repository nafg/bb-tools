package conversecore

import SoundCue.*

class SoundCueTest extends munit.FunSuite:
  test("the session start chime plays when listening begins from startup"):
    assertEquals(SoundCue.onTransition("starting", "listening", stoppedByUser = false), Some(SessionStarted))

  test("returning to listening after other phases is silent"):
    for from <- List("recording", "transcribing", "waiting", "speaking") do
      assertEquals(SoundCue.onTransition(from, "listening", stoppedByUser = false), None, from)

  test("the user starting to talk is silent: they already know"):
    assertEquals(SoundCue.onTransition("listening", "recording", stoppedByUser = false), None)

  test("the agent starting work is marked only by the pulse taking over from the taps"):
    assertEquals(SoundCue.onTransition("transcribing", "waiting", stoppedByUser = false), None)

  test("a reply starting chimes"):
    assertEquals(SoundCue.onTransition("waiting", "speaking", stoppedByUser = false), Some(ReplyStarting))

  test("a stop the user asked for is distinct from an unexpected end"):
    assertEquals(SoundCue.onTransition("listening", "idle", stoppedByUser = true), Some(StoppedByUser))
    assertEquals(SoundCue.onTransition("listening", "idle", stoppedByUser = false), Some(EndedUnexpectedly))
    assertEquals(SoundCue.onTransition("starting", "error", stoppedByUser = false), Some(EndedUnexpectedly))

  test("ending an already idle session makes no sound"):
    assertEquals(SoundCue.onTransition("idle", "idle", stoppedByUser = true), None)
    assertEquals(SoundCue.onTransition("error", "idle", stoppedByUser = false), None)

  test("only transcribing and thinking repeat"):
    assertEquals(SoundCue.repeatingIn("transcribing"), Some(Transcribing))
    assertEquals(SoundCue.repeatingIn("waiting"), Some(Thinking))
    for phase <- List("idle", "starting", "listening", "recording", "speaking", "error") do
      assertEquals(SoundCue.repeatingIn(phase), None, phase)

  test("every cue has a sound and a stable settings key"):
    for cue <- SoundCue.values do
      assert(cue.sound.notes.nonEmpty, cue.toString)
      assert(cue.settingKey.startsWith("cue"), cue.settingKey)
    assertEquals(SoundCue.values.map(_.settingKey).distinct.length, SoundCue.values.length)

  test("repeating cues carry a default repeat and the rest do not"):
    assert(Transcribing.defaultRepeat.isDefined)
    assert(Thinking.defaultRepeat.isDefined)
    assert(SessionStarted.defaultRepeat.isEmpty)

  test("a quick transcription stays silent and thinking opens with a rest"):
    assert(Transcribing.defaultRepeat.get.leadInMs >= 1000)
    assertEquals(Thinking.defaultRepeat.get.leadInMs, Thinking.defaultRepeat.get.intervalMs)

  test("a note without an explicit release decays from the end of its attack"):
    assertEquals(ToneNote(1, 0.5, 0.1, 1).releaseStartSec, 0.1)

  test("an explicit release starts that long before the end, but never inside the attack"):
    assertEquals(ToneNote(1, 1.0, 0.1, 1, releaseSec = Some(0.3)).releaseStartSec, 0.7)
    assertEquals(ToneNote(1, 0.5, 0.3, 1, releaseSec = Some(2.0)).releaseStartSec, 0.3)

  test("a sound's duration is the sum of its notes"):
    assertEquals(ToneSound(List(ToneNote(1, 0.2, 0.01, 1), ToneNote(1, 0.3, 0.01, 1))).durationSec, 0.5)
