package conversecore

import SoundCue.*
import SessionPhase as P
import VoiceSessionRestore.SessionEnd
import VoiceState.*

class SoundCueTest extends munit.FunSuite:
  private def endedBy(end: SessionEnd) = Off(Some(Ending(end, None)))

  test("the session start chime plays when listening begins from startup"):
    assertEquals(SoundCue.onTransition(On(P.Starting), On(P.Listening)), Some(SessionStarted))

  test("returning to listening after other phases is silent"):
    for from <- List(P.Recording, P.Transcribing, P.Waiting, P.Speaking) do
      assertEquals(SoundCue.onTransition(On(from), On(P.Listening)), None, from.toString)

  test("the user starting to talk is silent: they already know"):
    assertEquals(SoundCue.onTransition(On(P.Listening), On(P.Recording)), None)

  test("the agent starting work is marked only by the pulse taking over from the taps"):
    assertEquals(SoundCue.onTransition(On(P.Transcribing), On(P.Waiting)), None)

  test("a reply starting chimes"):
    assertEquals(SoundCue.onTransition(On(P.Waiting), On(P.Speaking)), Some(ReplyStarting))

  test("a stop the user asked for is distinct from every other ending"):
    assertEquals(SoundCue.onTransition(On(P.Listening), endedBy(SessionEnd.StoppedByUser)), Some(StoppedByUser))
    for end <- SessionEnd.values.filterNot(_ == SessionEnd.StoppedByUser) do
      assertEquals(SoundCue.onTransition(On(P.Listening), endedBy(end)), Some(EndedUnexpectedly), end.toString)

  test("a session that fails while starting is an unexpected end"):
    assertEquals(SoundCue.onTransition(On(P.Starting), endedBy(SessionEnd.Failed)), Some(EndedUnexpectedly))

  test("staying off makes no sound"):
    assertEquals(SoundCue.onTransition(Off(None), endedBy(SessionEnd.StoppedByUser)), None)
    assertEquals(SoundCue.onTransition(endedBy(SessionEnd.Failed), Off(None)), None)

  test("only transcribing and waiting repeat, whatever the agent is doing"):
    for activity <- None :: AgentActivity.values.toList.map(Some(_)) do
      assertEquals(SoundCue.repeatingIn(On(P.Transcribing), activity), Some(Transcribing), activity.toString)
      for phase <- List(P.Starting, P.Listening, P.Recording, P.Speaking) do
        assertEquals(SoundCue.repeatingIn(On(phase), activity), None, s"$phase $activity")
      assertEquals(SoundCue.repeatingIn(Off(None), activity), None, activity.toString)

  test("while waiting the pulse follows the agent's activity, defaulting to thinking"):
    assertEquals(SoundCue.repeatingIn(On(P.Waiting), None), Some(Thinking))
    assertEquals(SoundCue.repeatingIn(On(P.Waiting), Some(AgentActivity.Reasoning)), Some(Thinking))
    assertEquals(SoundCue.repeatingIn(On(P.Waiting), Some(AgentActivity.Tool)), Some(ToolRunning))
    assertEquals(SoundCue.repeatingIn(On(P.Waiting), Some(AgentActivity.Generating)), Some(Generating))

  test("listening for more speech silences the pulse"):
    assertEquals(SoundCue.repeatingIn(On(P.Waiting), Some(AgentActivity.AwaitingSpeech)), None)

  test("the activity pulses are the thinking texture at double rate or brighter"):
    val thinking = Thinking.sound.notes.head
    assertEquals(ToolRunning.sound.notes.head, thinking)
    assertEquals(ToolRunning.defaultRepeat, Some(Repeat(1428, 1428)))
    assertEquals(Generating.sound.notes.head, thinking.copy(harmonic2 = 0.3))
    assertEquals(Generating.defaultRepeat, Thinking.defaultRepeat)

  test("the awaiting-speech rise is two short notes an octave apart in the thinking timbre"):
    val List(low, high) = AwaitingSpeech.sound.notes
    assertEquals(low.frequencyHz, 129.0)
    assertEquals(high.frequencyHz, 258.0)
    for note <- List(low, high) do
      assertEquals((note.harmonic2, note.pinkNoise, note.lowpassHz), (0.1, 0.37, Some(2000.0)))
    assert(AwaitingSpeech.sound.durationSec < 1.0)
    assertEquals(AwaitingSpeech.defaultRepeat, None)

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
