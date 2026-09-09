package conversecore

import SoundCueSettings.Unchecked

class SoundCueSettingsTest extends munit.FunSuite:
  private val note = Unchecked.Note(
    frequencyHz = Some(440),
    durationSec = Some(0.2),
    attackSec = Some(0.01),
    gain = Some(0.3),
    releaseSec = None,
    harmonic2 = None,
    pinkNoise = None,
    lowpassHz = None
  )
  private val oneShot   = Unchecked.Cue(enabled = true, notes = List(note), repeat = None)
  private val repeating = Unchecked.Cue(enabled = true, notes = List(note), repeat = Some(Unchecked.Repeat(Some(0), Some(500))))

  test("the defaults are the hard-coded cues, all enabled, at half volume"):
    assertEquals(SoundCueSettings.default.volume, 0.5)
    for cue <- SoundCue.values do
      assertEquals(SoundCueSettings.default.cues(cue), CueSetting(true, cue.sound, cue.defaultRepeat), cue.toString)

  test("the defaults survive a round trip through the unchecked form"):
    assertEquals(SoundCueSettings.fromUnchecked(SoundCueSettings.default.unchecked), Right(SoundCueSettings.default))

  test("edited values survive a round trip"):
    val edited = SoundCueSettings.default.copy(
      volume = 0.8,
      cues = SoundCueSettings.default.cues
        .updated(SoundCue.Thinking, CueSetting(false, ToneSound(List(ToneNote(90, 1.5, 0.4, 0.1, Some(0.5), 0.2, 0.4, Some(900)))), Some(Repeat(0, 1200))))
        .updated(SoundCue.ReplyStarting, CueSetting(true, ToneSound(List(ToneNote(600, 0.1, 0.01, 0.4), ToneNote(900, 0.2, 0.02, 0.4, Some(0.1)))), None))
    )
    assertEquals(SoundCueSettings.fromUnchecked(edited.unchecked), Right(edited))

  test("cues left out of the unchecked form keep their defaults"):
    val result = SoundCueSettings.fromUnchecked(Unchecked(None, Map(SoundCue.ReplyStarting.settingKey -> oneShot.copy(enabled = false))))
    assertEquals(result.map(_.volume), Right(0.5))
    assertEquals(result.map(_.cues(SoundCue.ReplyStarting).enabled), Right(false))
    assertEquals(result.map(_.cues(SoundCue.Thinking)), Right(SoundCueSettings.default.cues(SoundCue.Thinking)))

  test("an unknown cue key is rejected"):
    assertEquals(SoundCueSettings.fromUnchecked(Unchecked(None, Map("cueBogus" -> oneShot))), Left("unknown cue cueBogus"))

  test("volume outside 0-1 is rejected"):
    assertEquals(SoundCueSettings.fromUnchecked(Unchecked(Some(1.5), Map.empty)), Left("volume must be between 0.0 and 1.0"))

  test("isDefault compares by value"):
    assert(SoundCueSettings.default.isDefault(SoundCue.Thinking))
    val quieter = SoundCueSettings.default.cues(SoundCue.Thinking).copy(enabled = false)
    assert(!SoundCueSettings.default.copy(cues = SoundCueSettings.default.cues.updated(SoundCue.Thinking, quieter)).isDefault(SoundCue.Thinking))

  test("harmonic and noise levels default to zero and unset release and low-pass stay unset"):
    assertEquals(SoundCueSettings.checkNote(note), Right(ToneNote(440, 0.2, 0.01, 0.3)))

  test("a missing required field names itself"):
    assertEquals(SoundCueSettings.checkNote(note.copy(gain = None)), Left("gain is missing"))

  test("pitch must be within the audible design range"):
    assertEquals(SoundCueSettings.checkNote(note.copy(frequencyHz = Some(59))), Left("frequencyHz must be between 60.0 and 2000.0"))
    assertEquals(SoundCueSettings.checkNote(note.copy(frequencyHz = Some(2001))).isLeft, true)
    assertEquals(SoundCueSettings.checkNote(note.copy(frequencyHz = Some(Double.NaN))).isLeft, true)

  test("levels are 0-1 and durations positive"):
    assert(SoundCueSettings.checkNote(note.copy(gain = Some(1.01))).isLeft)
    assert(SoundCueSettings.checkNote(note.copy(harmonic2 = Some(-0.1))).isLeft)
    assert(SoundCueSettings.checkNote(note.copy(pinkNoise = Some(2))).isLeft)
    assertEquals(SoundCueSettings.checkNote(note.copy(durationSec = Some(0))), Left("durationSec must be positive and at most 5.0"))
    assert(SoundCueSettings.checkNote(note.copy(releaseSec = Some(0))).isLeft)
    assert(SoundCueSettings.checkNote(note.copy(releaseSec = Some(0.1))).isRight)

  test("the attack cannot outlast the note"):
    assertEquals(SoundCueSettings.checkNote(note.copy(attackSec = Some(0.3))), Left("attackSec must be between 0.0 and 0.2"))

  test("a low-pass cutoff must be within the filter's range"):
    assert(SoundCueSettings.checkNote(note.copy(lowpassHz = Some(50))).isLeft)
    assertEquals(SoundCueSettings.checkNote(note.copy(lowpassHz = Some(1200))).map(_.lowpassHz), Right(Some(1200.0)))

  test("a repeat needs a non-negative lead-in and an interval of at least 100 ms, rounded to whole ms"):
    assertEquals(SoundCueSettings.checkRepeat(Unchecked.Repeat(Some(10.4), Some(333.6))), Right(Repeat(10, 334)))
    assertEquals(SoundCueSettings.checkRepeat(Unchecked.Repeat(Some(-1), Some(500))), Left("leadInMs must be between 0.0 and 60000.0"))
    assertEquals(SoundCueSettings.checkRepeat(Unchecked.Repeat(Some(0), Some(99))), Left("intervalMs must be between 100.0 and 60000.0"))
    assertEquals(SoundCueSettings.checkRepeat(Unchecked.Repeat(None, Some(500))), Left("leadInMs is missing"))

  test("a one-shot cue takes one to three notes and no repeat"):
    assert(SoundCueSettings.checkCue(SoundCue.SessionStarted, oneShot.copy(notes = List(note, note, note))).isRight)
    assertEquals(SoundCueSettings.checkCue(SoundCue.SessionStarted, oneShot.copy(notes = Nil)), Left("Session started: needs at least one note"))
    assertEquals(SoundCueSettings.checkCue(SoundCue.SessionStarted, oneShot.copy(notes = List(note, note, note, note))), Left("Session started: at most 3 notes"))
    assertEquals(SoundCueSettings.checkCue(SoundCue.SessionStarted, repeating), Left("Session started: only a repeating cue has a repeat"))

  test("a repeating cue takes exactly one note and a repeat"):
    assertEquals(SoundCueSettings.checkCue(SoundCue.Thinking, repeating).map(_.repeat), Right(Some(Repeat(0, 500))))
    assertEquals(SoundCueSettings.checkCue(SoundCue.Thinking, oneShot), Left("Thinking (repeating pulse): a repeating cue needs a repeat"))
    assertEquals(SoundCueSettings.checkCue(SoundCue.Thinking, repeating.copy(notes = List(note, note))), Left("Thinking (repeating pulse): a repeating cue has exactly one note"))

  test("a bad note is reported with its position and the cue's label"):
    assertEquals(
      SoundCueSettings.checkCue(SoundCue.SessionStarted, oneShot.copy(notes = List(note, note.copy(gain = Some(5))))),
      Left("Session started: note 2: gain must be between 0.0 and 1.0")
    )
