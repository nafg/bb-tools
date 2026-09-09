package conversecore

/** One cue as the session plays it: whether it sounds at all, the sound, and
  * for a repeating cue its timing.
  */
final case class CueSetting(enabled: Boolean, sound: ToneSound, repeat: Option[Repeat])

/** Every cue's setting and the master volume, all range-checked. */
final case class SoundCueSettings(volume: Double, cues: Map[SoundCue, CueSetting]):
  def isDefault(cue: SoundCue): Boolean = cues(cue) == SoundCueSettings.default.cues(cue)

  def unchecked: SoundCueSettings.Unchecked =
    SoundCueSettings.Unchecked(
      Some(volume),
      cues.map((cue, setting) => cue.settingKey -> SoundCueSettings.Unchecked.of(setting))
    )

object SoundCueSettings:
  val default: SoundCueSettings =
    SoundCueSettings(0.5, SoundCue.values.map(cue => cue -> CueSetting(true, cue.sound, cue.defaultRepeat)).toMap)

  val MinFrequencyHz = 60.0
  val MaxFrequencyHz = 2000.0
  val MaxDurationSec = 5.0
  val MinLowpassHz   = 100.0
  val MaxLowpassHz   = 8000.0
  val MinIntervalMs  = 100.0
  val MaxRepeatMs    = 60_000.0
  val MaxNotes       = 3

  /** The settings as they are stored and as the designer edits them: every
    * number optional and none of them range-checked, so a stored value or a
    * slider position is representable before `fromUnchecked` decides.
    */
  final case class Unchecked(volume: Option[Double], cues: Map[String, Unchecked.Cue])

  object Unchecked:
    final case class Note(
        frequencyHz: Option[Double],
        durationSec: Option[Double],
        attackSec: Option[Double],
        gain: Option[Double],
        releaseSec: Option[Double],
        harmonic2: Option[Double],
        pinkNoise: Option[Double],
        lowpassHz: Option[Double]
    )
    final case class Repeat(leadInMs: Option[Double], intervalMs: Option[Double])
    final case class Cue(enabled: Boolean, notes: List[Note], repeat: Option[Repeat])

    def of(note: ToneNote): Note =
      Note(
        Some(note.frequencyHz),
        Some(note.durationSec),
        Some(note.attackSec),
        Some(note.gain),
        note.releaseSec,
        Some(note.harmonic2),
        Some(note.pinkNoise),
        note.lowpassHz
      )

    def of(repeat: conversecore.Repeat): Repeat =
      Repeat(Some(repeat.leadInMs.toDouble), Some(repeat.intervalMs.toDouble))

    def of(setting: CueSetting): Cue =
      Cue(setting.enabled, setting.sound.notes.map(of), setting.repeat.map(of))

  /** Cues absent from `unchecked` keep their defaults; unknown keys are rejected. */
  def fromUnchecked(unchecked: Unchecked): Either[String, SoundCueSettings] =
    for
      volume <- unchecked.volume.fold(Right(default.volume))(between("volume", _, 0, 1))
      _ <- unchecked.cues.keys
        .find(key => SoundCue.values.forall(_.settingKey != key))
        .toLeft(())
        .left
        .map(key => s"unknown cue $key")
      cues <- traverse(SoundCue.values.toList) { cue =>
        unchecked.cues.get(cue.settingKey).fold(Right(default.cues(cue)))(checkCue(cue, _)).map(cue -> _)
      }
    yield SoundCueSettings(volume, cues.toMap)

  def checkCue(cue: SoundCue, unchecked: Unchecked.Cue): Either[String, CueSetting] =
    val repeating = cue.defaultRepeat.isDefined
    val result =
      for
        _ <- Either.cond(unchecked.notes.nonEmpty, (), "needs at least one note")
        _ <- Either.cond(unchecked.notes.length <= MaxNotes, (), s"at most $MaxNotes notes")
        _ <- Either.cond(!repeating || unchecked.notes.length == 1, (), "a repeating cue has exactly one note")
        notes <- traverse(unchecked.notes.zipWithIndex) { (note, i) =>
          checkNote(note).left.map(message => s"note ${i + 1}: $message")
        }
        repeat <- (repeating, unchecked.repeat) match
          case (true, Some(r))  => checkRepeat(r).map(Some(_))
          case (true, None)     => Left("a repeating cue needs a repeat")
          case (false, None)    => Right(None)
          case (false, Some(_)) => Left("only a repeating cue has a repeat")
      yield CueSetting(unchecked.enabled, ToneSound(notes), repeat)
    result.left.map(message => s"${cue.label}: $message")

  def checkNote(unchecked: Unchecked.Note): Either[String, ToneNote] =
    for
      frequencyHz <- required("frequencyHz", unchecked.frequencyHz)(between(_, _, MinFrequencyHz, MaxFrequencyHz))
      durationSec <- required("durationSec", unchecked.durationSec)(positiveUpTo(_, _, MaxDurationSec))
      attackSec   <- required("attackSec", unchecked.attackSec)(between(_, _, 0, durationSec))
      gain        <- required("gain", unchecked.gain)(between(_, _, 0, 1))
      releaseSec  <- optional("releaseSec", unchecked.releaseSec)(positiveUpTo(_, _, MaxDurationSec))
      harmonic2   <- unchecked.harmonic2.fold(Right(0.0))(between("harmonic2", _, 0, 1))
      pinkNoise   <- unchecked.pinkNoise.fold(Right(0.0))(between("pinkNoise", _, 0, 1))
      lowpassHz   <- optional("lowpassHz", unchecked.lowpassHz)(between(_, _, MinLowpassHz, MaxLowpassHz))
    yield ToneNote(frequencyHz, durationSec, attackSec, gain, releaseSec, harmonic2, pinkNoise, lowpassHz)

  def checkRepeat(unchecked: Unchecked.Repeat): Either[String, Repeat] =
    for
      leadInMs   <- required("leadInMs", unchecked.leadInMs)(between(_, _, 0, MaxRepeatMs))
      intervalMs <- required("intervalMs", unchecked.intervalMs)(between(_, _, MinIntervalMs, MaxRepeatMs))
    yield Repeat(leadInMs.round.toInt, intervalMs.round.toInt)

  private type Check = (String, Double) => Either[String, Double]

  private def required(name: String, value: Option[Double])(check: Check): Either[String, Double] =
    value.toRight(s"$name is missing").flatMap(check(name, _))

  private def optional(name: String, value: Option[Double])(check: Check): Either[String, Option[Double]] =
    value.fold(Right(None))(check(name, _).map(Some(_)))

  private def between(name: String, value: Double, min: Double, max: Double): Either[String, Double] =
    Either.cond(min <= value && value <= max, value, s"$name must be between $min and $max")

  private def positiveUpTo(name: String, value: Double, max: Double): Either[String, Double] =
    Either.cond(0 < value && value <= max, value, s"$name must be positive and at most $max")

  private def traverse[A, B](as: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    as.foldRight(Right(Nil): Either[String, List[B]])((a, acc) => f(a).flatMap(b => acc.map(b :: _)))
