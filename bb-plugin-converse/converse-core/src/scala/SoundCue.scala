package conversecore

/** One synthesized note: a sine at `frequencyHz`, optionally with its second
  * harmonic and pink noise mixed in at the given relative levels, shaped by a
  * linear attack, a hold, and a linear release that ends at `durationSec`.
  * `lowpassHz` filters the noise only, deciding how bright the air sounds.
  * `releaseSec` of None means the note decays from the end of the attack
  * straight to silence.
  */
final case class ToneNote(
    frequencyHz: Double,
    durationSec: Double,
    attackSec: Double,
    gain: Double,
    releaseSec: Option[Double] = None,
    harmonic2: Double = 0,
    pinkNoise: Double = 0,
    lowpassHz: Option[Double] = None
):
  def releaseStartSec: Double = releaseSec.fold(attackSec)(r => math.max(attackSec, durationSec - r))

/** Notes played back to back. */
final case class ToneSound(notes: List[ToneNote]):
  def durationSec: Double = notes.map(_.durationSec).sum

/** The audible cues of a voice session: one-shot chimes at phase transitions
  * and repeating pulses while the session is working.
  */
/** How a cue repeats: silence for `leadInMs` first, so a phase that ends
  * quickly makes no sound, then the sound every `intervalMs`.
  */
final case class Repeat(leadInMs: Int, intervalMs: Int)

enum SoundCue(val settingKey: String, val label: String, val sound: ToneSound, val defaultRepeat: Option[Repeat]):
  case SessionStarted extends SoundCue(
    "cueSessionStarted",
    "Session started",
    ToneSound(List(ToneNote(523, 0.18, 0.01, 0.35), ToneNote(784, 0.25, 0.01, 0.35))),
    None
  )
  case ReplyStarting extends SoundCue(
    "cueReplyStarting",
    "Reply starting",
    ToneSound(List(ToneNote(659, 0.35, 0.01, 0.35))),
    None
  )
  // A pause between spoken chunks is indistinguishable from the end without this.
  case ReplyFinished extends SoundCue(
    "cueReplyFinished",
    "Reply finished",
    ToneSound(List(ToneNote(523, 0.25, 0.01, 0.3))),
    None
  )
  case StoppedByUser extends SoundCue(
    "cueStoppedByUser",
    "Stopped by you",
    ToneSound(List(ToneNote(784, 0.25, 0.01, 0.35), ToneNote(523, 0.18, 0.01, 0.35))),
    None
  )
  case EndedUnexpectedly extends SoundCue(
    "cueEndedUnexpectedly",
    "Ended unexpectedly",
    ToneSound(List(ToneNote(659, 0.35, 0.01, 0.35), ToneNote(330, 0.4, 0.01, 0.35), ToneNote(262, 0.5, 0.01, 0.35))),
    None
  )
  case Transcribing extends SoundCue(
    "cueTranscribing",
    "Transcribing (repeating tap)",
    ToneSound(List(ToneNote(440, 0.08, 0.004, 0.3))),
    Some(Repeat(leadInMs = 1000, intervalMs = 330))
  )
  // Tuned by Naftoli in the tone lab (2026-09-08): low, breathy, and slow.
  case Thinking extends SoundCue(
    "cueThinking",
    "Thinking (repeating pulse)",
    ToneSound(List(ToneNote(129, 0.886, 0.345, 0.02, harmonic2 = 0.1, pinkNoise = 0.37, lowpassHz = Some(2000)))),
    Some(Repeat(leadInMs = 2857, intervalMs = 2857))
  )
  // Variations of Thinking: the same texture at double rate, and brighter.
  case ToolRunning extends SoundCue(
    "cueToolRunning",
    "Running a tool (repeating pulse)",
    ToneSound(List(ToneNote(129, 0.886, 0.345, 0.02, harmonic2 = 0.1, pinkNoise = 0.37, lowpassHz = Some(2000)))),
    Some(Repeat(leadInMs = 1428, intervalMs = 1428))
  )
  case Generating extends SoundCue(
    "cueGenerating",
    "Writing the reply (repeating pulse)",
    ToneSound(List(ToneNote(129, 0.886, 0.345, 0.02, harmonic2 = 0.3, pinkNoise = 0.37, lowpassHz = Some(2000)))),
    Some(Repeat(leadInMs = 2857, intervalMs = 2857))
  )
  case AwaitingSpeech extends SoundCue(
    "cueAwaitingSpeech",
    "Listening for more speech",
    ToneSound(
      List(
        ToneNote(129, 0.3, 0.1, 0.02, harmonic2 = 0.1, pinkNoise = 0.37, lowpassHz = Some(2000)),
        ToneNote(258, 0.4, 0.1, 0.02, harmonic2 = 0.1, pinkNoise = 0.37, lowpassHz = Some(2000))
      )
    ),
    None
  )

object SoundCue:
  /** The chime for a phase change, if any. `stoppedByUser` distinguishes a
    * deliberate stop from a session ending on its own.
    */
  def onTransition(from: VoiceState, to: VoiceState): Option[SoundCue] =
    import VoiceState.*
    (from, to) match
      case (On(SessionPhase.Starting), On(SessionPhase.Listening)) => Some(SessionStarted)
      case (On(_), On(SessionPhase.Speaking))                      => Some(ReplyStarting)
      case (On(_), Off(ending)) =>
        Some(
          if ending.exists(_.end == VoiceSessionRestore.SessionEnd.StoppedByUser) then StoppedByUser
          else EndedUnexpectedly
        )
      case _ => None

  /** The pulse that repeats for as long as the session is in `phase` with the
    * agent in `activity`; none while it is listening for more speech.
    */
  def repeatingIn(state: VoiceState, activity: Option[AgentActivity]): Option[SoundCue] =
    import VoiceState.On
    (state, activity) match
      case (On(SessionPhase.Transcribing), _)                               => Some(Transcribing)
      case (On(SessionPhase.Waiting), Some(AgentActivity.Tool))             => Some(ToolRunning)
      case (On(SessionPhase.Waiting), Some(AgentActivity.Generating))       => Some(Generating)
      case (On(SessionPhase.Waiting), Some(AgentActivity.AwaitingSpeech))   => None
      case (On(SessionPhase.Waiting), Some(AgentActivity.Reasoning) | None) => Some(Thinking)
      case _                                                                => None
