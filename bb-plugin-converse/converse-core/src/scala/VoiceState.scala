package conversecore

import VoiceSessionRestore.SessionEnd

/** What a running voice session is doing. */
enum SessionPhase(val name: String):
  case Starting     extends SessionPhase("starting")
  case Listening    extends SessionPhase("listening")
  case Recording    extends SessionPhase("recording")
  case Transcribing extends SessionPhase("transcribing")
  /** The utterance was sent; the agent is working on it. */
  case Waiting      extends SessionPhase("waiting")
  case Speaking     extends SessionPhase("speaking")

/** The frontend's voice session: either running, or not, remembering how the
  * last one ended so the user can be told. A session that fails to start is
  * simply off with a Failed ending: failure is not a state to be in.
  */
enum VoiceState:
  case Off(lastEnding: Option[VoiceState.Ending])
  case On(phase: SessionPhase)

  def isOn: Boolean = this match
    case On(_)  => true
    case Off(_) => false

  def runningPhase: Option[SessionPhase] = this match
    case On(p)  => Some(p)
    case Off(_) => None

object VoiceState:
  val initial: VoiceState = Off(None)

  /** How a session ended; `detail` is the failure message when there is one. */
  final case class Ending(end: SessionEnd, detail: Option[String]):
    /** What to tell the user, if anything. */
    def message: Option[String] = end match
      case SessionEnd.Failed                   => detail.orElse(Some("voice failed"))
      case SessionEnd.TakenOverByAnotherWindow => Some("voice was taken over by another window")
      case SessionEnd.PausedUntilUserGesture =>
        Some("the browser paused voice until you click: click the microphone to resume")
      case SessionEnd.StoppedByUser | SessionEnd.Superseded | SessionEnd.FrontendUnloaded => None
