package conversecore

/** How far the speech detector has got with what it is hearing. */
enum CaptureStage:
  case Idle
  /** Possibly speech, not yet long enough to count. */
  case Candidate
  /** Speech: the user is talking. */
  case Speech
  /** The speech ended and its recording is being finished before it is
    * queued for upload.
    */
  case Finishing

/** A running voice session as facts, from which what it is doing is derived
  * rather than stored, so the phase can never disagree with the facts. Every
  * transition holds in whatever order its events arrive, and applying a
  * report of backend state twice changes nothing the second time.
  *
  * `P` is what the page keeps for each recording until it is uploaded.
  */
final case class VoiceSession[P] private (
    /** The microphone and speech detector are not ready yet. */
    starting: Boolean,
    capture: CaptureStage,
    outbox: Outbox[P],
    /** Recordings whose outcome has been acted on, so a second report of one
      * (by signal and by sync) is ignored.
      */
    outcomesActedOn: Set[String],
    /** Threads that were sent a message and owe this session a reply. */
    awaitingReplies: Set[String],
    /** Changed with each local change to `awaitingReplies`, so a report of
      * the backend's view taken before that change is not applied over it.
      */
    awaitingVersion: Int,
    /** A reply's audio is playing. */
    playing: Boolean,
    turnActivity: Option[AgentActivity],
    /** An agent is waiting for the user to say more. */
    awaitingSpeech: Boolean
):
  /** What the session is doing, by precedence: getting ready, hearing the
    * user, playing a reply, finishing recordings, waiting on agents, listening.
    */
  def phase: SessionPhase =
    if starting then SessionPhase.Starting
    else if capture == CaptureStage.Speech then SessionPhase.Recording
    else if playing then SessionPhase.Speaking
    else if capture == CaptureStage.Finishing || outbox.busy then SessionPhase.Transcribing
    else if awaitingReplies.nonEmpty then SessionPhase.Waiting
    else SessionPhase.Listening

  def activity: Option[AgentActivity] =
    if awaitingSpeech then Some(AgentActivity.AwaitingSpeech) else turnActivity

  /** Replies wait while the user may be speaking or a recording is not yet
    * stored, so whether one was spoken before a reply is settled first.
    */
  def holdsReplies: Boolean = capture != CaptureStage.Idle || outbox.holdsUnstored

  /** Speech is being captured, confirmed or not. */
  def capturing: Boolean = capture == CaptureStage.Candidate || capture == CaptureStage.Speech

  def ready: VoiceSession[P] = copy(starting = false)

  def captureAt(stage: CaptureStage): VoiceSession[P] = copy(capture = stage)

  /** The finished recording is queued for upload. A new capture may already
    * have begun meanwhile, so only the finishing stage ends.
    */
  def recordingQueued(id: String, payload: P): VoiceSession[P] =
    copy(capture = finishingEnded, outbox = outbox.enqueue(id, payload))

  /** The finished recording was not sent (it was too long to upload). */
  def recordingDropped: VoiceSession[P] = copy(capture = finishingEnded)

  private def finishingEnded: CaptureStage =
    if capture == CaptureStage.Finishing then CaptureStage.Idle else capture

  /** The next recording to upload, if one is waiting and none is uploading. */
  def startNextUpload: Option[(Outbox.Entry[P], VoiceSession[P])] =
    outbox.startNext.map((next, rest) => (next, copy(outbox = rest)))

  def stored(id: String): VoiceSession[P] = copy(outbox = outbox.stored(id))

  /** The session with a recording's outcome taken in, or None when that
    * outcome was already acted on. A delivered recording's thread now owes a
    * reply.
    */
  def outcome(id: String, deliveredTo: Option[String]): Option[VoiceSession[P]] =
    Option.when(!outcomesActedOn.contains(id)) {
      val settled = copy(outbox = outbox.settled(id), outcomesActedOn = outcomesActedOn + id)
      deliveredTo.fold(settled)(settled.replyAwaited)
    }

  def replyAwaited(threadId: String): VoiceSession[P] =
    copy(awaitingReplies = awaitingReplies + threadId, awaitingVersion = awaitingVersion + 1)

  /** The thread's reply came, or its turn ended without one. */
  def replyCame(threadId: String): VoiceSession[P] =
    if awaitingReplies.contains(threadId) then
      copy(awaitingReplies = awaitingReplies - threadId, awaitingVersion = awaitingVersion + 1)
    else this

  /** The backend's view of which threads owe this session a reply, as of a
    * request made at `asOfVersion`: ignored when a local change came since.
    */
  def backendAwaiting(threadIds: Set[String], asOfVersion: Int): VoiceSession[P] =
    if asOfVersion == awaitingVersion then copy(awaitingReplies = threadIds) else this

  def playingNow(isPlaying: Boolean): VoiceSession[P] = copy(playing = isPlaying)

  def turnActivityNow(next: Option[AgentActivity]): VoiceSession[P] = copy(turnActivity = next)

  def awaitingSpeechNow(waiting: Boolean): VoiceSession[P] = copy(awaitingSpeech = waiting)

object VoiceSession:
  def starting[P]: VoiceSession[P] =
    VoiceSession(
      starting = true,
      capture = CaptureStage.Idle,
      outbox = Outbox.empty,
      outcomesActedOn = Set.empty,
      awaitingReplies = Set.empty,
      awaitingVersion = 0,
      playing = false,
      turnActivity = None,
      awaitingSpeech = false
    )
