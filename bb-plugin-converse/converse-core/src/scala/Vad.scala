package conversecore

/** Frame-based energy VAD, a port of claude-converse's EnergyVad semantics onto
  * normalized browser audio (RMS of Float32 samples in [0, 1]) instead of raw
  * 16-bit PCM. Frames are ~30ms of audio; defaults mirror Converse's tuning
  * (threshold 300/32768 ≈ 0.009, rounded to 0.01).
  *
  * Differences from the original, driven by MediaRecorder capture:
  *   - There is no PCM pre-buffer. Instead a Candidate phase starts at the
  *     first above-threshold frame so the caller can start its recorder before
  *     speech is confirmed, and is abandoned if speech is not confirmed within
  *     candidateWindowFrames.
  *   - No partial-chunk emission: bb transcription is final-only.
  *   - The minimum-utterance check counts voiced frames only, so it actually
  *     filters coughs/pops (the original's count included trailing silence,
  *     making the check vacuous at default settings).
  */
final case class VadConfig(
    threshold: Double = 0.01,
    speechStartFrames: Int = 3,
    chunkSilenceFrames: Int = 20,
    utteranceEndFrames: Int = 60,
    minUtteranceFrames: Int = 10,
    bargeInEnergyMultiplier: Double = 2.0,
    bargeInFrames: Int = 6,
    candidateWindowFrames: Int = 33
)

enum VadEvent:
  /** First above-threshold frame while idle: start capturing now. */
  case CandidateStart
  /** The candidate never became confirmed speech: discard the capture. */
  case CandidateAbandoned
  /** speechStartFrames consecutive above-threshold frames. */
  case SpeechStart
  /** A shorter pause (chunkSilenceFrames) mid-utterance: a good moment to
    * preview-transcribe what has been said so far. Fired at most once per
    * silence stretch.
    */
  case Partial
  /** utteranceEndFrames of silence after speech. longEnough is false for
    * bursts shorter than minUtteranceFrames of voiced audio.
    */
  case SpeechEnd(longEnough: Boolean)
  /** Sustained loud speech (bargeInFrames at threshold * multiplier); fired
    * once per loud burst. The caller decides whether playback is running.
    */
  case BargeIn

final class EnergyVad(config: VadConfig):
  private var candidateActive      = false
  private var framesSinceCandidate = 0
  private var consecutiveSpeech    = 0
  private var consecutiveSilence   = 0
  private var consecutiveLoud      = 0
  private var inSpeech             = false
  private var bargeInLatched       = false
  private var voicedFrames         = 0
  private var chunkEmitted         = false

  def isInSpeech: Boolean = inSpeech

  def reset(): Unit =
    candidateActive = false
    framesSinceCandidate = 0
    consecutiveSpeech = 0
    consecutiveSilence = 0
    consecutiveLoud = 0
    inSpeech = false
    bargeInLatched = false
    voicedFrames = 0
    chunkEmitted = false

  def pushFrame(rms: Double): List[VadEvent] =
    val out      = List.newBuilder[VadEvent]
    val isSpeech = rms > config.threshold
    val isLoud   = rms > config.threshold * config.bargeInEnergyMultiplier

    if isLoud then
      consecutiveLoud += 1
      if consecutiveLoud >= config.bargeInFrames && !bargeInLatched then
        bargeInLatched = true
        out += VadEvent.BargeIn
    else
      if consecutiveLoud > 0 then bargeInLatched = false
      consecutiveLoud = 0

    if !inSpeech then
      if candidateActive then framesSinceCandidate += 1
      if isSpeech then
        consecutiveSpeech += 1
        if !candidateActive then
          candidateActive = true
          framesSinceCandidate = 0
          out += VadEvent.CandidateStart
        if consecutiveSpeech >= config.speechStartFrames then
          inSpeech = true
          consecutiveSilence = 0
          voicedFrames = consecutiveSpeech
          out += VadEvent.SpeechStart
      else
        consecutiveSpeech = 0
        if candidateActive && framesSinceCandidate >= config.candidateWindowFrames then
          candidateActive = false
          out += VadEvent.CandidateAbandoned
    else
      if isSpeech then
        consecutiveSilence = 0
        voicedFrames += 1
        chunkEmitted = false
      else consecutiveSilence += 1
      if !chunkEmitted && consecutiveSilence == config.chunkSilenceFrames
        && voicedFrames >= config.minUtteranceFrames
      then
        chunkEmitted = true
        out += VadEvent.Partial
      if consecutiveSilence >= config.utteranceEndFrames then
        val longEnough = voicedFrames >= config.minUtteranceFrames
        inSpeech = false
        candidateActive = false
        consecutiveSpeech = 0
        consecutiveSilence = 0
        bargeInLatched = false
        voicedFrames = 0
        chunkEmitted = false
        out += VadEvent.SpeechEnd(longEnough)

    out.result()
