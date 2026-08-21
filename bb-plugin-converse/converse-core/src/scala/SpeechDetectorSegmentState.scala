package conversecore

final case class SpeechDetectorSegmentStateConfig(
    sampleRate: Int,
    frameSamples: Int,
    positiveSpeechThreshold: Double,
    negativeSpeechThreshold: Double,
    previewPauseMilliseconds: Int,
    trailingContextMilliseconds: Int
)

final case class SpeechDetectorEvidence(
    frameCount: Int,
    speechFrameCount: Int,
    peakRms: Double,
    peakSpeechProbability: Double,
    positiveSpeechThreshold: Double
)

/** State derived from the same Silero frames that establish utterance
  * boundaries. It only decides preview timing and how much of Silero's
  * redemption tail to retain; it is not a second speech detector.
  */
final class SpeechDetectorSegmentState(config: SpeechDetectorSegmentStateConfig):
  private var candidateActive       = false
  private var confirmedSpeech       = false
  private var previewPointEmitted    = false
  private var confidentSilenceSamples = 0
  private var samplesSinceLastSpeech  = 0
  private var frameCount              = 0
  private var speechFrameCount        = 0
  private var peakRms                  = 0.0
  private var peakSpeechProbability    = 0.0

  def beginCandidate(): Unit =
    candidateActive = true
    confirmedSpeech = false
    previewPointEmitted = false
    confidentSilenceSamples = 0
    samplesSinceLastSpeech = 0
    frameCount = 0
    speechFrameCount = 0
    peakRms = 0.0
    peakSpeechProbability = 0.0

  def confirmSpeech(): Unit = confirmedSpeech = true

  def processFrame(speechProbability: Double, rms: Double): Boolean =
    if !candidateActive then false
    else
      frameCount += 1
      peakRms = math.max(peakRms, rms)
      peakSpeechProbability = math.max(peakSpeechProbability, speechProbability)

      if speechProbability >= config.positiveSpeechThreshold then
        speechFrameCount += 1
        samplesSinceLastSpeech = 0
        confidentSilenceSamples = 0
        previewPointEmitted = false
        false
      else
        samplesSinceLastSpeech += config.frameSamples
        if speechProbability < config.negativeSpeechThreshold then
          confidentSilenceSamples += config.frameSamples
        val previewPauseSamples = math.ceil(
          config.previewPauseMilliseconds.toDouble * config.sampleRate / 1000.0
        ).toInt
        val shouldPreview =
          confirmedSpeech && !previewPointEmitted && confidentSilenceSamples >= previewPauseSamples
        if shouldPreview then previewPointEmitted = true
        shouldPreview

  def submittedSampleEndExclusive(totalSampleCount: Int, forcedBoundary: Boolean): Int =
    if forcedBoundary then totalSampleCount
    else
      val trailingContextSamples = math.round(
        config.trailingContextMilliseconds.toDouble * config.sampleRate / 1000.0
      ).toInt
      val excessTrailingSamples = math.max(0, samplesSinceLastSpeech - trailingContextSamples)
      math.max(0, totalSampleCount - excessTrailingSamples)

  def evidence: SpeechDetectorEvidence =
    SpeechDetectorEvidence(
      frameCount = frameCount,
      speechFrameCount = speechFrameCount,
      peakRms = peakRms,
      peakSpeechProbability = peakSpeechProbability,
      positiveSpeechThreshold = config.positiveSpeechThreshold
    )
