package conversecore

/** One segment as the transcriber reports it, timed from the start of the audio. */
final case class TranscriptSegment(startSec: Double, endSec: Double, noSpeechProbability: Double, avgLogProb: Double, text: String):
  def durationSec: Double = endSec - startSec

/** Which of a transcript's segments to keep. Transcribers invent stock
  * phrases on silence, noise, and padding; these rules catch them without
  * touching real speech.
  */
object TranscriptSegments:
  /** The transcriber's own reference rule: silence probability high and confidence low at once. */
  private def readsAsSilence(seg: TranscriptSegment): Boolean =
    seg.noSpeechProbability > 0.6 && seg.avgLogProb < -1.0

  /** How far past the end of the audio a segment may claim to run before it
    * counts as decoded from padding. Whisper-style transcribers work in fixed
    * windows; when the audio stops early in a window, the rest is padding, and
    * a phrase decoded there claims a span the audio does not have.
    */
  val OverrunToleranceSec = 1.0

  private def runsPastAudio(audioDurationSec: Option[Double])(seg: TranscriptSegment): Boolean =
    audioDurationSec.filter(_ > 0).exists(seg.endSec > _ + OverrunToleranceSec)

  /** A short last segment far less confident than the rest of the same
    * utterance is a phrase invented on the trailing silence: the transcriber
    * reports it as speech, so the silence rule misses it.
    */
  val TrailingMaxSec        = 1.5
  val TrailingConfidenceGap = 0.5

  def isInventedTail(segments: List[TranscriptSegment]): Boolean = segments match
    case init :+ last if init.nonEmpty && last.durationSec <= TrailingMaxSec =>
      val others = init.map(_.avgLogProb).sorted
      val median = others(others.length / 2)
      last.avgLogProb < median - TrailingConfidenceGap
    case _ => false

  /** `audioDurationSec` is the length of the audio as the transcriber reports
    * it, when it does.
    */
  def keep(segments: List[TranscriptSegment], audioDurationSec: Option[Double]): List[TranscriptSegment] =
    val spoken = segments.filterNot(readsAsSilence).filterNot(runsPastAudio(audioDurationSec))
    if isInventedTail(spoken) then spoken.init else spoken
