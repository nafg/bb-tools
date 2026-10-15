package conversecore

/** One segment as the transcriber reports it. */
final case class TranscriptSegment(durationSec: Double, noSpeechProbability: Double, avgLogProb: Double, text: String)

/** Which of a transcript's segments to keep. Transcribers invent stock
  * phrases on silence and noise; two rules catch them without touching real
  * speech.
  */
object TranscriptSegments:
  /** The transcriber's own reference rule: silence probability high and confidence low at once. */
  private def readsAsSilence(seg: TranscriptSegment): Boolean =
    seg.noSpeechProbability > 0.6 && seg.avgLogProb < -1.0

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

  def keep(segments: List[TranscriptSegment]): List[TranscriptSegment] =
    val spoken = segments.filterNot(readsAsSilence)
    if isInventedTail(spoken) then spoken.init else spoken
