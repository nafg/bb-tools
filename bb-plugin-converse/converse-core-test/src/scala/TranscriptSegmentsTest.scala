package conversecore

class TranscriptSegmentsTest extends munit.FunSuite:
  /** Segments laid end to end from zero, as the transcriber reports them. */
  private def laidOut(specs: (Double, Double, String, Double)*): List[TranscriptSegment] =
    specs.foldLeft((0.0, List.empty[TranscriptSegment])) { case ((at, acc), (duration, avgLogProb, text, noSpeech)) =>
      (at + duration, acc :+ TranscriptSegment(at, at + duration, noSpeech, avgLogProb, text))
    }._2
  private def seg(duration: Double, avgLogProb: Double, text: String, noSpeech: Double = 0.0) = (duration, avgLogProb, text, noSpeech)
  private def texts(kept: List[TranscriptSegment]) = kept.map(_.text)

  // Upload 1792037103852-270: 30.12 s of audio. The last segment claims 29.98 s from 29.44 s.
  private val paddedWindow = laidOut(
    seg(10.22, -0.264, "even if it's not like realistic,"),
    seg(6.54, -0.264, "I'm just curious"),
    seg(10.08, -0.264, "what burdens each version"),
    seg(2.60, -0.264, "adds to porting."),
    seg(29.98, -0.200, "Thank you.")
  )

  test("a phrase decoded from the padding after the audio ends is dropped"):
    assertEquals(texts(TranscriptSegments.keep(paddedWindow, Some(30.12))).last, "adds to porting.")

  test("without a usable audio length nothing is dropped for running past it"):
    assertEquals(TranscriptSegments.keep(paddedWindow, None).length, paddedWindow.length)
    assertEquals(TranscriptSegments.keep(paddedWindow, Some(0.0)).length, paddedWindow.length)

  test("a last segment ending within the tolerance of the audio's end stays"):
    val ending = laidOut(seg(10.0, -0.2, "real words"), seg(5.3, -0.2, "and a real ending"))
    assertEquals(TranscriptSegments.keep(ending, Some(15.0)).length, 2)

  // Upload with a "Thank you." in a window of its own: confidence far below the rest.
  private val lowConfidenceTail = laidOut(
    seg(5.90, -0.186, "You know, I'm thinking maybe it would be beneficial."),
    seg(4.42, -0.186, "And, you know, you could research all."),
    seg(17.96, -0.155, "And so one of the things that might be a stepping stone..."),
    seg(3.86, -0.155, "And I wonder if it just makes sense to do that first."),
    seg(1.00, -0.815, "Thank you.")
  )

  test("a short, far less confident last segment is dropped as invented"):
    assertEquals(texts(TranscriptSegments.keep(lowConfidenceTail, Some(33.2))).last, "And I wonder if it just makes sense to do that first.")

  test("a short last segment as confident as the rest is real speech and stays"):
    val ending = laidOut(seg(5.0, -0.2, "real words"), seg(1.0, -0.2, "That's it."))
    assertEquals(texts(TranscriptSegments.keep(ending, Some(6.0))).last, "That's it.")

  test("a one-segment utterance is never emptied by the tail rule"):
    assertEquals(texts(TranscriptSegments.keep(laidOut(seg(0.8, -0.9, "yes")), Some(0.8))), List("yes"))

  test("the transcriber's own silence rule still applies anywhere in the utterance"):
    val withSilence = laidOut(seg(3.0, -0.2, "real words"), seg(2.0, -1.4, "Subtitles by", noSpeech = 0.9), seg(3.0, -0.2, "more real words"))
    assertEquals(texts(TranscriptSegments.keep(withSilence, Some(8.0))), List("real words", "more real words"))
