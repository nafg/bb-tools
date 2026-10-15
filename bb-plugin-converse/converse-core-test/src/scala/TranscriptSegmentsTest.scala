package conversecore

class TranscriptSegmentsTest extends munit.FunSuite:
  private def seg(durationSec: Double, avgLogProb: Double, text: String, noSpeech: Double = 0.0) =
    TranscriptSegment(durationSec, noSpeech, avgLogProb, text)

  // The figures of a real utterance whose transcript ended in an invented "Thank you."
  private val realUtterance = List(
    seg(5.90, -0.186, "You know, I'm thinking maybe it would be beneficial."),
    seg(4.42, -0.186, "And, you know, you could research all, like, open tasks and stuff."),
    seg(11.30, -0.186, "But one thing that would really be much better..."),
    seg(1.62, -0.186, "It's very kludgy."),
    seg(4.66, -0.186, "It's an artifact of the original Grist implementation."),
    seg(17.96, -0.155, "And so one of the things that might be a stepping stone..."),
    seg(5.26, -0.155, "So again, I'm not talking about a breaking change, but a stepping stone."),
    seg(3.86, -0.155, "And I wonder if it just makes sense to bite the bullet and do that first."),
    seg(1.00, -0.815, "Thank you.")
  )

  test("a short, far less confident last segment is dropped as invented on the trailing silence"):
    assertEquals(TranscriptSegments.keep(realUtterance).map(_.text).last, "And I wonder if it just makes sense to bite the bullet and do that first.")

  test("a short last segment as confident as the rest is real speech and stays"):
    val ending = realUtterance.init :+ seg(1.00, -0.20, "That's it.")
    assertEquals(TranscriptSegments.keep(ending).map(_.text).last, "That's it.")

  test("a long last segment stays even when less confident"):
    val ending = realUtterance.init :+ seg(4.00, -0.9, "and then a longer mumbled sentence that is real")
    assertEquals(TranscriptSegments.keep(ending).length, realUtterance.length)

  test("a one-segment utterance is never emptied by the tail rule"):
    assertEquals(TranscriptSegments.keep(List(seg(0.8, -0.9, "yes"))).map(_.text), List("yes"))

  test("the transcriber's own silence rule still applies anywhere in the utterance"):
    val withSilence = List(seg(3.0, -0.2, "real words"), seg(2.0, -1.4, "Subtitles by", noSpeech = 0.9), seg(3.0, -0.2, "more real words"))
    assertEquals(TranscriptSegments.keep(withSilence).map(_.text), List("real words", "more real words"))
