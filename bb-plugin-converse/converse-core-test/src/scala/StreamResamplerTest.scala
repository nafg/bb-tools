package conversecore

class StreamResamplerTest extends munit.FunSuite:
  private def tone(hz: Double, rate: Int, seconds: Double): IArray[Float] =
    IArray.tabulate((rate * seconds).toInt)(i => (0.5 * math.sin(2 * math.Pi * hz * i / rate)).toFloat)

  private def resample(from: Int, to: Int, pieces: Seq[IArray[Float]]): IArray[Float] =
    val (last, outputs) = pieces.foldLeft((StreamResampler(from, to), Vector.empty[IArray[Float]])) {
      case ((resampler, out), piece) =>
        val (next, produced) = resampler.feed(piece)
        (next, out :+ produced)
    }
    IArray.from((outputs :+ last.finish).flatten)

  private def pieces(input: IArray[Float], sizes: Seq[Int]): Seq[IArray[Float]] =
    Iterator.unfold((input, Iterator.continually(sizes).flatten)) { (rest, size) =>
      Option.when(rest.nonEmpty) {
        val n = size.next()
        (rest.take(n), (rest.drop(n), size))
      }
    }.toSeq

  // Pocket's streamed pieces are 1920 samples, but nothing may depend on it.
  test("the output does not depend on how the input is divided"):
    val input = tone(440, 24000, 1)
    val whole = resample(24000, 48000, Seq(input))
    for sizes <- Seq(Seq(1920), Seq(1), Seq(7, 300, 13), Seq(5000)) do
      assertEquals(resample(24000, 48000, pieces(input, sizes)).toSeq, whole.toSeq, s"pieces of $sizes")

  test("the output has the length of the input at the new rate"):
    assertEquals(resample(24000, 48000, Seq(tone(440, 24000, 1))).length, 48000)
    assertEquals(resample(24000, 44100, Seq(tone(440, 24000, 1))).length, 44100)
    assertEquals(resample(48000, 16000, Seq(tone(440, 48000, 1))).length, 16000)

  private def maxErrorAwayFromEdges(actual: IArray[Float], expected: IArray[Float]): Double =
    val margin = actual.length / 10
    (margin until actual.length - margin).map(i => math.abs(actual(i) - expected(i))).max

  test("a tone keeps its frequency and level"):
    for to <- Seq(48000, 44100) do
      val out = resample(24000, to, Seq(tone(1000, 24000, 0.5)))
      assert(maxErrorAwayFromEdges(out, tone(1000, to, 0.5)) < 1e-3, s"to $to Hz")

  test("at the same rate the audio passes through unchanged"):
    val input = tone(1000, 24000, 0.5)
    assert(maxErrorAwayFromEdges(resample(24000, 24000, Seq(input)), input) < 1e-6)

  test("downsampling removes frequencies the new rate cannot carry"):
    val out = resample(48000, 16000, Seq(tone(12000, 48000, 0.5)))
    assert(maxErrorAwayFromEdges(out, IArray.fill(out.length)(0f)) < 0.01)
