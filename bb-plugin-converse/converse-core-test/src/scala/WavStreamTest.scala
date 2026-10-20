package conversecore

class WavStreamTest extends munit.FunSuite:
  private def le16(v: Int) = IArray[Byte]((v & 0xff).toByte, ((v >> 8) & 0xff).toByte)
  private def le32(v: Int) = le16(v & 0xffff) ++ le16((v >>> 16) & 0xffff)
  private def ascii(s: String) = IArray.from(s.getBytes("US-ASCII"))

  private def fmt(encoding: Int = 1, channels: Int = 1, rate: Int = 24000, bits: Int = 16) =
    ascii("fmt ") ++ le32(16) ++ le16(encoding) ++ le16(channels) ++ le32(rate) ++
      le32(rate * channels * bits / 8) ++ le16(channels * bits / 8) ++ le16(bits)

  // Pocket TTS writes placeholder sizes, since it starts sending before the
  // length is known.
  private def wav(samples: Seq[Int], extra: IArray[Byte] = IArray.empty, format: IArray[Byte] = fmt()) =
    ascii("RIFF") ++ le32(-1) ++ ascii("WAVE") ++ format ++ extra ++ ascii("data") ++ le32(-1) ++
      IArray.from(samples).flatMap(le16)

  private def read(pieces: Seq[IArray[Byte]]): Either[String, (WavStream, Vector[Vector[Float]])] =
    pieces.foldLeft[Either[String, (WavStream, Vector[Vector[Float]])]](Right((WavStream.start, Vector.empty))) {
      case (soFar, piece) =>
        soFar.flatMap { (stream, got) =>
          stream.feed(piece).map { (next, channels) =>
            val merged =
              if got.isEmpty then channels.map(_.toVector).toVector
              else if channels.isEmpty then got
              else got.zip(channels).map((a, b) => a ++ b)
            (next, merged)
          }
        }
    }

  private val samples = Seq(0, 1, -1, 32767, -32768, 1000, -1000, 12345)
  private val expected = samples.map(_ / 32768f).toVector

  test("samples are read as fractions of full scale, however the stream is divided"):
    val bytes = wav(samples)
    for split <- 1 until bytes.length do
      val Right((stream, channels)) = read(Seq(bytes.take(split), bytes.drop(split))): @unchecked
      assertEquals(channels.head, expected, s"split at byte $split")
      val WavStream.InSamples(format, partialFrame) = stream: @unchecked
      assertEquals(format, PcmFormat(24000, 1))
      assert(partialFrame.isEmpty, s"split at byte $split")

  test("a stream fed one byte at a time reads the same"):
    val Right((_, channels)) = read(wav(samples).map(IArray(_)).toSeq): @unchecked
    assertEquals(channels.head, expected)

  test("chunks between the format and the samples are skipped"):
    val list = ascii("LIST") ++ le32(5) ++ ascii("INFOx") ++ IArray(0.toByte)
    val Right((_, channels)) = read(Seq(wav(samples, extra = list))): @unchecked
    assertEquals(channels.head, expected)

  test("stereo samples come out one array per channel"):
    val Right((stream, channels)) = read(Seq(wav(Seq(1, -1, 2, -2), format = fmt(channels = 2)))): @unchecked
    assertEquals(channels, Vector(Vector(1, 2).map(_ / 32768f), Vector(-1, -2).map(_ / 32768f)))
    assertEquals(stream.asInstanceOf[WavStream.InSamples].format, PcmFormat(24000, 2))

  test("audio that is not 16-bit PCM WAV is refused"):
    assert(read(Seq(wav(samples, format = fmt(bits = 8)))).isLeft)
    assert(read(Seq(wav(samples, format = fmt(encoding = 3, bits = 32)))).isLeft)
    assert(read(Seq(ascii("ID3\u0004") ++ IArray.fill[Byte](20)(0))).isLeft)
