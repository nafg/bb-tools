package conversecore

/** The layout of PCM samples: frames per second, and samples per frame. */
final case class PcmFormat(sampleRate: Int, channels: Int)

/** A 16-bit PCM WAV file read as it arrives in pieces, the way a TTS provider
  * that streams its output sends it. The size fields of such a stream are
  * placeholders written before its length was known, so its samples are taken
  * to run to the end of the stream.
  */
enum WavStream:
  /** Still in the header; everything received so far. */
  case InHeader(received: IArray[Byte])

  /** In the samples; the bytes of a frame not yet complete. */
  case InSamples(format: PcmFormat, partialFrame: IArray[Byte])

  /** The stream with `bytes` taken in, and the samples they completed, one
    * array per channel; or why the stream cannot be read as 16-bit PCM WAV.
    */
  def feed(bytes: IArray[Byte]): Either[String, (WavStream, IArray[IArray[Float]])] =
    this match
      case InHeader(received) =>
        val all = received ++ bytes
        WavStream.header(all).flatMap {
          case None                        => Right((InHeader(all), IArray.empty))
          case Some((format, samplesStart)) => InSamples(format, IArray.empty).feed(all.drop(samplesStart))
        }
      case InSamples(format, partialFrame) =>
        val all        = partialFrame ++ bytes
        val frameBytes = 2 * format.channels
        val frames     = all.length / frameBytes
        val channels = IArray.tabulate(format.channels) { channel =>
          IArray.tabulate(frames) { frame =>
            val at = frame * frameBytes + 2 * channel
            ((all(at) & 0xff) | (all(at + 1) << 8)).toShort / 32768f
          }
        }
        Right((InSamples(format, all.drop(frames * frameBytes)), channels))

object WavStream:
  val start: WavStream = InHeader(IArray.empty)

  /** The format and where the samples begin, once the header has fully
    * arrived; None while more of it is still to come.
    */
  private def header(bytes: IArray[Byte]): Either[String, Option[(PcmFormat, Int)]] =
    def text(at: Int)    = new String(bytes.slice(at, at + 4).toArray, "US-ASCII")
    def int16(at: Int)   = (bytes(at) & 0xff) | ((bytes(at + 1) & 0xff) << 8)
    def int32(at: Int)   = int16(at) | (int16(at + 2) << 16)
    if bytes.length < 12 then Right(None)
    else if text(0) != "RIFF" || text(8) != "WAVE" then Left("not a WAV stream")
    else
      // The header's chunks, each an id, a size and that many bytes (padded to
      // an even count), up to the "data" chunk holding the samples.
      def chunks(at: Int, format: Option[PcmFormat]): Either[String, Option[(PcmFormat, Int)]] =
        if bytes.length < at + 8 then Right(None)
        else
          val size = int32(at + 4)
          text(at) match
            case id if id != "data" && size < 0 => Left(s"WAV chunk $id has no usable size")
            case "data" =>
              format.map(f => Right(Some((f, at + 8)))).getOrElse(Left("WAV samples before their format"))
            case "fmt " =>
              if bytes.length < at + 8 + 16 then Right(None)
              else
                val encoding = int16(at + 8)
                val bits     = int16(at + 22)
                if encoding != 1 || bits != 16 then
                  Left(s"WAV samples are not 16-bit PCM (encoding $encoding, $bits bits)")
                else chunks(at + 8 + size + (size & 1), Some(PcmFormat(int32(at + 12), int16(at + 10))))
            case _ => chunks(at + 8 + size + (size & 1), format)
      chunks(12, None)
