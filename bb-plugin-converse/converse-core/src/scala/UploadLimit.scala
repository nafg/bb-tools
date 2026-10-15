package conversecore

/** The largest recording the backend accepts, shared so the browser can tell
  * before uploading instead of sending a recording that will be refused.
  */
object UploadLimit:
  val MaxAudioBase64Chars: Int = 7 * 1024 * 1024

  /** Base64 encodes every three bytes as four characters. */
  def base64Chars(bytes: Double): Double = math.ceil(bytes / 3) * 4

  def fits(bytes: Double): Boolean = base64Chars(bytes) <= MaxAudioBase64Chars
