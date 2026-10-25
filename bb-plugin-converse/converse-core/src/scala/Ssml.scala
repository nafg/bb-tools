package conversecore

/** Speech Synthesis Markup Language for a speech chunk, as Google Cloud
  * Text-to-Speech's WaveNet, Standard and Neural2 voices read it. A word the
  * agent put between single *asterisks* is stressed with the standard
  * `<emphasis>` element; the rest is text, escaped. (Studio voices ignore
  * `<emphasis>`, and Chirp 3 HD voices take no SSML at all.)
  */
object Ssml:
  /** `text` as SSML character data: the five characters SSML reserves are
    * escaped.
    */
  def escape(text: String): String =
    val out = StringBuilder()
    text.foreach {
      case '&'   => out ++= "&amp;"
      case '<'   => out ++= "&lt;"
      case '>'   => out ++= "&gt;"
      case '"'   => out ++= "&quot;"
      case '\''  => out ++= "&apos;"
      case other => out += other
    }
    out.result()

  /** A whole SSML document speaking `markedText`, with each word between
    * single asterisks (as [[Speech.splitSpeechChunks]] keeps them) in
    * `<emphasis level="strong">`.
    */
  def withEmphasis(markedText: String): String =
    val body = Speech.stressMarkedSpans(markedText).map {
      case StressSpan.Plain(text)    => escape(text)
      case StressSpan.Stressed(text) => s"""<emphasis level="strong">${escape(text)}</emphasis>"""
    }
    s"<speak>${body.mkString}</speak>"

/** A run of a speech chunk's text: as written, or marked to be stressed. */
enum StressSpan:
  case Plain(text: String)
  case Stressed(text: String)
