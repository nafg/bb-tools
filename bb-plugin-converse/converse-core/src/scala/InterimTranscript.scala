package conversecore

/** The live preview of an utterance, assembled from separately transcribed
  * segments. Words can be lost where one segment ends and the next begins,
  * so the seams are shown rather than hidden, and the preview never reads
  * as a finished sentence while more speech is still being captured.
  */
object InterimTranscript:
  val Seam = " … "

  def render(segments: List[String], stillCapturing: Boolean): String =
    val spoken = segments.map(_.trim).filter(_.nonEmpty)
    if spoken.isEmpty then ""
    else spoken.mkString(Seam) + (if stillCapturing then " …" else "")

  /** What the next segment's transcription prompt should see: the words only. */
  def wordsSoFar(segments: List[String]): String = segments.map(_.trim).filter(_.nonEmpty).mkString(" ")
