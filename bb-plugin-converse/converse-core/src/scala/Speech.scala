package conversecore

/** A sentence-or-list-item-sized piece of speakable text with the pause to
  * insert after speaking it.
  */
final case class SpeechChunk(text: String, pauseSeconds: Double)

/** Markdown cleanup and sentence chunking for TTS, ported from
  * claude-converse's text.ts.
  */
object Speech:
  private val SentencePause  = 0.25
  private val ParagraphPause = 0.45

  private val Abbrevs =
    ("(?i)\\b(?:Mr|Mrs|Ms|Dr|Prof|Sr|Jr|vs|etc|approx|dept|est|govt|e\\.g|i\\.e|a\\.m|p\\.m|U\\.S" +
      "|Inc|Ltd|Co|Corp|Gen|Gov|Sgt|Pvt|Capt|Lt|Cmdr|Adm|Rev|Hon|Pres|Vol|No)\\.$").r
  private val SingleInitial  = "^[A-Z]\\.$".r
  private val SentenceEnding = ".*[.!?]$".r
  private val ListLine       = "^\\s*(?:[-*+]\\s|\\d+\\.\\s).*".r

  def stripMarkdown(text: String): String =
    var out = text
    out = out.replaceAll("(?s)```.*?```", " (code omitted) ")
    out = out.replaceAll("`([^`]+)`", "$1")
    out = out.replaceAll("\\*{1,3}(.+?)\\*{1,3}", "$1")
    out = out.replaceAll("_{1,3}(.+?)_{1,3}", "$1")
    out = out.replaceAll("(?m)^#{1,6}\\s+", "")
    out = out.replaceAll("\\[([^\\]]+)\\]\\([^)]+\\)", "$1")
    out = out.replaceAll("(?m)^[-*_]{3,}\\s*$", "")
    out = out.replaceAll("<[^>]+>", "")
    out = out.replaceAll("(?m)^\\s*[-*+]\\s+", "")
    out = out.replaceAll("(?m)^\\s*\\d+\\.\\s+", "")
    out.trim

  private def splitSentences(text: String): List[String] =
    val words     = text.split("\\s+").toList.filter(_.nonEmpty)
    val sentences = List.newBuilder[String]
    var current   = Vector.empty[String]
    for word <- words do
      current = current :+ word
      if SentenceEnding.matches(word) then
        val joined = current.mkString(" ")
        val abbrev = Abbrevs.findFirstIn(joined).isDefined || SingleInitial.matches(word)
        if !abbrev then
          sentences += joined
          current = Vector.empty
    if current.nonEmpty then sentences += current.mkString(" ")
    sentences.result()

  def splitSpeechChunks(text: String): List[SpeechChunk] =
    if text.trim.isEmpty then Nil
    else
      val chunks     = collection.mutable.ListBuffer.empty[SpeechChunk]
      val paragraphs = text.split("\\n{2,}").toList.map(_.trim).filter(_.nonEmpty)
      for (paragraph, paragraphIndex) <- paragraphs.zipWithIndex do
        val lines  = paragraph.split("\n").toList
        val isList = lines.exists(ListLine.matches)
        if isList then
          for line <- lines do
            val clean = stripMarkdown(line).trim
            if clean.nonEmpty then chunks += SpeechChunk(clean, SentencePause)
        else
          val clean    = stripMarkdown(paragraph)
          val fullText = clean.split("\n").map(_.trim).filter(_.nonEmpty).mkString(" ")
          for sentence <- splitSentences(fullText) do
            val trimmed = sentence.trim
            if trimmed.nonEmpty then chunks += SpeechChunk(trimmed, SentencePause)
        if paragraphIndex < paragraphs.length - 1 && chunks.nonEmpty then
          chunks(chunks.length - 1) = chunks.last.copy(pauseSeconds = ParagraphPause)
      if chunks.nonEmpty then chunks(chunks.length - 1) = chunks.last.copy(pauseSeconds = 0)
      chunks.result()
