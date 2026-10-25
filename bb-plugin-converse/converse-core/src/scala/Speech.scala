package conversecore

/** A sentence-or-list-item-sized piece of speakable text with the pause to
  * insert after speaking it. In `text`, a word the agent marked to be stressed
  * is still between *asterisks*, as a voice that can stress a word reads it;
  * [[withoutStressMarks]] is the same text for one that cannot.
  */
final case class SpeechChunk(text: String, pauseSeconds: Double):
  def withoutStressMarks: String = Speech.withoutStressMarks(text)

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
  // A period joining word characters (object.method, Widget.scala). At least
  // two characters must follow so decimals (3.5) and dotted abbreviations
  // (e.g., a.m., U.S.) keep their periods.
  private val IdentifierDot  = "([A-Za-z0-9_])\\.(?=[A-Za-z_][A-Za-z0-9_])".r
  // A short word with no vowels, such as a tool's name (sbt, npm, ssh) or an
  // acronym (PR).
  private val VowellessWord  = "[b-df-hj-np-tv-xzB-DF-HJ-NP-TV-XZ]{2,5}".r
  // Vowelless words that are said as words.
  private val SaidAsWords    =
    Set("mr", "mrs", "dr", "sr", "jr", "st", "lt", "sgt", "pvt", "vs", "nth") ++
      Set("hm", "hmm", "mm", "mhm", "shh", "psst", "pst", "tsk", "brr", "grr", "pfft", "zzz")
  // A number marked with "#", as in issue and pull request references.
  private val HashNumber     = "(^|[^\\w#])#(\\d+)\\b".r
  // A word, with what follows an apostrophe (I'll, PR's) kept in it.
  private val Word           = "\\b[A-Za-z0-9]+(?:['’][A-Za-z]+)*".r
  private val Digits         = "\\d+".r
  // A bb thread: a mention (with or without a message) or a bare id.
  private val ThreadReference = "@thread:(thr_[a-z0-9]+)(?:#msg=\\d+)?|\\b(thr_[a-z0-9]+)\\b".r
  // Words between single asterisks, which mark them to be stressed.
  private val StressMark      = "\\*([^*\\s](?:[^*]*[^*\\s])?)\\*".r

  /** `text` with its stress marks removed, for a voice that cannot stress a
    * word, or for showing the text.
    */
  def withoutStressMarks(text: String): String = StressMark.replaceAllIn(text, "$1")

  /** The ids of the bb threads `text` refers to, whose titles
    * [[splitSpeechChunks]] speaks in their place.
    */
  def threadsReferredTo(text: String): Set[String] =
    ThreadReference.findAllMatchIn(text).map(m => Option(m.group(1)).getOrElse(m.group(2))).toSet

  // No (?m)/(?s) flags: Scala.js rejects MULTILINE and DOTALL on the default
  // ES target, so line anchors are handled by splitting instead.
  /** `text` without its markdown, except that a word between single
    * asterisks keeps them: that is how the agent marks a word to be stressed
    * when spoken, and how Qwen3-TTS reads it. Bold is only emphasis in print,
    * so its asterisks go.
    */
  def stripMarkdown(text: String): String =
    var out = text
    out = out.replaceAll("```[\\S\\s]*?```", " (code omitted) ")
    out = out.replaceAll("`([^`]+)`", "$1")
    out = out.replaceAll("\\*{2,3}(.+?)\\*{2,3}", "$1")
    out = out.replaceAll("_{1,3}(.+?)_{1,3}", "$1")
    out = out.replaceAll("\\[([^\\]]+)\\]\\([^)]+\\)", "$1")
    out = out.replaceAll("<[^>]+>", "")
    out = out
      .split("\n", -1)
      .map { line =>
        var l = line.replaceFirst("^#{1,6}\\s+", "")
        l = if l.matches("[-*_]{3,}\\s*") then "" else l
        l = l.replaceFirst("^\\s*[-*+]\\s+", "")
        l.replaceFirst("^\\s*\\d+\\.\\s+", "")
      }
      .mkString("\n")
    out.trim

  /** TTS engines end a sentence at every period, so speak the ones inside
    * identifiers as the word "dot". Pocket TTS garbles a number written with
    * "#" (it dropped the numbers and repeated the words around them), so
    * "#233" is spoken as "number 233"; and it says a short word with no vowels
    * as a word ("sbt" as "sess B T", "ssh" as "set"), so such a word is
    * spelled out ("S B T"), as are hashes and ids, which it garbled. A
    * camelCase name is split into its words first, so each word is spoken as
    * any other is ("smsChat" as "S M S Chat"). Numbers are spoken as words,
    * since it read "3754" differently from one sentence to the next.
    */
  private def speakable(text: String): String =
    val dotted   = IdentifierDot.replaceAllIn(text, "$1 dot ")
    val numbered = HashNumber.replaceAllIn(dotted, "$1number $2")
    val worded   = Word.replaceAllIn(numbered, m => spoken(m.matched))
    Digits.replaceAllIn(worded, m => if leftAsDigits(worded, m.start, m.end) then m.matched else NumberWords(m.matched))

  /** A word as it is spoken. What follows an apostrophe is kept as it is
    * (the "ll" of "I'll" is no word to spell out), and the rest is spoken
    * like any word.
    */
  private def spoken(token: String): String =
    val apostrophe = token.indexWhere(c => c == '\'' || c == '’')
    if apostrophe >= 0 then spoken(token.take(apostrophe)) + token.drop(apostrophe)
    else if isId(token) then spelled(token)
    else words(token).split(" ").map(spokenWord).mkString(" ")

  /** One word of a name, spelled out if it is an acronym or has no vowels. */
  private def spokenWord(word: String): String =
    if word.length >= 3 && word.last == 's' && word.init.forall(_.isUpper) then spelled(word.init) + "'s"
    else if VowellessWord.matches(word) && !SaidAsWords(word.toLowerCase) then spelled(word)
    else word

  /** The words of a name run together: split where a lowercase letter meets a
    * capital (smsChat), a run of capitals meets a capitalized word
    * (HTTPServer), or letters meet digits (Scala3, V2Sms).
    */
  private def words(token: String): String =
    val out = StringBuilder()
    for i <- token.indices do
      val c = token(i)
      if i > 0 then
        val before = token(i - 1)
        val next   = token.lift(i + 1)
        // The "s" of a plural acronym (IDs, URLs) is no word of its own.
        val plural = next.contains('s') && i + 2 == token.length
        val joint =
          (c.isUpper && (before.isLower || (before.isUpper && next.exists(_.isLower) && !plural))) ||
            (c.isDigit != before.isDigit)
        if joint then out += ' '
      out += c
    out.result()

  /** Digits spoken as they are: part of a decimal or version number (2.13,
    * 3.5), or an amount of money ($5).
    */
  private def leftAsDigits(text: String, start: Int, end: Int): Boolean =
    (start >= 2 && text(start - 1) == '.' && text(start - 2).isDigit) ||
      (end + 1 < text.length && text(end) == '.' && text(end + 1).isDigit) ||
      (start >= 1 && text(start - 1) == '$')

  /** A hash or an id: one case throughout, at least five characters, with
    * letters and digits alternating at least twice (0db1ea5, uexjg8nei2).
    */
  private def isId(token: String): Boolean =
    token.length >= 5 && (token == token.toLowerCase || token == token.toUpperCase) &&
      token.sliding(2).count(pair => pair(0).isDigit != pair(1).isDigit) >= 2

  private def spelled(token: String): String = token.toUpperCase.mkString(" ")

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

  /** `text` in the pieces it is spoken in. A bb thread it refers to is spoken
    * as its title from `threadTitles`, or as "a thread" when that has none.
    */
  def splitSpeechChunks(text: String, threadTitles: Map[String, String] = Map.empty): List[SpeechChunk] =
    if text.trim.isEmpty then Nil
    else
      val titled = ThreadReference.replaceAllIn(
        text,
        m => scala.util.matching.Regex.quoteReplacement(threadTitles.getOrElse(Option(m.group(1)).getOrElse(m.group(2)), "a thread"))
      )
      val chunks     = collection.mutable.ListBuffer.empty[SpeechChunk]
      val paragraphs = titled.split("\\n{2,}").toList.map(_.trim).filter(_.nonEmpty)
      for (paragraph, paragraphIndex) <- paragraphs.zipWithIndex do
        val lines  = paragraph.split("\n").toList
        val isList = lines.exists(ListLine.matches)
        if isList then
          for line <- lines do
            val clean = stripMarkdown(line).trim
            if clean.nonEmpty then chunks += SpeechChunk(speakable(clean), SentencePause)
        else
          val clean    = stripMarkdown(paragraph)
          val fullText = clean.split("\n").map(_.trim).filter(_.nonEmpty).mkString(" ")
          for sentence <- splitSentences(fullText) do
            val trimmed = sentence.trim
            if trimmed.nonEmpty then chunks += SpeechChunk(speakable(trimmed), SentencePause)
        if paragraphIndex < paragraphs.length - 1 && chunks.nonEmpty then
          chunks(chunks.length - 1) = chunks.last.copy(pauseSeconds = ParagraphPause)
      if chunks.nonEmpty then chunks(chunks.length - 1) = chunks.last.copy(pauseSeconds = 0)
      chunks.result()
