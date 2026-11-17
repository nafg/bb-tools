package subscriptionsource

/** What a subscription source made of a target string a thread wants to subscribe to. */
enum Recognition:
  /** The source watches this target. `url` is its canonical form, stored and handed back to every check; `label` is
    * how messages name it.
    */
  case Recognized(url: String, label: String)
  /** The string is this source's kind of target but isn't valid, for example a thread with no event named. */
  case Invalid(reason: String)
  case NotMine

/** One check of a subscribed target. `previous` is the state the last check returned, or `None` when the thread is
  * subscribing; a first check reports nothing, it only records where things stand. `force` asks the source to also
  * read what it normally reads only when something has changed.
  */
final case class CheckRequest(url: String, previous: Option[String], subscriberThreadId: String, force: Boolean)

/** The result of a check.
  *
  * `state` is opaque to the broker: stored and passed back as the next check's `previous`. `messages` are the
  * paragraphs to send the subscriber, in order, none on a first check. `summary` describes where the target stands
  * now; `promise` says what a subscriber will hear about. `finished` means nothing more will happen, which ends the
  * subscription. `link` is how a message points at the target.
  */
final case class CheckResult(
    state: String,
    label: String,
    summary: String,
    promise: String,
    link: String,
    messages: List[String],
    finished: Boolean
)

/** The RPC method names a source plugin registers, discoverable, for the subscriptions plugin to find and call. */
object SourceMethods:
  val Recognize = "subscriptionsRecognize"
  val Check     = "subscriptionsCheck"

/** Text helpers for quoting what people wrote. */
object Text:
  private val MaxChars = 1500

  /** The text as GitHub or GitLab displays it: without HTML comments, which bots use for hidden markers, and without
    * runs of blank lines.
    */
  def visibleText(body: String): String =
    body
      .replaceAll("(?s)<!--.*?-->", "")
      // Formatting tags carry no information for an agent; their text stays. Tags that break a line or paragraph
      // become a space so the words on either side stay apart.
      .replaceAll("(?i)</?(p|br|div)\\b[^>]*>", " ")
      .replaceAll("(?i)</?(strong|b|em|i|small|sub|sup|code|span)\\b[^>]*>", "")
      .linesIterator
      .map(compactLine)
      .mkString("\n")
      .replaceAll("\n{3,}", "\n\n")
      .trim

  /** Drops the padding a Markdown table or aligned text carries: runs of spaces inside the line, and the dashes of
    * a table's separator row beyond three. Leading indentation is kept, since it can be meaningful.
    */
  private def compactLine(line: String): String =
    val indent = line.takeWhile(_ == ' ')
    val rest   = line.drop(indent.length).stripTrailing.replaceAll(" {2,}", " ")
    val table  = if rest.matches("""\|?(\s*:?-+:?\s*\|)+\s*:?-*:?\s*""") then rest.replaceAll("-{4,}", "---") else rest
    indent + table

  /** The visible text, cut to a length fit for a message. */
  def excerpt(body: String): String =
    val trimmed = visibleText(body)
    if trimmed.length <= MaxChars then trimmed else trimmed.take(MaxChars) + "… (cut; open the link for the rest)"

  /** `text` indented as a quote under a list item or heading. */
  def quoted(text: String): String = text.linesIterator.map("  " + _).mkString("\n")
