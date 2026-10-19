package activitycore

/** What the frontend reports about one pane's `view` span at a moment. */
enum ViewReport:
  /** Open a span starting now. */
  case Open
  /** Report the open span's end as `end`. */
  case Extend(end: Long)
  /** Close the open span with `end`; an idle close ends at the last input. */
  case Close(end: Long)

/** The rule for one pane's `view` span. A span is open while the pane is
  * viewed, meaning focused, with the page visible and focused, and input
  * within `idleMs`. It is reported when opened, at most every `heartbeatMs`
  * while open, and when closed.
  */
final case class ViewSpanPolicy(heartbeatMs: Long, idleMs: Long):
  /** `open` is the open span's start and the end last reported, if any. */
  def decide(viewable: Boolean, now: Long, lastInputAt: Long, open: Option[(Long, Long)]): Option[ViewReport] =
    val idle   = now - lastInputAt >= idleMs
    val viewed = viewable && !idle
    (viewed, open) match
      case (true, None)                                                   => Some(ViewReport.Open)
      case (true, Some((_, reportedAt))) if now - reportedAt >= heartbeatMs => Some(ViewReport.Extend(now))
      case (false, Some((start, _)))                                      => Some(ViewReport.Close(if idle then math.max(start, lastInputAt) else now))
      case _                                                              => None
