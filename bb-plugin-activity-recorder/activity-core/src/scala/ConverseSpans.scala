package activitycore

/** One item of converse's `activitySpans` RPC, as decoded. */
final case class ConverseSpan(seq: Long, id: String, threadId: String, kind: String, source: String, start: Long, end: Long)

/** How a page of converse spans becomes signals and a new cursor. */
object ConverseSpans:
  /** `lastSeq` is the page's highest sequence whether or not its item was
    * kept, so a page of unusable items still advances the cursor;
    * `newestEnd` is the latest end among the kept signals.
    */
  final case class Page(signals: Vector[Signal], lastSeq: Option[Long], newestEnd: Option[Long])

  /** The sequences the recorder can no longer fetch: those after its cursor
    * and before the oldest span converse still holds. None when converse
    * does not say what it holds, or nothing was missed.
    */
  def lost(cursorSeq: Option[Long], oldestSeq: Option[Long]): Option[(Long, Long)] =
    oldestSeq.map(oldest => (cursorSeq.getOrElse(0L) + 1, oldest - 1)).filter((from, to) => from <= to)

  def fold(items: Vector[ConverseSpan]): Page =
    val signals = items.flatMap(signal)
    Page(signals, items.map(_.seq).maxOption, signals.map(_.end).maxOption)

  /** A span becomes a signal when its kind is speech or playback and it
    * names a thread; an end before the start is taken as the start.
    */
  def signal(item: ConverseSpan): Option[Signal] =
    SignalKind.parse(item.kind).collect {
      case kind @ (SignalKind.Speech | SignalKind.Playback) if item.threadId.nonEmpty =>
        Signal(item.id, item.threadId, kind, item.start, math.max(item.start, item.end), source(item.source))
    }

  /** Converse's own source names, prefixed: `session` is plain `converse`;
    * `replay` and `backfill` become `converse-replay` and `converse-backfill`.
    */
  def source(converseSource: String): String =
    if converseSource.isEmpty || converseSource == "session" then "converse" else s"converse-$converseSource"
