package activitycore

/** One recorded interval of one covered thread, as a submission lists it; `day` is its local day. */
final case class Window(threadId: String, day: String, start: Long, end: Long):
  def durationMs: Long = end - start
  def key: String      = s"$threadId|$start|$end"

/** A cluster of activity across threads: no gap inside it reaches the session gap. `[start, end)` in epoch
  * milliseconds, from the first stretch's (or agent span's) start to the last one's end.
  */
final case class Session(start: Long, end: Long, threadIds: Set[String]):
  def durationMs: Long = end - start

object Sessions:
  /** How long a quiet spell, with no attention on any thread and no agent work the user set going, ends one session
    * and begins the next.
    */
  val DefaultGapMs: Long = 2 * 3600_000L

  /** The sessions of the given stretches and agent spans, oldest first. A stretch belongs to the session it starts
    * in; a gap shorter than `gapMs` never separates two sessions, however many stretches lie on either side.
    *
    * An agent span keeps a session open until it ends, and the gap counts from then, when the span qualifies: it
    * was started by the user, or by a thread already in the session (see [[Starter]]). A span that does not
    * qualify is ignored, so automations and bb's own workers never chain sessions together.
    */
  def of(stretches: Iterable[Window], agentSpans: Iterable[AgentSpan] = Nil, gapMs: Long = DefaultGapMs): Vector[Session] =
    // Attention and spans in one order: by start, attention first at a tie, so a span the user started at the very
    // moment of a stretch joins that stretch's session.
    val items: Vector[(Long, Long, Int, Either[Window, AgentSpan])] =
      stretches.toVector.map(w => (w.start, w.end, 0, Left(w): Either[Window, AgentSpan])) ++
        agentSpans.toVector.map(s => (s.start, s.end, 1, Right(s): Either[Window, AgentSpan]))
    items.sortBy((start, end, rank, _) => (start, rank, end)).foldLeft(Vector.empty[Session]) { (acc, item) =>
      val (start, end, _, content) = item
      val threadId                 = content.fold(_.threadId, _.threadId)
      val qualifies                = content.fold(_ => true, span => span.startedBy.isUser || acc.lastOption.exists(s => span.startedBy.threadIds.exists(s.threadIds)))
      if !qualifies then acc
      else
        acc.lastOption match
          case Some(s) if start - s.end < gapMs =>
            acc.init :+ Session(s.start, math.max(s.end, end), s.threadIds + threadId)
          case _                                => acc :+ Session(start, end, Set(threadId))
    }

  /** The session a stretch falls in, by where it starts. */
  def containing(sessions: Seq[Session], w: Window): Option[Session] =
    sessions.find(s => w.start >= s.start && w.start <= s.end)
