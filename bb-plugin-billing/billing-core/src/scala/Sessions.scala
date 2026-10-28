package billingcore

/** A cluster of attention across threads: no gap inside it reaches the session gap. `[start, end)` in epoch
  * milliseconds, from the first stretch's start to the last stretch's end.
  */
final case class Session(start: Long, end: Long, threadIds: Set[String]):
  def durationMs: Long = end - start

object Sessions:
  /** How long a quiet spell, with no attention on any thread, ends one session and begins the next. */
  val DefaultGapMs: Long = 2 * 3600_000L

  /** The sessions of the given stretches, oldest first. A stretch belongs to the session it starts in; a gap shorter
    * than `gapMs` never separates two sessions, however many stretches lie on either side.
    */
  def of(stretches: Iterable[Window], gapMs: Long = DefaultGapMs): Vector[Session] =
    stretches.toVector.sortBy(w => (w.start, w.end)).foldLeft(Vector.empty[Session]) { (acc, w) =>
      acc.lastOption match
        case Some(s) if w.start - s.end < gapMs =>
          acc.init :+ Session(s.start, math.max(s.end, w.end), s.threadIds + w.threadId)
        case _                                  => acc :+ Session(w.start, w.end, Set(w.threadId))
    }

  /** The session a stretch falls in, by where it starts. */
  def containing(sessions: Seq[Session], w: Window): Option[Session] =
    sessions.find(s => w.start >= s.start && w.start <= s.end)
