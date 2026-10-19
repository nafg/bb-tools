package activitycore

/** The gap rule: signals on one thread no further apart than `gapMs` form one
  * interval, and the time between two signals further apart than that counts
  * as nothing.
  */
object Intervals:
  /** Merges `signals` per thread and clips the result to `[from, to)`. Pass
    * every signal that could join one inside the window, which is those
    * reaching into `[from - gapMs, to + gapMs]`. A point signal inside the
    * window stays as a zero-length interval; one on the window's edge or
    * beyond is dropped. Sorted by thread id, then start.
    */
  def derive(signals: Iterable[Signal], gapMs: Long, from: Long, to: Long): Vector[Interval] =
    signals
      .groupBy(_.threadId)
      .toVector
      .sortBy((threadId, _) => threadId)
      .flatMap { (threadId, ofThread) =>
        merge(threadId, ofThread.toVector.sortBy(signal => (signal.start, signal.end)), gapMs)
          .flatMap(clip(_, from, to))
      }

  private def merge(threadId: String, sortedByStart: Vector[Signal], gapMs: Long): Vector[Interval] =
    sortedByStart.foldLeft(Vector.empty[Interval]) { (merged, signal) =>
      merged.lastOption match
        case Some(last) if signal.start - last.end <= gapMs =>
          merged.init :+ last.copy(end = math.max(last.end, signal.end), kinds = last.kinds + signal.kind)
        case _ => merged :+ Interval(threadId, signal.start, signal.end, Set(signal.kind))
    }

  private def clip(interval: Interval, from: Long, to: Long): Option[Interval] =
    val start = math.max(interval.start, from)
    val end   = math.min(interval.end, to)
    if start < end then Some(interval.copy(start = start, end = end))
    else if interval.start == interval.end && from <= interval.start && interval.start < to then Some(interval)
    else None
