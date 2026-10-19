package activitycore

/** A thread's attention within a range: the sum of its intervals, and how
  * many there were. A thread with intervals but no time was only touched.
  */
final case class ThreadTotal(threadId: String, ms: Long, intervals: Int):
  def touchedOnly: Boolean = ms == 0

object Summary:
  /** Per-thread totals of the intervals, most time first, ties by thread id. */
  def totals(intervals: Iterable[Interval]): Vector[ThreadTotal] =
    intervals
      .groupBy(_.threadId)
      .map((threadId, ofThread) => ThreadTotal(threadId, ofThread.map(i => i.end - i.start).sum, ofThread.size))
      .toVector
      .sortBy(total => (-total.ms, total.threadId))

  /** Whole minutes as `3h 07m` or `12m`; under a minute is `<1m`, nothing is `0m`. */
  def formatDuration(ms: Long): String =
    val minutes = ms / 60_000
    if minutes >= 60 then f"${minutes / 60}h ${minutes % 60}%02dm"
    else if minutes > 0 || ms == 0 then s"${minutes}m"
    else "<1m"
