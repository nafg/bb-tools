package billingcore

/** A span of attention on one thread, `[start, end)` in epoch milliseconds, as the activity recorder reports it. */
final case class Interval(threadId: String, start: Long, end: Long):
  def durationMs: Long = end - start

/** An interval clipped to one local day. */
final case class DayInterval(day: String, threadId: String, start: Long, end: Long):
  def durationMs: Long = end - start

object Intervals:
  /** Cuts every interval at local midnight so each piece lies within one day. Empty pieces are dropped. */
  def splitByDay(intervals: Seq[Interval], calendar: Calendar): Vector[DayInterval] =
    intervals.iterator.flatMap { interval =>
      Iterator
        .unfold(interval.start) { cursor =>
          if cursor >= interval.end then None
          else
            val next = calendar.nextDayStart(cursor)
            val end  = math.min(next, interval.end)
            Some(DayInterval(calendar.dayOf(cursor), interval.threadId, cursor, end) -> end)
        }
    }.filter(_.durationMs > 0).toVector
