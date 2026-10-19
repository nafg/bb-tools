package billingcore

/** A span of one day during which the same two or more threads were attended at once; a new block begins whenever the
  * set of attended threads changes, so "all to one thread" never credits time that thread was not present for. Dividing
  * the span is the reviewer's decision; nothing here splits it.
  */
final case class OverlapBlock(day: String, start: Long, end: Long, threadIds: Set[String]):
  def durationMs: Long = end - start
  def key: BlockKey    = BlockKey(day, start, end, threadIds)

/** Identifies a block across recomputations: a stored decision applies only while a block with this exact key exists.
  */
final case class BlockKey(day: String, start: Long, end: Long, threadIds: Set[String]):
  /** The stable text form used as a storage key: `day|start|end|threadA,threadB`. */
  def text: String = s"$day|$start|$end|${threadIds.toSeq.sorted.mkString(",")}"

object BlockKey:
  def parse(text: String): Option[BlockKey] =
    text.split('|') match
      case Array(day, start, end, threads) =>
        for s <- start.toLongOption; e <- end.toLongOption
        yield BlockKey(day, s, e, threads.split(',').filter(_.nonEmpty).toSet)
      case _                               => None

object Overlaps:
  /** A segment between two consecutive boundaries with the threads attended during it. */
  private final case class Segment(start: Long, end: Long, active: Set[String])

  private def segments(intervals: Seq[DayInterval]): Vector[Segment] =
    val bounds = intervals.iterator.flatMap(i => Iterator(i.start, i.end)).toVector.distinct.sorted
    bounds
      .zip(bounds.drop(1))
      .map { case (s, e) =>
        Segment(s, e, intervals.iterator.filter(i => i.start < e && i.end > s).map(_.threadId).toSet)
      }
      .filter(_.active.nonEmpty)

  /** The overlap blocks of one day's intervals, in time order. */
  def blocks(day: String, intervals: Seq[DayInterval]): Vector[OverlapBlock] =
    segments(intervals.filter(_.day == day))
      .filter(_.active.size >= 2)
      .map(seg => OverlapBlock(day, seg.start, seg.end, seg.active))
