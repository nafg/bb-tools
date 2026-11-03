package billingcore

import activitycore.Window

/** What a draft of `windows` would bill. Stretches are cut on the unclipped recording, so their keys never depend on
  * what was sent before; only the summing is confined to the draft.
  */
final case class SubmissionPlan(
  windows: Vector[Window],
  /** Billable milliseconds per local day over the draft's windows: time alone plus decided shares of shared time. */
  totalsByDay: Map[String, Long],
  /** Shared stretches touching the draft that involve a billable thread and still need a decision. */
  undecided: Vector[OverlapBlock],
  /** Shared stretches touching the draft that are decided. */
  decided: Vector[(OverlapBlock, Allocation)]):
  def totalMs: Long = totalsByDay.values.sum

object Submissions:
  /** `windows` is the draft: the stretches ticked for this submission, of the covered `threads`. `intervals` is the
    * recording of every thread (day-split, unclipped), for the shared stretches.
    */
  def plan(
    threads: Seq[String],
    windows: Vector[Window],
    intervals: Seq[DayInterval],
    decisions: Map[BlockKey, Allocation],
    billability: String => Billability): SubmissionPlan =
    val range    = threads.toSet
    val included = windows.filter(w => range(w.threadId)).sortBy(w => (w.start, w.threadId))
    def includedMs(threadId: String, start: Long, end: Long): Long =
      included.iterator.filter(_.threadId == threadId).map(w =>
        math.max(0L, math.min(end, w.end) - math.max(start, w.start))
      ).sum

    val days     = intervals.map(_.day).distinct
    val blocks   = days.flatMap(day => Overlaps.blocks(day, intervals))
    val relevant = blocks.filter(b => b.threadIds.exists(t => includedMs(t, b.start, b.end) > 0))
    val (decidedBlocks, undecidedBlocks) = relevant.partition(b => decisions.contains(b.key))
    // A stretch needs a decision only when a billable thread is in it.
    val undecided                        = undecidedBlocks.filter(b =>
      b.threadIds.exists(t =>
        billability(t) match
          case Billability.Billable(_) => true
          case Billability.NotBillable => false
      )
    )

    val byDay = scala.collection.mutable.Map.empty[String, Long].withDefaultValue(0L)
    // Time alone: segments where exactly one thread is active are not blocks; take each window minus its block overlap.
    included.foreach { w =>
      val shared = blocks.iterator.filter(b => b.threadIds(w.threadId)).map(b =>
        math.max(0L, math.min(w.end, b.end) - math.max(w.start, b.start))
      ).sum
      byDay(w.day) += w.durationMs - shared
    }
    // Shared time: each decided stretch gives covered threads their share, confined to what the draft includes.
    decidedBlocks.foreach { b =>
      val share = decisions(b.key).share(b.durationMs)
      share.foreach { (t, ms) =>
        val counted = includedMs(t, b.start, b.end)
        if range.contains(t) && counted > 0 then byDay(b.day) += ms * counted / b.durationMs
      }
    }
    SubmissionPlan(
      windows = included,
      totalsByDay = byDay.toMap.filter(_._2 > 0),
      undecided = undecided.toVector,
      decided = decidedBlocks.toVector.map(b => b -> decisions(b.key))
    )

  /** The one-click decision for a plan: every undecided stretch with exactly one billable thread goes to it. */
  def soleBillableDecisions(undecided: Vector[OverlapBlock], billability: String => Billability): Vector[Decision] =
    undecided.flatMap { block =>
      block.threadIds.filter(t =>
        billability(t) match
          case Billability.Billable(_) => true
          case Billability.NotBillable => false
      ).toList match
        case only :: Nil => Some(Decision(block.key, Allocation.WholeTo(only)))
        case _           => None
    }

object Windows:
  /** The shortest piece a cut may leave at the end of a window. */
  val MinTailMs: Long = 15 * 60_000L

  /** Cuts a window longer than `maxMs` into pieces at the user's own message times, so each piece reads as one stretch
    * of work: a cut goes at the first message after the piece reaches `maxMs`, unless it would leave a tail shorter
    * than `MinTailMs`. A window with no messages to cut at stays whole. Keys stay stable because message times do not
    * change.
    */
  def subdivide(window: Window, messageTimes: Seq[Long], maxMs: Long): Vector[Window] =
    val cuts   = messageTimes.filter(t => t > window.start && t < window.end).sorted
    val starts = cuts.foldLeft(Vector(window.start)) { (acc, t) =>
      if t - acc.last >= maxMs && window.end - t >= MinTailMs then acc :+ t else acc
    }
    starts.zip(starts.drop(1) :+ window.end).map((s, e) => window.copy(start = s, end = e))
