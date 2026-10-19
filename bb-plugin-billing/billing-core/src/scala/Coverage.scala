package billingcore

/** A span of one thread's recording that a submission covered: sent to Zoho, or deliberately not billed. */
final case class Span(threadId: String, start: Long, end: Long):
  def durationMs: Long = end - start

/** What is covered, per thread, by the submissions that exist. Billed is a property of each stretch of recording: it
  * lies inside a submission's spans or it does not. There is no "billed until"; "up to here" is only a way to pick
  * stretches for a submission.
  */
final case class Coverage(spans: Map[String, Vector[Span]]):
  /** The parts of `[start, end)` of `threadId` that no submission covers, in order. */
  def uncovered(threadId: String, start: Long, end: Long): Vector[(Long, Long)] =
    spans.getOrElse(threadId, Vector.empty).sortBy(_.start).foldLeft(Vector((start, end))) { (open, span) =>
      open.flatMap { (s, e) =>
        if span.end <= s || span.start >= e then Vector((s, e))
        else Vector((s, math.min(e, span.start)), (math.max(s, span.end), e)).filter((a, b) => b > a)
      }
    }

  def covers(threadId: String): Boolean = spans.get(threadId).exists(_.nonEmpty)

  /** True when the whole of `[start, end)` of `threadId` lies inside the spans. */
  def coversAll(threadId: String, start: Long, end: Long): Boolean = uncovered(threadId, start, end).isEmpty

object Coverage:
  val empty: Coverage = Coverage(Map.empty)

  def of(spans: Seq[Span]): Coverage = Coverage(spans.groupBy(_.threadId).view.mapValues(_.toVector).toMap)
