package billingcore

/** A tool the agent called in a thread, with when: the raw material for noticing time that already went to Zoho. */
final case class ToolCall(at: Long, name: String, args: String)

/** Evidence, from the agent's own tool calls, that a thread's time already went to Zoho by hand through the Zoho timer
  * MCP: `timer_create` creates and starts a timer (Zoho runs one timer at a time, so it ends whatever ran),
  * `timer_start` resumes an entry, `timer_stop` stops the running one. Such time must not be billed again. `detail` is
  * what the timer was for, from the call's arguments.
  */
enum ZohoTrace:
  /** The timer ran from `start` to `end` in the thread. */
  case TimerRan(start: Long, end: Long, detail: String)

  /** The timer was started and not stopped in the thread within a working day: stopped elsewhere, or much later. */
  case TimerLeftRunning(start: Long, detail: String)

  /** True when the trace touches the window `[start, end)`. */
  def touches(start: Long, end: Long): Boolean =
    this match
      case TimerRan(s, e, _)      => s < end && e > start
      case TimerLeftRunning(s, _) => s >= start && s < end

object ZohoTrace:
  /** A tool call's JSON arguments as `key: value` pairs, for people; anything unparseable stays as it is. */
  def readable(args: String): String =
    val pair  = """"([^"]+)"\s*:\s*(?:"((?:[^"\\]|\\.)*)"|([-0-9.]+|true|false))""".r
    val pairs =
      pair.findAllMatchIn(args).map(m => s"${m.group(1)}: ${Option(m.group(2)).getOrElse(m.group(3))}").toVector
    if pairs.isEmpty then args else pairs.mkString(", ")

  /** A start and a stop further apart than this are not one run of the timer. */
  val MaxRunMs: Long = 12 * 3600_000L

  private def lower(name: String) = name.toLowerCase

  /** True for a call to a Zoho timer tool, whatever the MCP server prefix. */
  def isZohoTimeTool(name: String): Boolean =
    val n = lower(name)
    n.contains("zoho") && n.contains("timer")

  /** A timer left running in the thread, given the time Zoho logged for its entry, becomes a run over the thread's
    * recorded activity: from the start until that much recorded activity has gone by, never beyond the end of the local
    * day the timer started on (`dayEnd` of the start), because a Zoho entry's time belongs to its date and a timer does
    * not run past midnight. Logged time is not clock time (the timer may have been paused), so the run is measured in
    * recorded activity, and that logged time is only an upper bound: only activity that could have fallen while the
    * timer ran counts. When the day's recorded activity runs out before the logged time is used up, the run ends with
    * the last of it; when there is none, the trace stays a start with no run. `recording` is the thread's recorded
    * intervals.
    */
  def lengthened(
    traces: Vector[ZohoTrace],
    loggedMs: TimerLeftRunning => Option[Long],
    recording: Seq[(Long, Long)],
    dayEnd: Long => Long): Vector[ZohoTrace] =
    val sorted                                        = recording.sortBy(_._1)
    def endAfter(start: Long, ms: Long): Option[Long] =
      val cap   = dayEnd(start)
      val inDay = sorted.collect { case (s, e) if e > start && s < cap => (math.max(s, start), math.min(e, cap)) }
        .filter((s, e) => s < e)
      val (end, _) = inDay.foldLeft((Option.empty[Long], ms)) { case ((done, left), (s, e)) =>
        if done.isDefined then (done, left)
        else if e - s >= left then (Some(s + left), 0L)
        else (None, left - (e - s))
      }
      end.orElse(inDay.map(_._2).maxOption)
    traces.map {
      case t: TimerLeftRunning =>
        loggedMs(t).filter(_ > 0).flatMap(ms => endAfter(t.start, ms).filter(_ > t.start).map(ms -> _))
          .fold(t)((ms, end) =>
            TimerRan(
              t.start,
              end,
              s"${t.detail}; ${Durations.hoursMinutes(ms)} logged, measured over recorded activity"
            )
          )
      case other               => other
    }

  /** The traces in one thread's calls, in time order. */
  def of(calls: Seq[ToolCall]): Vector[ZohoTrace] =
    val sorted = calls.filter(c => isZohoTimeTool(c.name)).sortBy(_.at)
    // A start replaced within a minute (a failed `timer_start`, retried as a create) is no run at all.
    def close(open: Option[(Long, String)], at: Long): Option[ZohoTrace] =
      open.collect {
        case (s, d) if at - s > MaxRunMs => TimerLeftRunning(s, d)
        case (s, d) if at - s >= 60_000L => TimerRan(s, at, d)
      }
    val (open, done)                                                     =
      sorted.foldLeft((Option.empty[(Long, String)], Vector.empty[ZohoTrace])) { case ((open, acc), c) =>
        val n = lower(c.name)
        if n.endsWith("timer_create") || n.endsWith("timer_start") then
          (Some((c.at, readable(c.args))), acc ++ close(open, c.at))
        else if n.endsWith("timer_stop") then (None, acc ++ close(open, c.at))
        else (open, acc)
      }
    done ++ open.map((s, d) => TimerLeftRunning(s, d))
