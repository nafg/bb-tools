package billing

import billingcore.*

/** The billing state as text, for agents and the terminal. */
object Text:
  private def hm(ms: Long): String = Durations.hoursMinutes(ms)

  def clock(cal: Calendar)(ms: Long): String =
    val secs = (ms - cal.dayStart(ms)) / 1000
    f"${secs / 3600}%02d:${(secs % 3600) / 60}%02d"

  def stamp(cal: Calendar)(ms: Long): String = s"${cal.dayOf(ms)} ${clock(cal)(ms)}"

  /** Threads with unbilled client time, archived ones first: the list to act on. */
  def pending(o: Overview): String =
    val rows = o.threads.filter(_.standing.needsDecision).sortBy(-_.totals.unbilledMs)
    if rows.isEmpty then "No archived client thread has unbilled time."
    else
      rows.map { t =>
        s"- ${t.title} [${t.threadId}] (${t.projectName}): ${Words.totals(t.totals, t.submissions)}, ${Words.when(t, o.calendar.dayOf)}; ${Words.billsTo(t.resolved)}"
      }.mkString("\n")

  /** Every thread with recorded time since the overview's start, as a tree by bb project. */
  def threads(o: Overview): String =
    val all                                                = o.threads
    val ids                                                = all.map(_.threadId).toSet
    val children                                           = all.groupBy(_.parentThreadId.filter(ids).getOrElse(""))
    def line(t: ThreadAccount, depth: Int): Vector[String] =
      s"${"  " * depth}- ${t.title} [${t.threadId}]: ${Words.billsTo(t.resolved)} (${Words.decidedBy(t.resolved, t.threadId)}); ${Words
          .totals(t.totals, t.submissions)}; ${t.standing.label}; ${Words.when(t, o.calendar.dayOf)}" +:
        children.getOrElse(t.threadId, Vector.empty).sortBy(_.title).flatMap(c => line(c, depth + 1))
    val roots                                              = all.filter(t => t.parentThreadId.filter(ids).isEmpty)
    val header = s"Threads with recorded attention since ${stamp(o.calendar)(o.since)} (${o.calendar.timeZone})"
    header +: roots
      .groupBy(t => (t.projectId, t.projectName))
      .toVector
      .sortBy(_._1._2)
      .flatMap { case ((projectId, name), rs) =>
        val zoho = o.projectDefaults.get(projectId).fold("no single Zoho project")(z =>
          s"default ${z.projectName} / ${z.taskName}"
        )
        s"## ${if name.isEmpty then "(unknown bb project)" else name} ($zoho)" +: rs.sortBy(_.title).flatMap(r =>
          line(r, 0)
        )
      } mkString "\n"

  /** A submission plan in full: windows with keys, stretches to settle with keys, dated entries, hand-entered time. */
  def plan(p: PlanResult, blockers: Vector[String]): String =
    val cal                                                 = p.calendar
    val windows                                             = p.lines.map { l =>
      val w = l.window
      s"- ${w.day} ${clock(cal)(w.start)}–${clock(cal)(w.end)} (${hm(w.durationMs)}) ${p.titles.getOrElse(
          w.threadId,
          w.threadId
        )}: ${l.summary}${if l.fromModel then "" else " (first message; no summary yet)"}\n  key: ${w.key}"
    }
    def block(b: OverlapBlock, decided: Option[Allocation]) =
      val who = b.threadIds.toSeq.sorted.map(t => s"${p.titles.getOrElse(t, t)} [$t]").mkString(" + ")
      val d   = decided.map {
        case Allocation.WholeTo(t)  => s"all to ${p.titles.getOrElse(t, t)}"
        case Allocation.Portions(m) =>
          m.map((t, ms) => s"${p.titles.getOrElse(t, t)} ${hm(ms)}").mkString("split: ", ", ", "")
        case Allocation.Nobody      => "nobody"
      }
      s"- ${b.day} ${clock(cal)(b.start)}–${clock(cal)(b.end)} (${hm(
          b.durationMs
        )}): $who\n  key: ${b.key.text}${d.fold("")(x => s"\n  settled: $x")}"
    val entries                                             = p.entries.map(e =>
      s"- ${e.day}: ${hm(e.exactMs)} recorded, ${ZohoEntries.logTime(e.billedMinutes)} to Zoho${
          if e.notesEdited then " (notes edited)"
          else if e.notesFromModel then " (notes by the model)"
          else " (notes are the lines joined; --summaries writes them)"
        }\n  notes: ${e.notes.replace("\n", " / ")}"
    )
    val hand                                                = p.handEntered.map(h =>
      s"- ${h.entry.date}: ${hm(h.entry.minutes * 60_000)} ${h.entry.taskName} by ${h.entry.userName}: ${h.entry.notes} (${h.entry.billedStatus})"
    )
    Vector(
      s"# ${p.root.title} [${p.root.threadId}]",
      p.target.fold("Not client work: choose where it bills first.")(z => s"Bills to ${Words.target(z)}"),
      p.covered.map(c => s"${c.title} [${c.threadId}]: ${Words.totals(c.totals, c.submissions)}").mkString("\n"),
      if p.leftOut.nonEmpty then
        s"Left out: ${p.leftOut.map((t, why) => s"${t.title} [${t.threadId}] ($why)").mkString("; ")}"
      else "",
      if p.history.isEmpty then "Nothing of these threads was sent or settled before."
      else
        "Sent or settled before:\n" + p.history.map(sub =>
          s"- ${Words.submission(sub, stamp(cal))} [${sub.id}]"
        ).mkString("\n")
      ,
      if p.later.nonEmpty then
        s"Not in this draft: ${p.later.size} stretch(es), ${hm(p.later.map(_.window.durationMs).sum)}, stay unbilled."
      else "",
      if blockers.nonEmpty then s"Blocked: ${blockers.mkString("; ")}" else "Ready to send.",
      if !p.summariesAvailable then
        "No model is available to write summaries: lines are your first message in each stretch."
      else "",
      "",
      s"## Windows (${p.lines.size}; ${hm(p.plan.totalMs)} to bill)",
      if windows.isEmpty then "(none)" else windows.mkString("\n"),
      "",
      s"## Shared stretches to settle (${p.plan.undecided.size})",
      if p.plan.undecided.isEmpty then "(none)" else p.plan.undecided.map(b => block(b, None)).mkString("\n"),
      if p.plan.decided.nonEmpty then
        s"\n## Settled (${p.plan.decided.size})\n${p.plan.decided.map((b, a) => block(b, Some(a))).mkString("\n")}"
      else "",
      "",
      "## Zoho entries this would create",
      if entries.isEmpty then "(none)" else entries.mkString("\n"),
      if hand.nonEmpty then
        s"\n## Already in Zoho on these dates, not from bb (check for double billing)\n${hand.mkString("\n")}"
      else "",
      p.handEnteredError.fold("")(e => s"\nCould not read Zoho's existing entries: $e")
    ).filter(_.nonEmpty).mkString("\n")

  def trace(t: ZohoTrace, cal: Calendar): String =
    t match
      case ZohoTrace.TimerRan(s, e, d)      =>
        val end = if cal.dayOf(e) == cal.dayOf(s) then clock(cal)(e) else stamp(cal)(e)
        s"Zoho timer ran ${stamp(cal)(s)} to $end (${hm(e - s)}): $d"
      case ZohoTrace.TimerLeftRunning(s, d) =>
        s"Zoho timer started ${stamp(cal)(s)}, not stopped in this thread (stopped elsewhere; see Zoho for its length): $d"

  def audit(a: Service#Audit): String =
    val cal     = a.calendar
    val sent    =
      a.submission.entries.map(e =>
        s"- ${e.day}: ${hm(e.minutes * 60_000)} (entry ${e.timeEntryId}): ${e.notes.take(120)}"
      )
    val found   = a.insideSentRange.map((t, tr) => s"- ${a.titles.getOrElse(t, t)} [$t]: ${trace(tr, cal)}")
    val pre     = a.preExisting.map(e =>
      s"- ${e.date}: ${hm(e.minutes * 60_000)} ${e.taskName} by ${e.userName}: ${e.notes.take(120)} (${e.billedStatus})"
    )
    val verdict =
      if a.insideSentRange.nonEmpty && a.preExisting.nonEmpty then
        "Likely double billed: the threads ran the Zoho timer inside the sent range, and Zoho has entries on those days that bb did not create. Compare the spans above with the pre-existing entries; undo the submission or strike those stretches and resend."
      else if a.insideSentRange.nonEmpty then
        "The threads ran the Zoho timer inside the sent range, but Zoho shows no other entry on those days for this project: the timer's time may have gone to another project, or was discarded."
      else if a.preExisting.nonEmpty then
        "No Zoho timer call inside the sent range, but Zoho has entries on those days that bb did not create: check them against the sent stretches by hand."
      else "No sign of double billing: no Zoho timer call inside the sent range, and no other Zoho entry on those days."
    Vector(
      s"# Audit of ${a.submission.id}: ${Words.submission(a.submission, stamp(cal))}",
      "",
      "## Sent",
      if sent.isEmpty then "(nothing: marked settled)" else sent.mkString("\n"),
      "",
      "## Zoho timer calls found inside the sent range",
      if found.isEmpty then "(none)" else found.mkString("\n"),
      "",
      "## Zoho entries on those days not made by bb",
      if pre.isEmpty then "(none)" else pre.mkString("\n"),
      a.preExistingError.fold("")(e => s"Could not read Zoho: $e"),
      "",
      verdict
    ).filter(_ != null).mkString("\n")

  def submission(s: SubmissionRecord, cal: Calendar): String =
    s"- ${Words.submission(s, stamp(cal))} [${s.id}]" +
      (if s.billed then s.entries.map(e => s" (entry ${e.timeEntryId})").mkString else "")
