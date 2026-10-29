package billingcore

/** Where a thread stands with billing: the one word the page, the CLI and the agent all use for it. */
enum Standing(val label: String):
  case NotClientWork   extends Standing("not client work")

  /** Recorded time, and nothing on the thread, above it, or in its bb project says whether it is client work. */
  case Undecided       extends Standing("not chosen whether client work")
  case NothingRecorded extends Standing("nothing recorded")

  /** Billed before, and nothing recorded since. */
  case Settled          extends Standing("settled")
  case OpenUnbilled     extends Standing("open, unbilled")
  case OpenPartlyBilled extends Standing("open, partly billed")

  /** Archived with recorded time no submission covers: it needs a decision, bill or settle. */
  case ArchivedToSettle extends Standing("archived, to settle")

  /** True when the thread waits on the user: archived client work with unbilled time, or unbilled time nobody has said
    * is client work or not.
    */
  def needsDecision: Boolean = this == ArchivedToSettle || this == Undecided

object Standing:
  def of(billable: Boolean, decided: Boolean, unbilledMs: Long, archived: Boolean, billedBefore: Boolean): Standing =
    if !billable then if !decided && unbilledMs > 0 then Undecided else NotClientWork
    else if unbilledMs <= 0 then if billedBefore then Settled else NothingRecorded
    else if archived then ArchivedToSettle
    else if billedBefore then OpenPartlyBilled
    else OpenUnbilled
