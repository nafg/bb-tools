package billingcore

/** The one state a stretch of a thread's recording is in. A stretch is billed when a submission covers it, and not
  * otherwise; the other cases say why an uncovered stretch is, or is not, offered for a draft.
  */
enum StretchState:
  /** Nothing covers it; it can go into a draft. */
  case Unbilled

  /** The user struck it out: it stays unbilled and out of drafts until put back. */
  case LeftOut

  /** The agent put this time into Zoho by hand in the thread; out of drafts unless the user includes it anyway. */
  case InZohoByHand(trace: ZohoTrace)

  /** Covered by a submission that sent it to Zoho. */
  case Sent(submissionId: String)

  /** Covered by a submission that settled it without billing. */
  case Settled(submissionId: String)

  /** True when a draft may include this stretch. */
  def draftable: Boolean =
    this match
      case Unbilled | InZohoByHand(_)     => true
      case LeftOut | Sent(_) | Settled(_) => false

  /** True when "everything unbilled" takes it without being asked. */
  def unbilledByDefault: Boolean = this == Unbilled

/** A span of a thread's recording that one submission covers. */
final case class CoveredBy(submissionId: String, billed: Boolean, span: Span)

/** One stretch of the ledger: a piece of the recording and its state. */
final case class LedgerStretch(window: Window, state: StretchState):
  def key: String = window.key

object Ledger:
  /** The ledger's stretches for the given stretches of recording: each is cut where a submission's coverage or a
    * strike-out begins or ends, so every piece has exactly one state whatever the windows were cut into. `traces` is
    * the Zoho timer evidence per thread.
    */
  def of(
    windows: Seq[Window],
    covering: Seq[CoveredBy],
    struck: Coverage,
    traces: Map[String, Seq[ZohoTrace]]): Vector[LedgerStretch] =
    windows.toVector.flatMap { w =>
      val spans = covering.filter(c => c.span.threadId == w.threadId && c.span.start < w.end && c.span.end > w.start)
      val struckEdges = struck.spans.getOrElse(w.threadId, Vector.empty).flatMap(s => Vector(s.start, s.end))
      val cuts        = (Vector(w.start, w.end) ++ spans.flatMap(c => Vector(c.span.start, c.span.end)) ++ struckEdges)
        .filter(t => t >= w.start && t <= w.end).distinct.sorted
      cuts.zip(cuts.drop(1)).map { (s, e) =>
        val piece = w.copy(start = s, end = e)
        val state = spans.find(c => c.span.start <= s && c.span.end >= e) match
          case Some(c) if c.billed => StretchState.Sent(c.submissionId)
          case Some(c)             => StretchState.Settled(c.submissionId)
          case None                =>
            if struck.coversAll(w.threadId, s, e) then StretchState.LeftOut
            else
              traces.getOrElse(w.threadId, Nil).find(_.touches(s, e)) match
                case Some(t) => StretchState.InZohoByHand(t)
                case None    => StretchState.Unbilled
        LedgerStretch(piece, state)
      }
    }.sortBy(l => (l.window.start, l.window.threadId))

  /** The recording by state. In-Zoho-by-hand stretches are unbilled too, and counted apart. */
  def totals(stretches: Seq[LedgerStretch]): Totals =
    stretches.foldLeft(Totals.zero) { (t, l) =>
      val ms = l.window.durationMs
      l.state match
        case StretchState.Unbilled        => t.copy(unbilledMs = t.unbilledMs + ms)
        case StretchState.InZohoByHand(_) =>
          t.copy(unbilledMs = t.unbilledMs + ms, inZohoByHandMs = t.inZohoByHandMs + ms)
        case StretchState.LeftOut         => t.copy(leftOutMs = t.leftOutMs + ms)
        case StretchState.Sent(_)         => t.copy(sentMs = t.sentMs + ms)
        case StretchState.Settled(_)      => t.copy(settledMs = t.settledMs + ms)
    }

  /** The keys of every stretch "everything unbilled" takes: the default draft. */
  def unbilledKeys(stretches: Seq[LedgerStretch]): Set[String] =
    stretches.filter(_.state.unbilledByDefault).map(_.key).toSet

/** How a caller picks the stretches of a draft: the keys it ticked, or everything unbilled. */
enum DraftPick:
  case Keys(keys: Set[String])
  case AllUnbilled

  def keysOn(stretches: Seq[LedgerStretch]): Set[String] =
    this match
      case Keys(keys)  => keys
      case AllUnbilled => Ledger.unbilledKeys(stretches)

  /** `["k1", "k2"]` or `"all"`. */
  def encode: Wire =
    this match
      case Keys(keys)  => Wire.strings(keys)
      case AllUnbilled => Wire.Str("all")

object DraftPick:
  def decode(value: Wire): Option[DraftPick] =
    value match
      case Wire.Str("all") => Some(AllUnbilled)
      case Wire.Arr(items) => Some(Keys(items.flatMap(_.asStr).toSet))
      case _               => None
