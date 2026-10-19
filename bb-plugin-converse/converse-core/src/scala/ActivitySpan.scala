package conversecore

/** What kind of activity a span records: the user speaking, or a reply being
  * played to them.
  */
enum ActivityKind(val code: String):
  case Speech   extends ActivityKind("speech")
  case Playback extends ActivityKind("playback")

/** Where a span came from: the voice session, a message replayed on request
  * from its action, or a recording stored before speech times were stamped,
  * whose span is approximate.
  */
enum ActivitySource(val code: String):
  case Session  extends ActivitySource("session")
  case Replay   extends ActivitySource("replay")
  case Backfill extends ActivitySource("backfill")

object ActivitySource:
  def fromCode(code: String): Option[ActivitySource] = values.find(_.code == code)

  /** The sources the browser reports spans under. */
  val Reported: Set[ActivitySource] = Set(Session, Replay)
