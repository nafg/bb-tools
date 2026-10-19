package conversecore

/** What kind of activity a span records: the user speaking, or a reply being
  * played to them.
  */
enum ActivityKind(val code: String):
  case Speech   extends ActivityKind("speech")
  case Playback extends ActivityKind("playback")

/** Where a span came from: the voice session, or a message replayed on
  * request from its action.
  */
enum ActivitySource(val code: String):
  case Session extends ActivitySource("session")
  case Replay  extends ActivitySource("replay")

object ActivitySource:
  def fromCode(code: String): Option[ActivitySource] = values.find(_.code == code)
