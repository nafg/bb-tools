package activitycore

/** What a recorded signal is evidence of. */
enum SignalKind(val name: String):
  /** The user spoke to the thread (a converse utterance). */
  case Speech extends SignalKind("speech")
  /** The thread's reply was read aloud to the user. */
  case Playback extends SignalKind("playback")
  /** The user sent the thread a typed message. */
  case Message extends SignalKind("message")
  /** The thread's agent ran a turn. */
  case Turn extends SignalKind("turn")
  /** The thread's pane was focused and visible, with recent user input. */
  case View extends SignalKind("view")

object SignalKind:
  /** The kinds that are the user's own attention. `Turn` is the agent's time. */
  val Attention: Set[SignalKind] = Set(Speech, Playback, Message, View)

  def parse(name: String): Option[SignalKind] = values.find(_.name == name)

/** One recorded span of activity on a thread, in epoch milliseconds. A point
  * signal has `start == end`. `source` names what produced it: `converse`
  * (a voice session), `converse-replay` (a reply replayed from its message),
  * `converse-backfill` (converse's approximation for speech before spans
  * were recorded), `bb`, or `app`.
  */
final case class Signal(id: String, threadId: String, kind: SignalKind, start: Long, end: Long, source: String)

/** A stretch of time the user was active on one thread, in epoch
  * milliseconds, with the kinds of signal that make it up. Intervals of
  * different threads overlap freely.
  */
final case class Interval(threadId: String, start: Long, end: Long, kinds: Set[SignalKind])
