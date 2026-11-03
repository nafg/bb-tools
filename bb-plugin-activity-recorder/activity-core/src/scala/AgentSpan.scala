package activitycore

/** Who started an agent's turn, as far as keeping a session open is concerned. */
enum Starter:
  /** The user, writing to the thread (or spawning it). */
  case User
  /** One or more other threads: a parent that spawned this thread, a thread that messaged it, or a child whose
    * outcome woke it. The turn counts toward a session when any of them is already in it.
    */
  case Threads(ids: Set[String])

  def isUser: Boolean = this match
    case User => true
    case _    => false

  /** The threads that started the turn; none for the user. */
  def threadIds: Set[String] = this match
    case Threads(ids) => ids
    case User         => Set.empty

/** One turn of an agent that the user set going, directly or through the threads working for them: a turn started by
  * a scheduled automation, a subscription or one of bb's own workers is not one. `[start, end)` in epoch
  * milliseconds; a turn still running ends at the time it was read.
  */
final case class AgentSpan(threadId: String, start: Long, end: Long, startedBy: Starter)

/** What a turn request says about who started the turn, as the recorder reads it from the thread's events. */
final case class TurnRequest(
  /** `user`, `agent` or `system`, as bb stamps the request. */
  initiator: String,
  /** The thread that sent the message, when one did. */
  senderThreadId: Option[String],
  /** bb's label for a system message, such as `child-completed`; empty for an ordinary message. */
  systemMessageKind: String,
  /** The thread a system message is about, when it names one. */
  subjectThreadIds: Set[String],
  /** The text of the message. */
  text: String,
  /** The request that started the thread, for which `parentThreadId` is the thread that spawned it. */
  startsThread: Boolean,
  parentThreadId: Option[String]
)

object Starter:
  /** The system messages that wake a thread because a thread it started has finished, failed or needs help. */
  private val ChildKinds = Set("child-completed", "child-failed", "child-needs-attention", "child-interrupted", "child-outcome-batch")

  private val ThreadMention = """@thread:(thr_[A-Za-z0-9]+)""".r

  /** The threads a system message names in its text, for the messages that carry no structured subject. */
  def mentionedThreads(text: String): Set[String] = ThreadMention.findAllMatchIn(text).map(_.group(1)).toSet

  /** Who started the turn this request began, or none when it is not work the user set going.
    *
    * An automation or subscription arrives tagged as the user, so its `[bb ` prefix is the only sign of it. The first
    * request of a thread someone else spawned is stamped as the user's too, so it is told apart by the thread's
    * parent. A thread nobody spawned and the user did not write to (bb's own workers are hidden threads, which the
    * caller leaves out before asking) has no starter.
    */
  def of(request: TurnRequest): Option[Starter] =
    val text = request.text.trim
    request.senderThreadId.filter(_.nonEmpty) match
      case Some(sender)                                                           => Some(Threads(Set(sender)))
      case None if request.initiator == "system" && ChildKinds(request.systemMessageKind) =>
        Some(request.subjectThreadIds ++ mentionedThreads(text)).filter(_.nonEmpty).map(Threads(_))
      case None if request.initiator == "user" && !text.startsWith("[bb ")        =>
        if request.startsThread then Some(request.parentThreadId.filter(_.nonEmpty).fold(User)(parent => Threads(Set(parent))))
        else Some(User)
      case None                                                                   => None

  /** The text a store keeps for who started a turn: empty for a turn that is not the user's work, `user`, or
    * `threads:` and the thread ids.
    */
  def encode(starter: Option[Starter]): String = starter match
    case None               => ""
    case Some(User)         => "user"
    case Some(Threads(ids)) => "threads:" + ids.toVector.sorted.mkString(",")

  /** Reads what [[encode]] wrote; text it does not recognize is no starter. */
  def decode(text: String): Option[Starter] =
    if text == "user" then Some(User)
    else if text.startsWith("threads:") then Some(text.stripPrefix("threads:").split(",").filter(_.nonEmpty).toSet).filter(_.nonEmpty).map(Threads(_))
    else None

