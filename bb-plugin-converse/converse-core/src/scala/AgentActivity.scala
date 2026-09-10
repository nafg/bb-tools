package conversecore

/** What the agent is doing while a voice session waits on it. */
enum AgentActivity(val signalName: String):
  /** The model is producing reasoning text. */
  case Reasoning extends AgentActivity("reasoning")
  /** A tool call, command, file read, search, or similar is running. */
  case Tool extends AgentActivity("tool")
  /** The model is producing the reply text. */
  case Generating extends AgentActivity("generating")
  /** The agent asked for more speech and is listening for the user to continue. */
  case AwaitingSpeech extends AgentActivity("awaiting-speech")

object AgentActivity:
  def fromSignalName(name: String): Option[AgentActivity] = values.find(_.signalName == name)

/** One thread event as far as the activity is concerned. */
enum TurnEvent:
  case TurnStarted
  case TurnCompleted
  case ItemStarted(itemId: String, itemKind: String)
  case ItemCompleted(itemId: String)

/** The activity a thread's turn events say the agent is in. Items started but
  * not yet completed are tracked most recent first; the activity is that of
  * the most recently started open item that has one. With nothing open the
  * agent is back to reasoning about its next step. Events that say nothing
  * about the activity (an item kind with none, a completion leaving only such
  * items open) keep the previous one.
  */
final case class TurnActivityTracker(openItems: List[(String, String)], activity: Option[AgentActivity]):
  def apply(event: TurnEvent): TurnActivityTracker = event match
    case TurnEvent.TurnStarted   => TurnActivityTracker(Nil, Some(AgentActivity.Reasoning))
    case TurnEvent.TurnCompleted => TurnActivityTracker(Nil, None)
    case TurnEvent.ItemStarted(itemId, itemKind) =>
      withOpen((itemId, itemKind) :: openItems.filterNot(_._1 == itemId))
    case TurnEvent.ItemCompleted(itemId) =>
      withOpen(openItems.filterNot(_._1 == itemId))

  private def withOpen(open: List[(String, String)]): TurnActivityTracker =
    val fromOpenItems = open.iterator.flatMap((_, kind) => TurnActivityTracker.ofItemKind(kind)).nextOption()
    val betweenItems  = if open.isEmpty && activity.isDefined then Some(AgentActivity.Reasoning) else activity
    TurnActivityTracker(open, fromOpenItems.orElse(betweenItems))

object TurnActivityTracker:
  val initial: TurnActivityTracker = TurnActivityTracker(Nil, None)

  def ofItemKind(itemKind: String): Option[AgentActivity] = itemKind match
    case "reasoning"    => Some(AgentActivity.Reasoning)
    case "agentMessage" => Some(AgentActivity.Generating)
    case "toolCall" | "commandExecution" | "fileChange" | "fileRead" | "webSearch" | "webFetch" | "imageView" |
        "search" =>
      Some(AgentActivity.Tool)
    case _ => None

  def after(events: Iterable[TurnEvent]): TurnActivityTracker = events.foldLeft(initial)(_(_))
