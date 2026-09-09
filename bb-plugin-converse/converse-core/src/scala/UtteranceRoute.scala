package conversecore

/** Where an utterance goes once captured, fixed at capture start so later
  * navigation cannot redirect it.
  */
enum UtteranceRoute:
  case ToThread(threadId: String)
  case NewThread(projectId: String)

  /** The thread that will receive the utterance; None until a new one exists. */
  def existingThreadId: Option[String] = this match
    case ToThread(id) => Some(id)
    case NewThread(_) => None

object UtteranceRoute:
  /** The New thread screen is in view; `projectId` is the project selected
    * there, None while no project is selected.
    */
  final case class ComposeSurface(projectId: Option[String])

  def latch(
      composeSurface: Option[ComposeSurface],
      viewedThreadId: Option[String],
      sessionThreadId: Option[String]
  ): Option[UtteranceRoute] =
    composeSurface match
      case Some(compose) => compose.projectId.filter(_.nonEmpty).map(NewThread.apply)
      case None          => viewedThreadId.orElse(sessionThreadId).filter(_.nonEmpty).map(ToThread.apply)
