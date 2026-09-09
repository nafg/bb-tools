package conversecore

/** The voice session the next frontend generation brings back after the
  * current one is unloaded underneath a live session, as on a plugin reload.
  * Stored by the frontend when a session starts; whether it outlives the
  * session's end depends on why the session ended.
  */
object VoiceSessionRestore:
  final case class Intent(sessionId: String, threadId: Option[String], startOptionsJson: String)

  enum SessionEnd:
    case StoppedByUser, Superseded, Failed, TakenOverByAnotherWindow, FrontendUnloaded

    def notifiesServer: Boolean = this match
      case TakenOverByAnotherWindow | FrontendUnloaded => false
      case StoppedByUser | Superseded | Failed         => true

    def keepsIntent: Boolean = this == FrontendUnloaded

  private val FieldSeparator = "\n"

  def encode(intent: Intent): String =
    List(intent.sessionId, intent.threadId.getOrElse(""), intent.startOptionsJson).mkString(FieldSeparator)

  def decode(text: String): Option[Intent] =
    text.split(FieldSeparator, 3) match
      case Array(sessionId, threadId, startOptionsJson) if sessionId.nonEmpty =>
        Some(Intent(sessionId, Option(threadId).filter(_.nonEmpty), startOptionsJson))
      case _ => None
