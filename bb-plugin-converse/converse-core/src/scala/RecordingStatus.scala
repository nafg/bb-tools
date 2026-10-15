package conversecore

/** Where a stored recording is on its way to becoming a message. A recording
  * is stored before anything else is done with it, so every state here keeps
  * the audio; Failed ones can be retried.
  */
enum RecordingStatus:
  /** Stored and waiting to be transcribed and delivered. */
  case Received

  /** Being handed to the thread. A restart in this state cannot tell whether
    * the message arrived, so it is not resent automatically.
    */
  case Sending
  case Sent

  /** Transcribed to nothing; there was no message to deliver. */
  case Empty
  case Failed(reason: String)

  /** The value stored in the status column. */
  def code: String = this match
    case Received  => "received"
    case Sending   => "sending"
    case Sent      => "sent"
    case Empty     => "empty"
    case Failed(_) => "failed"

object RecordingStatus:
  val InterruptedWhileSending =
    "the plugin restarted while this was being delivered; it may or may not have reached the thread"

  /** Reads the status and last-error columns. Rows stored before statuses
    * were tracked have no status.
    */
  def fromColumns(code: String | Null, lastError: String | Null): Option[RecordingStatus] =
    Option(code).map {
      case "received" => Received
      case "sending"  => Sending
      case "sent"     => Sent
      case "empty"    => Empty
      case "failed"   => Failed(Option(lastError).getOrElse(""))
      case other      => throw new IllegalArgumentException(s"unknown recording status: $other")
    }

  /** What a recording found in `status` when the plugin starts should become:
    * a received one is processed again; one cut off mid-delivery is marked
    * failed rather than risk delivering it twice.
    */
  def afterRestart(status: RecordingStatus): RecordingStatus = status match
    case Sending => Failed(InterruptedWhileSending)
    case other   => other
