package conversecore

/** A voice session's recordings on their way to becoming messages, as the
  * page sees them: waiting to be uploaded, being uploaded (one at a time, in
  * order), or stored by the backend with their outcome still to come. Each
  * event is a transition that holds in whatever order the events arrive; in
  * particular a recording's outcome can arrive before the backend's reply
  * confirming that it stored it.
  */
final case class Outbox[P] private (
    waiting: Vector[Outbox.Entry[P]],
    uploading: Option[Outbox.Entry[P]],
    /** Recordings handed over whose outcome has not come back. */
    unsettled: Set[String]
):
  /** Adds a recording to upload after those already waiting. */
  def enqueue(id: String, payload: P): Outbox[P] =
    copy(waiting = waiting :+ Outbox.Entry(id, payload), unsettled = unsettled + id)

  /** The next recording to upload and the outbox with it uploading, unless
    * one is uploading already or none is waiting.
    */
  def startNext: Option[(Outbox.Entry[P], Outbox[P])] =
    if uploading.isDefined then None
    else waiting.headOption.map(next => (next, copy(waiting = waiting.tail, uploading = Some(next))))

  /** The backend confirmed storing a recording. */
  def stored(id: String): Outbox[P] =
    if uploading.exists(_.id == id) then copy(uploading = None) else this

  /** The backend finished with a recording: delivered it, found nothing said,
    * or gave up on it.
    */
  def settled(id: String): Outbox[P] = copy(unsettled = unsettled - id)

  /** Whether a recording is not yet stored, so replies wait for it. */
  def holdsUnstored: Boolean = uploading.isDefined || waiting.nonEmpty

  /** Whether any recording is still on its way to becoming a message. */
  def busy: Boolean = holdsUnstored || unsettled.nonEmpty

object Outbox:
  final case class Entry[P](id: String, payload: P)

  def empty[P]: Outbox[P] = Outbox(Vector.empty, None, Set.empty)
