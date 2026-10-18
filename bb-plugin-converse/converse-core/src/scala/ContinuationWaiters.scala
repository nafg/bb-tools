package conversecore

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

/** One pending agent request for more speech per thread. A wait ends either
  * because the user continued speaking (true) or not (false); the speech
  * itself reaches the thread as a message. The transport owns timeout
  * scheduling; this class only makes continuing versus expiring atomic.
  */
final class ContinuationWaiters:
  private val pending = mutable.Map.empty[String, Promise[Boolean]]

  def begin(threadId: String): Future[Boolean] =
    pending.getOrElseUpdate(threadId, Promise[Boolean]()).future

  def isWaiting(threadId: String): Boolean = pending.contains(threadId)

  /** Ends the thread's wait with "the user continued"; false if none was pending. */
  def continued(threadId: String): Boolean =
    pending.remove(threadId).exists(_.trySuccess(true))

  def expire(threadId: String): Boolean =
    pending.remove(threadId).exists(_.trySuccess(false))

  def expireAll(): Unit =
    val promises = pending.values.toList
    pending.clear()
    promises.foreach(_.trySuccess(false))
