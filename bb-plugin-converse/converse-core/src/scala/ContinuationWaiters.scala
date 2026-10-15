package conversecore

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

/** One pending agent request for more speech per thread. The transport owns
  * timeout scheduling; this class only makes delivery versus expiration
  * atomic so an utterance can never be both consumed and steered.
  */
final class ContinuationWaiters:
  private val pending = mutable.Map.empty[String, Promise[Option[String]]]

  def begin(threadId: String): Future[Option[String]] =
    pending.getOrElseUpdate(threadId, Promise[Option[String]]()).future

  def isWaiting(threadId: String): Boolean = pending.contains(threadId)

  def deliver(threadId: String, text: String): Boolean =
    pending.remove(threadId).exists(_.trySuccess(Some(text)))

  def expire(threadId: String): Boolean =
    pending.remove(threadId).exists(_.trySuccess(None))

  def expireAll(): Unit =
    val promises = pending.values.toList
    pending.clear()
    promises.foreach(_.trySuccess(None))
