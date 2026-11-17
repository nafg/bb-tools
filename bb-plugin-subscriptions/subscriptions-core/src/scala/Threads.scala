package subscriptionscore

import subscriptionsource.{CheckResult, Recognition, Text}

/** Another bb thread being archived or deleted. Written `bb:thread/<id>/archived`: bb object type, then id, then the
  * event, since other bb objects, and other things about a thread, could be watched too.
  */
final case class ThreadArchived(threadId: String):
  def url: String   = s"bb:thread/$threadId/archived"
  def label: String = s"archiving of @thread:$threadId"

object ThreadArchived:
  private val BbThread = """^bb:thread/(thr_[a-z0-9]+)(?:/([a-z]+))?$""".r

  val supported = "another bb thread being archived, as bb:thread/thr_…/archived"

  def parse(input: String): Option[ThreadArchived] = input.trim match
    case BbThread(id, "archived") => Some(ThreadArchived(id))
    case _                        => None

  def recognize(input: String): Recognition = input.trim match
    case BbThread(id, "archived") => Recognition.Recognized(ThreadArchived(id).url, ThreadArchived(id).label)
    case BbThread(id, _)          => Recognition.Invalid(s"bb:thread/$id/archived is the only supported thread event")
    case _                        => Recognition.NotMine

/** Whether a bb thread is still open. */
enum ThreadLife:
  case Open, Archived, Deleted

object ThreadWatch:
  def encode(life: ThreadLife): String = s"thread $life"

  def decode(s: String): Either[String, ThreadLife] = s.split(' ').toList match
    case "thread" :: life :: Nil => scala.util.Try(ThreadLife.valueOf(life)).toEither.left.map(_.getMessage)
    case _                       => Left(s"unreadable stored state: $s")

  val promise =
    "This thread will get a message, with that thread's last reply, when it is archived or deleted; then the " +
      "subscription ends."

  /** What to tell a subscriber about `target` since `previous`, and what to remember. `title` and `lastReply` are
    * the watched thread's, for the message that reports it finishing. A first check reports nothing.
    */
  def check(
      target: ThreadArchived,
      previous: Option[ThreadLife],
      now: ThreadLife,
      title: String,
      lastReply: Option[String]
  ): CheckResult =
    val messages = (previous, now) match
      case (Some(ThreadLife.Open), life) if life != ThreadLife.Open =>
        val how   = if life == ThreadLife.Deleted then "was deleted" else "was archived"
        val quote = lastReply.map(Text.excerpt).filter(_.nonEmpty).fold("")(r => "\nIts last reply:\n" + Text.quoted(r))
        List(s"Thread \"$title\" (@thread:${target.threadId}) $how.$quote")
      case _ => Nil
    CheckResult(
      state = encode(now),
      label = target.label,
      summary = now.toString.toLowerCase,
      promise = promise,
      link = s"@thread:${target.threadId}",
      messages = messages,
      finished = now != ThreadLife.Open
    )
