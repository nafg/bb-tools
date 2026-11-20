package agentmailcore

/** Email addresses as message fields hold them: bare (`a@b.com`) or after a display name (`Name <a@b.com>`). */
object EmailAddress:
  /** The address in `field`, lowercased; None when the field holds no address. */
  def bare(field: String): Option[String] =
    val address = field.lastIndexOf('<') match
      case -1 => field
      case start =>
        val end = field.indexOf('>', start)
        if end < 0 then field.substring(start + 1) else field.substring(start + 1, end)
    Some(address.trim.toLowerCase).filter { a =>
      val at = a.lastIndexOf('@')
      at > 0 && at < a.length - 1
    }

  /** The domain of a bare address. */
  def domain(address: String): String = address.substring(address.lastIndexOf('@') + 1)

  /** Whether the domains are equal or one is a subdomain of the other, as tickets.jivetel.com is of jivetel.com.
    * Sibling subdomains are not related.
    */
  def sameOrSubdomain(a: String, b: String): Boolean =
    a == b || a.endsWith("." + b) || b.endsWith("." + a)

/** An email thread the plugin maps to a bb thread. `correspondents` are the bare addresses it exchanged mail with,
  * other than the plugin's own inbox. A greater `createdAt` is a later thread.
  */
final case class KnownEmailThread(
    agentmailThreadId: String,
    bbThreadId: String,
    subject: String,
    correspondents: Set[String],
    createdAt: Long
)

/** Matching an inbound message to a known email thread by its subject and sender, for messages AgentMail did not
  * file in a known thread.
  */
object UnthreadedReply:
  /** A known subject with fewer identifying words than this is too common to identify a conversation by. */
  val MinimumSubjectWords = 3

  private val ReplyMarkers = Set("re", "fw", "fwd")

  /** The subject's words, lowercased; anything but letters and digits separates words. */
  private def words(subject: String): Vector[String] =
    subject.toLowerCase.map(c => if Character.isLetterOrDigit(c) then c else ' ').split(' ').toVector.filter(_.nonEmpty)

  /** The known thread `subject` and `sender` most likely continue: one whose subject's words, without leading reply
    * and forward markers, number at least [[MinimumSubjectWords]] and appear in a row among `subject`'s words, and
    * one of whose correspondents' domains is `sender`'s domain or a subdomain or parent of it. Of several, the one
    * with the most subject words wins, then the latest.
    */
  def findOriginal(subject: String, sender: String, known: Seq[KnownEmailThread]): Option[KnownEmailThread] =
    EmailAddress.bare(sender).map(EmailAddress.domain).flatMap { senderDomain =>
      val inboundWords = words(subject)
      known
        .map(thread => (thread, words(thread.subject).dropWhile(ReplyMarkers)))
        .filter { (thread, identifying) =>
          identifying.size >= MinimumSubjectWords &&
          inboundWords.containsSlice(identifying) &&
          thread.correspondents.exists(c => EmailAddress.sameOrSubdomain(EmailAddress.domain(c), senderDomain))
        }
        .maxByOption((thread, identifying) => (identifying.size, thread.createdAt))
        .map(_._1)
    }
