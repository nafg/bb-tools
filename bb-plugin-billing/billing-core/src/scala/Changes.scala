package billingcore

/** Something the plugin changed, or learned had changed, that a result computed earlier may depend on. Each case names
  * exactly what it touches, so whoever keeps a result can tell whether it still holds (see [[Inputs.affectedBy]]).
  */
enum Change:
  /** A shared stretch was decided, or its decision undone. */
  case Decided(key: BlockKey)

  /** A window of the thread was left out, or put back. */
  case LeftOut(threadId: String)

  /** A submission covering these threads was made or undone. */
  case Submitted(threadIds: Set[String])

  /** The notes of one of the root thread's dated entries were edited or restored. */
  case NotesEdited(rootThreadId: String)

  /** Lines were written for stretches of these threads, or day notes for drafts holding them. */
  case Summarized(threadIds: Set[String])

  /** Where some thread or bb project bills changed. A thread follows its parent and its bb project, so this can move
    * any thread; nothing narrower can be said.
    */
  case BillsTo

  /** Entries made in Zoho outside this plugin changed in the Zoho project. */
  case ZohoHandEntries(projectId: String)

  /** The thread was archived or unarchived. */
  case Archived(threadId: String)

  /** The plugin started: everything kept before is gone. */
  case Restarted

  def encode: Wire.Obj =
    this match
      case Decided(key)        => Wire.obj("kind" -> Wire.Str("decided"), "key" -> Wire.Str(key.text))
      case LeftOut(t)          => Wire.obj("kind" -> Wire.Str("leftOut"), "threadId" -> Wire.Str(t))
      case Submitted(ts)       => Wire.obj("kind" -> Wire.Str("submitted"), "threadIds" -> Wire.strings(ts.toSeq.sorted))
      case NotesEdited(root)   => Wire.obj("kind" -> Wire.Str("notesEdited"), "threadId" -> Wire.Str(root))
      case Summarized(ts)      => Wire.obj("kind" -> Wire.Str("summarized"), "threadIds" -> Wire.strings(ts.toSeq.sorted))
      case BillsTo             => Wire.obj("kind" -> Wire.Str("billsTo"))
      case ZohoHandEntries(p)  => Wire.obj("kind" -> Wire.Str("zohoHandEntries"), "projectId" -> Wire.Str(p))
      case Archived(t)         => Wire.obj("kind" -> Wire.Str("archived"), "threadId" -> Wire.Str(t))
      case Restarted           => Wire.obj("kind" -> Wire.Str("restarted"))

object Change:
  def decode(value: Wire): Option[Change] =
    def str(name: String)  = value.field(name).flatMap(_.asStr)
    def strs(name: String) = value.field(name).flatMap(_.asArr).map(_.flatMap(_.asStr).toSet)
    str("kind") match
      case Some("decided")         => str("key").flatMap(BlockKey.parse).map(Decided.apply)
      case Some("leftOut")         => str("threadId").map(LeftOut.apply)
      case Some("submitted")       => strs("threadIds").map(Submitted.apply)
      case Some("notesEdited")     => str("threadId").map(NotesEdited.apply)
      case Some("summarized")      => strs("threadIds").map(Summarized.apply)
      case Some("billsTo")         => Some(BillsTo)
      case Some("zohoHandEntries") => str("projectId").map(ZohoHandEntries.apply)
      case Some("archived")        => str("threadId").map(Archived.apply)
      case Some("restarted")       => Some(Restarted)
      case _                       => None

/** What a result was computed from: the threads whose recording, stretches, decisions or submissions it read, and the
  * Zoho projects whose entries it read.
  */
final case class Inputs(threadIds: Set[String], zohoProjectIds: Set[String]):
  /** True when the result may no longer hold after `change`. */
  def affectedBy(change: Change): Boolean =
    change match
      case Change.Decided(key)          => key.threadIds.exists(threadIds)
      case Change.LeftOut(t)            => threadIds(t)
      case Change.Submitted(ts)         => ts.exists(threadIds)
      case Change.NotesEdited(root)     => threadIds(root)
      case Change.Summarized(ts)        => ts.exists(threadIds)
      case Change.BillsTo               => true
      case Change.ZohoHandEntries(p)    => zohoProjectIds(p)
      case Change.Archived(t)           => threadIds(t)
      case Change.Restarted             => true

  def affectedByAny(changes: Iterable[Change]): Boolean = changes.exists(affectedBy)
