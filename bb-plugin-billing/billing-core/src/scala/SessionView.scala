package billingcore

/** Whether time on a thread can be billed, as the user has set it. A thread never marked is not the same as one marked
  * not client work: the screen tells them apart.
  */
enum Billing:
  case Bills
  case NotClientWork
  case Unmarked

/** What the session screen needs to know of a thread, whatever its billing. */
final case class ThreadInfo(
  id: String,
  title: String,
  parentId: Option[String],
  billing: Billing,
  createdAt: Long,
  archivedAt: Option[Long])

/** One stretch of attention in the session, in the state the ledger gives it. `text` is what to show on it without a
  * click: the summary, else the user's first message.
  */
final case class SessionStretch(window: Window, state: StretchState, text: Option[String]):
  def durationMs: Long = window.durationMs

/** One thing drawn in a thread's own strip: a stretch, or several tiny ones that read as one mark. */
enum Mark:
  case One(stretch: SessionStretch)

  /** Two or more tiny stretches of one thread, close together: the first, then the others in time order. */
  case Cluster(first: SessionStretch, rest: Vector[SessionStretch])

  def members: Vector[SessionStretch] =
    this match
      case One(s)           => Vector(s)
      case Cluster(f, rest) => f +: rest

  def start: Long        = members.head.window.start
  def end: Long          = members.map(_.window.end).max
  def durationMs: Long   = members.map(_.durationMs).sum

/** Where a thread's box begins or ends within the session. */
enum BoxEdge:
  /** The thread was created, or archived, at this time inside the session. */
  case Real(at: Long)

  /** The thread's life goes on past the session's edge. */
  case ClippedAtSession

/** A thread's box over its life inside the session. `own` are its own stretches only, never its children's. */
final case class ThreadBox(
  info: ThreadInfo,
  top: BoxEdge,
  bottom: BoxEdge,
  own: Vector[Mark],
  children: Vector[ThreadBox])

object SessionView:
  /** A stretch shorter than this is tiny. */
  val TinyMs: Long = 2 * 60_000L

  /** Tiny stretches of one thread no further apart than this read as one mark. */
  val ClusterGapMs: Long = 10 * 60_000L

  /** Folds a thread's own stretches, in time order, so that runs of tiny ones close together become clusters. */
  def marks(stretches: Seq[SessionStretch]): Vector[Mark] =
    stretches.sortBy(s => (s.window.start, s.window.end)).foldLeft(Vector.empty[Mark]) { (acc, s) =>
      acc.lastOption match
        case Some(prev) if s.durationMs < TinyMs && prev.members.forall(_.durationMs < TinyMs) &&
              s.window.start - prev.end <= ClusterGapMs =>
          acc.init :+ (prev match
            case Mark.One(f)          => Mark.Cluster(f, Vector(s))
            case Mark.Cluster(f, rest) => Mark.Cluster(f, rest :+ s))
        case _ => acc :+ Mark.One(s)
    }

  /** The thread forest of one session: every thread with a stretch of its own in the session or a descendant that has
    * one, each nested under its parent when the parent appears too. Roots and siblings are in order of first activity.
    */
  def forest(session: Session, threads: Seq[ThreadInfo], stretches: Seq[SessionStretch]): Vector[ThreadBox] =
    val byThread = stretches.groupBy(_.window.threadId)
    val byId     = threads.map(t => t.id -> t).toMap
    val childrenOf = threads.groupBy(_.parentId.filter(byId.contains)).withDefaultValue(Nil)

    def box(t: ThreadInfo): Option[ThreadBox] =
      val own      = marks(byThread.getOrElse(t.id, Nil))
      val children = childrenOf(Some(t.id)).flatMap(box).toVector.sortBy(firstActivity)
      if own.isEmpty && children.isEmpty then None
      else
        val top    = if t.createdAt > session.start then BoxEdge.Real(t.createdAt) else BoxEdge.ClippedAtSession
        val bottom = t.archivedAt.filter(_ < session.end) match
          case Some(at) => BoxEdge.Real(at)
          case None     => BoxEdge.ClippedAtSession
        Some(ThreadBox(t, top, bottom, own, children))

    childrenOf(None).flatMap(box).toVector.sortBy(firstActivity)

  /** When the box, or anything inside it, first has activity. */
  def firstActivity(b: ThreadBox): Long =
    (b.own.map(_.start) ++ b.children.map(firstActivity)).min
