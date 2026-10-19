package goalscore

/** The six goals a thread can serve. `letter` is what is stored and typed. */
enum Goal(val letter: String, val title: String):
  case B extends Goal("B", "Billable client work")
  case C extends Goal("C", "Client work, not billable")
  case P extends Goal("P", "Product or future income")
  case S extends Goal("S", "Sanity")
  case M extends Goal("M", "Maintainer obligations")
  case F extends Goal("F", "Fun")

object Goal:
  def parse(text: String): Option[Goal] =
    val wanted = text.trim.toUpperCase
    Goal.values.find(_.letter == wanted)

enum LabelSource:
  case Own
  case Inherited(fromThreadId: String)

/** The goal a thread is read as serving, and where that reading came from. `value` is None when the goal was set without one.
  */
final case class Effective(goal: Goal, value: Option[Int], source: LabelSource)

/** What one thread stores in the goals metadata namespace. A `value` without a `goal` is kept but does not label the thread.
  */
final case class ThreadLabels(goal: Option[Goal], value: Option[Int], blockedBy: List[String]):
  def ownLabel: Option[Effective] = goal.map(Effective(_, value, LabelSource.Own))

object ThreadLabels:
  val Empty: ThreadLabels = ThreadLabels(None, None, Nil)

  private val ThreadIdPattern = "^thr_[a-z0-9]+$".r
  private val None_           = "none"

  /** Reads a stored namespace without failing: a field that is not what the plugin would have written is treated as absent. Values arrive
    * as the JSON numbers they were stored as.
    */
  def fromStored(goal: Option[Any], value: Option[Any], blockedBy: Option[Seq[Any]]): ThreadLabels =
    ThreadLabels(
      goal = goal.collect { case s: String => s }.flatMap(Goal.parse),
      value = value.collect { case n: Double => n }.map(_.toInt).filter(v => v >= 1 && v <= 3),
      blockedBy = blockedBy.getOrElse(Nil).collect { case s: String if ThreadIdPattern.matches(s) => s }.distinct.toList
    )

  /** A goal as typed by a person or an agent: a letter, or "none" to clear. */
  def parseGoalWrite(text: String): Either[String, Option[Goal]] =
    val trimmed = text.trim
    if trimmed.isEmpty || trimmed.equalsIgnoreCase(None_) then Right(None)
    else Goal.parse(trimmed).map(Some(_)).toRight(s"unknown goal '$trimmed'; expected one of ${Goal.values.map(_.letter).mkString(", ")}")

  /** A value as typed: 1 to 3, or "none" to clear. */
  def parseValueWrite(text: String): Either[String, Option[Int]] =
    val trimmed = text.trim
    if trimmed.isEmpty || trimmed.equalsIgnoreCase(None_) then Right(None)
    else trimmed.toIntOption.filter(v => v >= 1 && v <= 3).map(Some(_)).toRight(s"value must be 1, 2, or 3, got '$trimmed'")

  def parseThreadId(text: String): Either[String, String] =
    val trimmed = text.trim
    if ThreadIdPattern.matches(trimmed) then Right(trimmed) else Left("not a thread id")

object Inheritance:
  /** The label a thread is read as having: its own, else that of the nearest ancestor with a goal (`ancestors` lists each thread's
    * ancestors from its parent upward), else none. A value is only ever read from the thread the goal came from.
    */
  def effective(threadId: String, own: Map[String, ThreadLabels], ancestors: Map[String, List[String]]): Option[Effective] =
    own.get(threadId).flatMap(_.ownLabel).orElse {
      ancestors
        .getOrElse(threadId, Nil)
        .iterator
        .flatMap(id => own.get(id).flatMap(_.goal).map(g => Effective(g, own(id).value, LabelSource.Inherited(id))))
        .nextOption()
    }

/** A label a blocked thread passes to the thread blocking it. */
final case class Lent(fromThreadId: String, goal: Goal, value: Option[Int])

object Lending:
  /** How a value ranks for ordering: 1 is the biggest lever. An unvalued goal ranks in the middle, but is still displayed as unvalued.
    */
  def rank(value: Option[Int]): Int = value.getOrElse(2)

  /** Which labels each blocker receives. Every open, labeled thread lends to each thread reachable along `blockedBy` edges (a blocked
    * thread's blockers, their blockers, and so on), never to itself; cycles are walked once.
    */
  def lent(blockedBy: Map[String, List[String]], effective: String => Option[Effective], open: Set[String]): Map[String, List[Lent]] =
    val received = scala.collection.mutable.Map.empty[String, List[Lent]]
    for
      lender <- open.toList.sorted
      label  <- effective(lender)
    do
      val gift                     = Lent(lender, label.goal, label.value)
      val visited                  = scala.collection.mutable.Set(lender)
      def walk(from: String): Unit =
        for blocker <- blockedBy.getOrElse(from, Nil) if visited.add(blocker) do
          received.update(blocker, received.getOrElse(blocker, Nil) :+ gift)
          walk(blocker)
      walk(lender)
    received.toMap

  /** The best rank among a thread's own label and what it was lent; None when it has neither.
    */
  def importance(own: Option[Effective], lent: List[Lent]): Option[Int] =
    (own.map(e => rank(e.value)).toList ++ lent.map(l => rank(l.value))).minOption

/** A requested change to one thread's labels, as typed by a person or an agent: each field is absent (leave alone), or `Some(None)`
  * (clear), or a new value. Validated before anything is written.
  */
final case class LabelEdit(goal: Option[Option[Goal]], value: Option[Option[Int]], blockedBy: Option[List[String]]):
  def isEmpty: Boolean = goal.isEmpty && value.isEmpty && blockedBy.isEmpty

object LabelEdit:
  def parse(threadId: String, goal: Option[String], value: Option[String], blockedBy: Option[List[String]]): Either[String, LabelEdit] =
    for
      g <- goal.fold[Either[String, Option[Option[Goal]]]](Right(None))(ThreadLabels.parseGoalWrite(_).map(Some(_)))
      v <- value.fold[Either[String, Option[Option[Int]]]](Right(None))(ThreadLabels.parseValueWrite(_).map(Some(_)))
      b <- blockedBy.fold[Either[String, Option[List[String]]]](Right(None)) { ids =>
        ids
          .map(_.trim)
          .filter(_.nonEmpty)
          .distinct
          .foldLeft[Either[String, List[String]]](Right(Nil)) { (acc, id) =>
            acc.flatMap(done => ThreadLabels.parseThreadId(id).left.map(reason => s"blockedBy '$id': $reason").map(done :+ _))
          }
          .flatMap(ids => if ids.contains(threadId) then Left("a thread cannot block itself") else Right(Some(ids)))
      }
    yield LabelEdit(g, v, b)
