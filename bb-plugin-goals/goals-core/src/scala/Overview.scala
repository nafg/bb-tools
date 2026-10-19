package goalscore

/** What the overview needs to know about an open thread. */
final case class ThreadInfo(id: String, parentId: Option[String], projectId: String, title: String)

final case class OverviewThread(
    id: String,
    title: String,
    projectId: String,
    label: Option[Effective],
    lent: List[Lent],
    importance: Option[Int],
    blockedBy: List[String],
    seconds: Double,
    children: List[OverviewThread]
)

/** The open root threads read as serving `goal` (None: unlabeled), and the attention every open thread with that effective goal received,
  * summed in full, so a child with its own goal counts toward that goal and not its parent's.
  */
final case class GoalGroup(goal: Option[Goal], threads: List[OverviewThread], seconds: Double)

final case class OverviewModel(groups: List[GoalGroup])

/** What one open thread resolves to before any tree or time is involved. `closedBlockers` are ids in its `blockedBy` that are not open
  * threads, so a picker can say the wait is on something closed.
  */
final case class Resolved(
    label: Option[Effective],
    lent: List[Lent],
    importance: Option[Int],
    blockedBy: List[String],
    closedBlockers: List[String]
)

object Overview:
  /** Every open thread's label, what it was lent, and its importance. `own` may hold closed threads too (ancestors, and anything reached
    * through `blockedBy`), so inheritance and lending can pass through them; only the open ones are returned.
    */
  def resolve(threads: List[ThreadInfo], own: Map[String, ThreadLabels], ancestors: Map[String, List[String]]): Map[String, Resolved] =
    val openIds   = threads.map(_.id).toSet
    val effective = threads.map(t => t.id -> Inheritance.effective(t.id, own, ancestors)).toMap
    val lent      = Lending.lent(own.view.mapValues(_.blockedBy).toMap, effective.getOrElse(_, None), openIds)
    threads.map { t =>
      val label     = effective(t.id)
      val received  = lent.getOrElse(t.id, Nil)
      val blockedBy = own.get(t.id).map(_.blockedBy).getOrElse(Nil)
      t.id -> Resolved(label, received, Lending.importance(label, received), blockedBy, blockedBy.filterNot(openIds.contains))
    }.toMap

  /** `threads` are the open threads; `ancestors` lists each one's ancestors from its parent upward (archived ones included, since they can
    * carry the label); `seconds` is attention time per thread, absent meaning none.
    */
  def build(
      threads: List[ThreadInfo],
      own: Map[String, ThreadLabels],
      ancestors: Map[String, List[String]],
      seconds: Map[String, Double]
  ): OverviewModel =
    val openIds  = threads.map(_.id).toSet
    val resolved = resolve(threads, own, ancestors)
    val byParent = threads.groupBy(_.parentId.filter(openIds.contains))

    def node(t: ThreadInfo): OverviewThread =
      val r = resolved(t.id)
      OverviewThread(
        id = t.id,
        title = t.title,
        projectId = t.projectId,
        label = r.label,
        lent = r.lent,
        importance = r.importance,
        blockedBy = r.blockedBy,
        seconds = seconds.getOrElse(t.id, 0.0),
        children = byParent.getOrElse(Some(t.id), Nil).map(node)
      )

    val roots         = byParent.getOrElse(None, Nil).map(node)
    val rootsByGoal   = roots.groupBy(_.label.map(_.goal))
    val secondsByGoal = threads
      .groupMapReduce(t => resolved(t.id).label.map(_.goal))(t => seconds.getOrElse(t.id, 0.0))(_ + _)
    val goals = (rootsByGoal.keySet ++ secondsByGoal.keySet).toList.sortBy(_.map(_.ordinal).getOrElse(Int.MaxValue))
    OverviewModel(goals.map(g => GoalGroup(g, rootsByGoal.getOrElse(g, Nil), secondsByGoal.getOrElse(g, 0.0))))
