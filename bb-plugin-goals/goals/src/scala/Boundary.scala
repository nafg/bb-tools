package goals

import bbplugin.Dyn
import goalscore.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** Option-returning readers for loosely typed values that cross the plugin boundary, beside the shared `Dyn` helpers (whose `str` returns
  * an empty string for an absent value). Anything that is not what the SDK's declared shape promises reads as absent rather than throwing.
  */
object Js:
  def optStr(value: js.Dynamic): Option[String] =
    if Dyn.isDefined(value) then
      value.asInstanceOf[Any] match
        case s: String => Some(s)
        case _         => None
    else None

  def optArr(value: js.Dynamic): Option[List[js.Dynamic]] =
    if Dyn.isDefined(value) && js.Array.isArray(value) then Some(value.asInstanceOf[js.Array[js.Dynamic]].toList) else None

  def labelsOf(metadata: js.Dynamic): ThreadLabels =
    ThreadLabels.fromStored(
      goal = optStr(metadata.goal),
      value = Dyn.number(metadata.value),
      myMove = optStr(metadata.myMove),
      myMoveAt = Dyn.number(metadata.myMoveAt)
    )

/** The goals namespace and the overview as the frontend, the CLI, and agents receive them. */
object Out:
  def goal(g: Goal): js.Any = g.letter

  /** A thread's own labels, with the threads it waits on beside them. */
  def own(labels: ThreadLabels, blockedBy: List[String]): js.Dynamic =
    js.Dynamic.literal(
      "goal"      -> labels.goal.map(goal).orNull,
      "value"     -> labels.value.map(v => v: js.Any).orNull,
      "blockedBy" -> blockedBy.toJSArray,
      "myMove"    -> labels.myMove.map(m => m.name: js.Any).orNull,
      "myMoveAt"  -> labels.myMoveAt.map(t => t.toDouble: js.Any).orNull
    )

  def effective(label: Option[Effective]): js.Any =
    label.fold[js.Any](null) { e =>
      val source = e.source match
        case LabelSource.Own             => js.Dynamic.literal("kind" -> "own")
        case LabelSource.Inherited(from) => js.Dynamic.literal("kind" -> "inherited", "fromThreadId" -> from)
      js.Dynamic.literal("goal" -> goal(e.goal), "value" -> e.value.map(v => v: js.Any).orNull, "source" -> source)
    }

  def lent(items: List[Lent]): js.Array[js.Any] =
    items
      .map(l =>
        js.Dynamic.literal("fromThreadId" -> l.fromThreadId, "goal" -> goal(l.goal), "value" -> l.value.map(v => v: js.Any).orNull): js.Any
      )
      .toJSArray

  def thread(t: OverviewThread): js.Dynamic =
    js.Dynamic.literal(
      "id"         -> t.id,
      "title"      -> t.title,
      "projectId"  -> t.projectId,
      "label"      -> effective(t.label),
      "lent"       -> lent(t.lent),
      "importance" -> t.importance.map(v => v: js.Any).orNull,
      "blockedBy"  -> t.blockedBy.toJSArray,
      "seconds"    -> t.seconds,
      "archived"   -> t.archived,
      "children"   -> t.children.map(thread).toJSArray
    )

  def groups(model: OverviewModel): js.Array[js.Any] =
    model.groups.map { g =>
      js.Dynamic.literal(
        "goal"    -> g.goal.map(goal).orNull,
        "title"   -> g.goal.fold("Unlabeled")(_.title),
        "seconds" -> g.seconds,
        "threads" -> g.threads.map(thread).toJSArray
      ): js.Any
    }.toJSArray
