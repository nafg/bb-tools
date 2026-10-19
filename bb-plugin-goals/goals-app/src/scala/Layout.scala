package goalsapp

import goalscore.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

/** What one treemap cell stands for. */
private enum Key:
  case GoalBox(letter: Option[String])
  case Thread(id: String)

/** Lays the overview's groups out as nested boxes: a box per goal, root threads inside it, children inside their parents, each thread's own
  * time as a self cell among its children. `groups` is the `overview` rpc result's `groups` array. Returns cells `{ kind: "goal" | "thread" |
  * "self", goal?, id?, x, y, w, h, depth }` in paint order (parents before children).
  */
@JSExportTopLevel("layoutOverview")
def layoutOverview(groups: js.Array[js.Dynamic], width: Double, height: Double, pad: Double, header: Double): js.Array[js.Dynamic] =
  def seconds(t: js.Dynamic): Double = t.seconds.asInstanceOf[Any] match
    case n: Double => n
    case _         => 0.0
  def children(t: js.Dynamic): List[js.Dynamic] =
    if js.Array.isArray(t.children) then t.children.asInstanceOf[js.Array[js.Dynamic]].toList else Nil
  def allThreads(t: js.Dynamic): List[js.Dynamic] = t :: children(t).flatMap(allThreads)

  val roots = groups.toList.flatMap(g => if js.Array.isArray(g.threads) then g.threads.asInstanceOf[js.Array[js.Dynamic]].toList else Nil)
  // Every thread gets at least about a 24 px square of the whole map, whatever its time; the rest stays proportional.
  val all     = roots.flatMap(allThreads)
  val weights = all.map(t => t.id.asInstanceOf[String]).zip(Treemap.floorWeights(all.map(seconds), width * height, minPx = 24 * 24)).toMap

  def node(t: js.Dynamic): Node[Key] =
    val id = t.id.asInstanceOf[String]
    Node(Key.Thread(id), weights.getOrElse(id, 1.0), children(t).map(node))

  val goalNodes = groups.toList.map { g =>
    val letter = g.goal.asInstanceOf[Any] match
      case s: String => Some(s)
      case _         => None
    val threads = if js.Array.isArray(g.threads) then g.threads.asInstanceOf[js.Array[js.Dynamic]].toList else Nil
    Node(Key.GoalBox(letter), 0.0, threads.map(node))
  }

  Treemap
    .layoutNested(goalNodes, Rect(0, 0, width, height), pad, header)
    .map { placed =>
      val cell = js.Dynamic.literal(
        "x"     -> placed.rect.x,
        "y"     -> placed.rect.y,
        "w"     -> placed.rect.w,
        "h"     -> placed.rect.h,
        "depth" -> placed.depth
      )
      placed.key match
        case Key.GoalBox(letter) =>
          cell.updateDynamic("kind")("goal")
          cell.updateDynamic("goal")(letter.orNull)
        case Key.Thread(id) =>
          cell.updateDynamic("kind")(if placed.isSelf then "self" else "thread")
          cell.updateDynamic("id")(id)
      cell
    }
    .toJSArray
