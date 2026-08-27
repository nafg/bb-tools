package wbscore

enum Leaf:
  case Hours(low: Double, high: Double)
  case Placeholder(note: String)

/** A validated node of the estimate: a named item, or a fork on a proposition. */
enum Node:
  case Item(name: String, value: Node.Value)
  case Fork(proposition: String, thenBranch: List[Node], elseBranch: List[Node])

object Node:
  enum Value:
    case Leaf(leaf: wbscore.Leaf)
    case Group(children: List[Node])

/** Hours for a node once its leaves are summed, with placeholders counted separately. */
final case class Estimate(low: Double, high: Double, unestimated: Int):
  def text: String =
    val hours = if low == high then Estimate.formatHours(low) else s"${Estimate.formatHours(low)}–${Estimate.formatHours(high)}"
    if unestimated == 0 then hours
    else s"$hours + $unestimated unestimated"

object Estimate:
  /** Hours as a duration: `4h`, `30m`, `1h 30m`, `0`; rounded to the minute. */
  def formatHours(hours: Double): String =
    val totalMinutes = math.round(hours * 60)
    val h            = totalMinutes / 60
    val m            = totalMinutes % 60
    if totalMinutes == 0 then "0"
    else if m == 0 then s"${h}h"
    else if h == 0 then s"${m}m"
    else s"${h}h ${m}m"

final case class FormatError(path: List[String], message: String) extends Exception(
      if path.isEmpty then message else s"${path.mkString(" / ")}: $message"
    )

object Wbs:
  /** Propositions match after whitespace normalisation, case-insensitively. */
  def propositionKey(proposition: String): String = proposition.trim.split("\\s+").mkString(" ").toLowerCase

  /** Parses estimate-file text into nodes, applying the format rules from the `wbs` skill. */
  def parse(source: String): Either[Exception, List[Node]] =
    Tree.parse(source).flatMap { entries =>
      try Right(nodes(entries, Nil))
      catch case e: FormatError => Left(e)
    }

  private def nodes(entries: List[Entry], path: List[String]): List[Node] =
    entries.map {
      case Entry.Item(name, tree) =>
        val here = path :+ name
        val value = tree match
          case Tree.Hours(h) => Node.Value.Leaf(Leaf.Hours(h, h))
          case Tree.Range(lo, hi) =>
            if lo > hi then
              throw FormatError(here, s"range low (${Estimate.formatHours(lo)}) exceeds high (${Estimate.formatHours(hi)})")
            Node.Value.Leaf(Leaf.Hours(lo, hi))
          case Tree.Placeholder(note) => Node.Value.Leaf(Leaf.Placeholder(note))
          case Tree.Group(children)   => Node.Value.Group(nodes(children, here))
        Node.Item(name, value)
      case Entry.Fork(proposition, Branches(t, e)) =>
        val here = path :+ s"if $proposition"
        Node.Fork(
          proposition,
          t.fold(Nil)(g => nodes(g.entries, here :+ "then")),
          e.fold(Nil)(g => nodes(g.entries, here :+ "else"))
        )
    }

  /** Distinct proposition keys beneath these nodes, in first-appearance order, with their display text. */
  def propositions(ns: List[Node]): List[(String, String)] =
    val seen = collection.mutable.LinkedHashMap.empty[String, String]
    def walk(ns: List[Node]): Unit = ns.foreach {
      case Node.Item(_, Node.Value.Group(cs)) => walk(cs)
      case Node.Item(_, _)                    => ()
      case Node.Fork(p, t, e) =>
        seen.getOrElseUpdate(propositionKey(p), p)
        walk(t)
        walk(e)
    }
    walk(ns)
    seen.toList

  /** Placeholder leaves beneath these nodes as (path, note). */
  def placeholders(ns: List[Node]): List[(List[String], String)] =
    def walk(ns: List[Node], path: List[String]): List[(List[String], String)] = ns.flatMap {
      case Node.Item(name, Node.Value.Leaf(Leaf.Placeholder(note))) => List((path :+ name) -> note)
      case Node.Item(_, Node.Value.Leaf(_))                         => Nil
      case Node.Item(name, Node.Value.Group(cs))                    => walk(cs, path :+ name)
      case Node.Fork(p, t, e) =>
        walk(t, path :+ s"if $p" :+ "then") ++ walk(e, path :+ s"if $p" :+ "else")
    }
    walk(ns, Nil)

  /** Min–max over every assignment of the propositions beneath `ns`; see the skill's Totals section. */
  def estimate(ns: List[Node]): Estimate =
    val props = propositions(ns).map(_._1)
    def sum(ns: List[Node], assignment: Map[String, Boolean]): (Double, Double) =
      ns.foldLeft((0.0, 0.0)) { case ((lo, hi), n) =>
        val (l, h) = n match
          case Node.Item(_, Node.Value.Leaf(Leaf.Hours(l, h))) => (l, h)
          case Node.Item(_, Node.Value.Leaf(_))                => (0.0, 0.0)
          case Node.Item(_, Node.Value.Group(cs))              => sum(cs, assignment)
          case Node.Fork(p, t, e) => sum(if assignment(propositionKey(p)) then t else e, assignment)
        (lo + l, hi + h)
      }
    val assignments: Iterator[Map[String, Boolean]] =
      Iterator.range(0, 1 << props.length).map { bits =>
        props.zipWithIndex.map((p, i) => p -> (((bits >> i) & 1) == 1)).toMap
      }
    val (low, high) = assignments.map(sum(ns, _)).foldLeft((Double.PositiveInfinity, Double.NegativeInfinity)) {
      case ((lo, hi), (l, h)) => (math.min(lo, l), math.max(hi, h))
    }
    Estimate(low, high, placeholders(ns).length)

  def estimate(n: Node): Estimate = estimate(List(n))
