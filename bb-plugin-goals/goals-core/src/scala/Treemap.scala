package goalscore

final case class Rect(x: Double, y: Double, w: Double, h: Double)

/** A node to lay out: `self` is its own weight, children carry theirs. */
final case class Node[A](key: A, self: Double, children: List[Node[A]]):
  def total: Double = self + children.map(_.total).sum

/** One rectangle of a nested layout. `isSelf` marks the cell standing for a node's own weight, drawn among its children.
  */
final case class Placed[A](key: A, rect: Rect, depth: Int, isSelf: Boolean)

object Treemap:
  /** Squarified treemap (Bruls, Huizing, van Wijk): the rectangle is cut into cells whose areas are proportional to the weights, laid in
    * rows along the shorter side, each row closed when adding the next item would make its cells less square. Zero or negative weights
    * share the space equally.
    */
  def squarify[A](items: List[(A, Double)], rect: Rect): List[(A, Rect)] =
    if items.isEmpty then Nil
    else
      val positive = if items.exists(_._2 > 0) then items.map { case (k, w) => k -> math.max(w, 0.0) } else items.map(_._1 -> 1.0)
      val total    = positive.map(_._2).sum
      val area     = rect.w * rect.h
      val scaled   = positive.sortBy(-_._2).map { case (k, w) => k -> (if total > 0 then w / total * area else area / positive.size) }
      place(scaled, rect, Nil)

  private def worst(row: List[Double], side: Double): Double =
    val sum = row.sum
    if sum <= 0 || side <= 0 then Double.PositiveInfinity
    else
      val s2 = sum * sum
      val sq = side * side
      row.map(a => math.max(sq * a / s2, s2 / (sq * a))).max

  @annotation.tailrec
  private def place[A](remaining: List[(A, Double)], free: Rect, acc: List[(A, Rect)]): List[(A, Rect)] =
    remaining match
      case Nil           => acc.reverse
      case first :: rest =>
        val side = math.min(free.w, free.h)
        @annotation.tailrec
        def grow(row: List[(A, Double)], rest: List[(A, Double)]): (List[(A, Double)], List[(A, Double)]) =
          rest match
            case next :: tail if worst((row :+ next).map(_._2), side) <= worst(row.map(_._2), side) => grow(row :+ next, tail)
            case _                                                                                  => (row, rest)
        val (row, left)       = grow(List(first), rest)
        val rowArea           = row.map(_._2).sum
        val (cells, nextFree) =
          if free.w >= free.h then
            // Lay the row as a vertical strip on the left.
            val stripW = if free.h > 0 then rowArea / free.h else 0.0
            var y      = free.y
            val cells  = row.map { case (k, a) =>
              val h = if stripW > 0 then a / stripW else 0.0
              val r = Rect(free.x, y, stripW, h)
              y += h
              k -> r
            }
            (cells, Rect(free.x + stripW, free.y, math.max(free.w - stripW, 0), free.h))
          else
            val stripH = if free.w > 0 then rowArea / free.w else 0.0
            var x      = free.x
            val cells  = row.map { case (k, a) =>
              val w = if stripH > 0 then a / stripH else 0.0
              val r = Rect(x, free.y, w, stripH)
              x += w
              k -> r
            }
            (cells, Rect(free.x, free.y + stripH, free.w, math.max(free.h - stripH, 0)))
        place(left, nextFree, cells.reverse ::: acc)

  /** Raises each weight so that, once the list is laid out in `areaPx` pixels, no item gets less than `minPx`: every weight below the floor
    * becomes the floor, where the floor is the share of the total that `minPx` is of `areaPx`, found by fixed-point iteration since raising
    * weights raises the total. Items above the floor keep their proportions. With nothing above the floor, every item weighs the same.
    */
  def floorWeights(weights: List[Double], areaPx: Double, minPx: Double): List[Double] =
    val clean = weights.map(w => if w.isNaN || w < 0 then 0.0 else w)
    val share = if areaPx > 0 then math.min(minPx / areaPx, 1.0 / math.max(clean.size, 1)) else 0.0
    if share <= 0 || clean.sum <= 0 then clean.map(_ => 1.0)
    else
      @annotation.tailrec
      def settle(floor: Double, rounds: Int): Double =
        val total = clean.map(math.max(_, floor)).sum
        val next  = share * total
        if rounds == 0 || math.abs(next - floor) <= 1e-9 * total then next else settle(next, rounds - 1)
      val floor = settle(0.0, 50)
      clean.map(math.max(_, floor))

  /** Lays `nodes` out in `rect`, then each node's children inside its own rectangle, below a `header` strip and inset by `pad`. A node with
    * its own weight gets a self cell among its children. Rectangles never go negative: a node too small for its padding gets an empty inner
    * area.
    */
  def layoutNested[A](nodes: List[Node[A]], rect: Rect, pad: Double, header: Double): List[Placed[A]] =
    def inner(r: Rect): Rect =
      Rect(r.x + pad, r.y + header + pad, math.max(r.w - 2 * pad, 0), math.max(r.h - header - 2 * pad, 0))
    def place(node: Node[A], r: Rect, depth: Int): List[Placed[A]] =
      val own = Placed(node.key, r, depth, isSelf = false)
      if node.children.isEmpty then List(own)
      else
        val selfItem = if node.self > 0 then List(Option.empty[Node[A]] -> node.self) else Nil
        val items    = node.children.map(c => Option(c) -> c.total) ++ selfItem
        own :: squarify(items, inner(r)).flatMap {
          case (Some(child), cr) => place(child, cr, depth + 1)
          case (None, cr)        => List(Placed(node.key, cr, depth + 1, isSelf = true))
        }
    squarify(nodes.map(n => n -> n.total), rect).flatMap { case (node, r) => place(node, r, 0) }
