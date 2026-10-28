package billingcore

/** A mark placed on the page: `yTop` and `yBottom` in pixels from the top of the session. */
final case class PlacedMark(mark: Mark, yTop: Double, yBottom: Double)

/** A thread's box placed on the page. It occupies columns `column` to `column + span - 1`; its own strip is the first
  * column and its children's boxes follow side by side.
  */
final case class PlacedBox(
  box: ThreadBox,
  column: Int,
  span: Int,
  yTop: Double,
  yBottom: Double,
  own: Vector[PlacedMark],
  children: Vector[PlacedBox])

/** A stretch of the session in which no thread had attention, drawn as a labelled break. */
final case class QuietGap(yTop: Double, yBottom: Double, durationMs: Long)

/** The session on the page: the boxes, the breaks, and the time at some heights so the page can label its axis. */
final case class SessionLayout(
  height: Double,
  columns: Int,
  roots: Vector[PlacedBox],
  quiet: Vector[QuietGap],
  axis: Vector[(Double, Long)])

object SessionGeometry:
  /** Height given to a step between two moments when something is happening in it. */
  val ActiveStepPx: Double = 4.0

  /** Height given to a short quiet step, and to a long one. */
  val ShortQuietPx: Double = 6.0
  val LongQuietPx: Double  = 36.0

  /** A quiet spell at least this long is drawn as a labelled break. */
  val LongQuietMs: Long = 60 * 60_000L

  /** The minimum height a mark must get so its text is readable: what the stretch's duration alone would give. */
  def minHeight(mark: Mark): Double = StretchHeight.px(mark.durationMs)

  /** The axis moments, with any closer than `AxisMinGapPx` to the last one kept dropped so labels do not pile up. */
  val AxisMinGapPx: Double = 16.0
  private def thin(axis: Vector[(Double, Long)]): Vector[(Double, Long)] =
    axis.foldLeft(Vector.empty[(Double, Long)]) { (acc, p) =>
      if acc.isEmpty || p._1 - acc.last._1 >= AxisMinGapPx then acc :+ p else acc
    }

  private def marksOf(box: ThreadBox): Vector[Mark] = box.own ++ box.children.flatMap(marksOf)

  private def boxTimes(session: Session, box: ThreadBox): Vector[Long] =
    def edge(e: BoxEdge, fallback: Long): Long =
      e match
        case BoxEdge.Real(at)        => at
        case BoxEdge.ClippedAtSession => fallback
    Vector(edge(box.top, session.start), edge(box.bottom, session.end)) ++ box.children.flatMap(boxTimes(session, _))

  /** Places the forest. One pass down the sorted moments gives every moment a height: each step has a small height of
    * its own, and a moment is pushed lower where a mark ending there would otherwise be shorter than its minimum. Every
    * thread uses the same heights, so two threads at the same moment are level.
    */
  def place(session: Session, forest: Vector[ThreadBox]): SessionLayout =
    val allMarks = forest.flatMap(marksOf)
    val moments  = (Vector(session.start, session.end) ++ allMarks.flatMap(m => Vector(m.start, m.end)) ++
      forest.flatMap(boxTimes(session, _))).map(t => math.min(math.max(t, session.start), session.end)).distinct.sorted
    def covered(a: Long, b: Long): Boolean = allMarks.exists(m => m.start < b && m.end > a)

    val endingAt = allMarks.groupBy(_.end)
    val heights  = scala.collection.mutable.Map(moments.head -> 0.0)
    val quiet    = Vector.newBuilder[QuietGap]
    moments.zip(moments.drop(1)).foreach { (a, b) =>
      val isQuiet = !covered(a, b)
      val step    =
        if !isQuiet then ActiveStepPx
        else if b - a >= LongQuietMs then LongQuietPx
        else ShortQuietPx
      val floor   = endingAt.getOrElse(b, Vector.empty).map(m => heights(math.max(m.start, session.start)) + minHeight(m))
      val y       = (heights(a) + step) +: floor
      heights(b) = y.max
      if isQuiet && b - a >= LongQuietMs then quiet += QuietGap(heights(a), heights(b), b - a)
    }
    def y(t: Long): Double = heights(math.min(math.max(t, session.start), session.end))

    // A box is drawn from where its thread began, or first has activity, to where it ended, or last has activity.
    def extent(b: ThreadBox): (Long, Long) =
      val from = b.top match { case BoxEdge.Real(at) => at; case BoxEdge.ClippedAtSession => SessionView.firstActivity(b) }
      val to   = b.bottom match { case BoxEdge.Real(at) => at; case BoxEdge.ClippedAtSession => SessionView.lastActivity(b) }
      (math.min(from, SessionView.firstActivity(b)), math.max(to, SessionView.lastActivity(b)))

    /** Siblings that are never active together share a column: each goes in the first lane free by its start. */
    def lanes(siblings: Vector[ThreadBox]): Vector[Vector[ThreadBox]] =
      siblings.sortBy(b => extent(b)._1).foldLeft(Vector.empty[Vector[ThreadBox]]) { (acc, b) =>
        val (from, _) = extent(b)
        acc.indexWhere(lane => extent(lane.last)._2 <= from) match
          case -1 => acc :+ Vector(b)
          case i  => acc.updated(i, acc(i) :+ b)
      }

    def widthOf(b: ThreadBox): Int                       = 1 + laneWidths(b.children).sum
    def laneWidths(siblings: Vector[ThreadBox]): Vector[Int] = lanes(siblings).map(_.map(widthOf).max)

    def placeAll(siblings: Vector[ThreadBox], column: Int): Vector[PlacedBox] =
      val ls      = lanes(siblings)
      val offsets = ls.scanLeft(column)((c, lane) => c + lane.map(widthOf).max)
      ls.zip(offsets).flatMap((lane, c) => lane.map(b => placeBox(b, c)))

    def placeBox(b: ThreadBox, column: Int): PlacedBox =
      val (from, to) = extent(b)
      PlacedBox(
        b,
        column,
        widthOf(b),
        y(from),
        y(to),
        b.own.map(m => PlacedMark(m, y(m.start), y(m.end))),
        placeAll(b.children, column + 1)
      )
    val placed = placeAll(forest, 0)
    SessionLayout(y(session.end), laneWidths(forest).sum, placed, quiet.result(), thin(moments.map(t => y(t) -> t)))
