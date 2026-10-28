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

    def widthOf(b: ThreadBox): Int = 1 + b.children.map(widthOf).sum
    def placeBox(b: ThreadBox, column: Int): PlacedBox =
      val top      = b.top match { case BoxEdge.Real(at) => y(at); case BoxEdge.ClippedAtSession => 0.0 }
      val bottom   = b.bottom match { case BoxEdge.Real(at) => y(at); case BoxEdge.ClippedAtSession => y(session.end) }
      val offsets  = b.children.scanLeft(column + 1)((c, ch) => c + widthOf(ch))
      val children = b.children.zip(offsets).map((ch, c) => placeBox(ch, c))
      PlacedBox(
        b,
        column,
        widthOf(b),
        top,
        bottom,
        b.own.map(m => PlacedMark(m, y(m.start), y(m.end))),
        children
      )
    val rootOffsets = forest.scanLeft(0)((c, b) => c + widthOf(b))
    val placed      = forest.zip(rootOffsets).map((b, c) => placeBox(b, c))
    SessionLayout(y(session.end), forest.map(widthOf).sum, placed, quiet.result(), moments.map(t => y(t) -> t))
