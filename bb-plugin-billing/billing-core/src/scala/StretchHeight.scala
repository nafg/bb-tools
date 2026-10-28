package billingcore

/** How tall a stretch is drawn: it grows with its duration but slowly, and never below a readable minimum, so a
  * seconds-long glance and a three-hour block can share one scale with no zooming.
  */
object StretchHeight:
  val MinPx: Double       = 28.0
  private val PxPerSqrtMin = 10.0

  /** Height in pixels: `MinPx` plus a square-root of the minutes, so doubling the time adds well under double. */
  def px(durationMs: Long): Double =
    val minutes = math.max(durationMs, 0L) / 60_000.0
    MinPx + PxPerSqrtMin * math.sqrt(minutes)
