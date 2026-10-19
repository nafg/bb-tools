package billingcore

object Durations:
  /** `1h 05m`, `12m`, `0m`: whole minutes, seconds dropped, for display. */
  def hoursMinutes(ms: Long): String =
    val minutes = ms / 60_000
    if minutes >= 60 then f"${minutes / 60}h ${minutes % 60}%02dm" else s"${minutes}m"
