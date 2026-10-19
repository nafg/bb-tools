package billingcore

/** One time entry as Zoho Books logs it: a thread's whole day under its project and task. */
object ZohoEntries:
  /** Zoho caps `notes` at 500 characters. */
  val NotesMaxLength = 500

  /** Rounds a duration up to the next multiple of `incrementMinutes`; a zero duration stays zero. */
  def billedMinutes(exactMs: Long, incrementMinutes: Int): Long =
    require(incrementMinutes >= 1, "billing increment must be at least one minute")
    if exactMs <= 0 then 0
    else
      val minutes = (exactMs + 59_999) / 60_000
      ((minutes + incrementMinutes - 1) / incrementMinutes) * incrementMinutes

  def logTime(minutes: Long): String = f"${minutes / 60}%02d:${minutes % 60}%02d"

  def notes(threadTitle: String): String =
    val trimmed = threadTitle.trim
    if trimmed.length <= NotesMaxLength then trimmed else trimmed.take(NotesMaxLength - 1) + "…"
