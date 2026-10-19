package billing

import billingcore.{Calendar, CivilDate}

import scala.scalajs.js

/** Local days of an IANA zone, through Node's Intl. Daylight-saving shifts are handled by re-checking the computed
  * midnight against the zone's own reading of it.
  */
final class IntlCalendar(val timeZone: String) extends Calendar:
  private val format = js.Dynamic.newInstance(js.Dynamic.global.Intl.DateTimeFormat)(
    "en-US",
    js.Dynamic.literal(
      "timeZone"  -> timeZone,
      "hourCycle" -> "h23",
      "year"      -> "numeric",
      "month"     -> "2-digit",
      "day"       -> "2-digit",
      "hour"      -> "2-digit",
      "minute"    -> "2-digit",
      "second"    -> "2-digit"
    )
  )

  /** The zone's reading of `epochMs`: its date and the seconds since that date's midnight. */
  private def local(epochMs: Long): (CivilDate, Long) =
    val parts = format
      .formatToParts(new js.Date(epochMs.toDouble))
      .asInstanceOf[js.Array[js.Dynamic]]
      .map(part => part.`type`.asInstanceOf[String] -> part.value.asInstanceOf[String])
      .toMap
    val hour  = parts("hour").toInt % 24 // some engines print midnight as "24"
    (
      CivilDate(parts("year").toInt, parts("month").toInt, parts("day").toInt),
      hour * 3600L + parts("minute").toLong * 60 + parts("second").toLong
    )

  /** `epochMs` as the zone would stamp it on a clock with no zone: date plus time of day, in ms. */
  private def wallClockMs(epochMs: Long): Long =
    val (date, seconds) = local(epochMs)
    date.epochDay * Calendar.DayMs + seconds * 1000 + Math.floorMod(epochMs, 1000L)

  def dayOf(epochMs: Long): String = local(epochMs)._1.iso

  def startOfDay(day: String): Long = startOf(CivilDate.parse(day))

  private def startOf(date: CivilDate): Long =
    val wall   = date.epochDay * Calendar.DayMs
    val first  = wall - (wallClockMs(wall) - wall)
    val offset = wallClockMs(first) - first
    val second = wall - offset
    if wallClockMs(second) == wall then second else first

  def dayStart(epochMs: Long): Long     = startOf(local(epochMs)._1)
  def nextDayStart(epochMs: Long): Long = startOf(local(epochMs)._1.plusDays(1))

object IntlCalendar:
  /** The zone the server process runs in. */
  def serverZone: String =
    js.Dynamic.newInstance(js.Dynamic.global.Intl.DateTimeFormat)().resolvedOptions().timeZone.asInstanceOf[String]
