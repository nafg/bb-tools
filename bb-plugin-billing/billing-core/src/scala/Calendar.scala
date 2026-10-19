package billingcore

/** Where local days begin and end for a given zone: the platform supplies this (Intl on Node, java.time on the JVM) so
  * the pure day arithmetic stays cross-built.
  */
trait Calendar:
  /** The local calendar day `epochMs` falls on, as `yyyy-MM-dd`. */
  def dayOf(epochMs: Long): String

  /** The instant the local day containing `epochMs` begins. */
  def dayStart(epochMs: Long): Long

  /** The instant the local day after the one containing `epochMs` begins. */
  def nextDayStart(epochMs: Long): Long

  /** The instant the local day `day` (`yyyy-MM-dd`) begins. */
  def startOfDay(day: String): Long

object Calendar:
  val DayMs: Long = 24L * 60 * 60 * 1000

  /** A zone with a constant offset from UTC. Enough for tests and for zones without daylight saving. */
  final class FixedOffset(offsetMs: Long) extends Calendar:
    private def localDays(epochMs: Long): Long = Math.floorDiv(epochMs + offsetMs, DayMs)
    def dayOf(epochMs: Long): String           = CivilDate.fromEpochDay(localDays(epochMs)).iso
    def dayStart(epochMs: Long): Long          = localDays(epochMs) * DayMs - offsetMs
    def nextDayStart(epochMs: Long): Long      = dayStart(epochMs) + DayMs
    def startOfDay(day: String): Long          = CivilDate.parse(day).epochDay * DayMs - offsetMs

  val Utc: Calendar = FixedOffset(0)

/** A proleptic Gregorian date, for day keys without java.time (which Scala.js lacks). */
final case class CivilDate(year: Int, month: Int, day: Int):
  def iso: String = f"$year%04d-$month%02d-$day%02d"

  /** Days since 1970-01-01. */
  def epochDay: Long =
    val y   = if month <= 2 then year - 1 else year
    val era = Math.floorDiv(y, 400)
    val yoe = y - era * 400
    val mp  = (month + 9) % 12
    val doy = (153 * mp + 2) / 5 + day - 1
    val doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
    era * 146097L + doe - 719468L

  def plusDays(n: Long): CivilDate = CivilDate.fromEpochDay(epochDay + n)

  /** 0 = Sunday … 6 = Saturday. */
  def dayOfWeek: Int = Math.floorMod(epochDay + 4, 7).toInt

object CivilDate:
  def fromEpochDay(epochDay: Long): CivilDate =
    val z   = epochDay + 719468
    val era = Math.floorDiv(z, 146097)
    val doe = z - era * 146097
    val yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val y   = yoe + era * 400
    val doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp  = (5 * doy + 2) / 153
    val d   = doy - (153 * mp + 2) / 5 + 1
    val m   = if mp < 10 then mp + 3 else mp - 9
    CivilDate((if m <= 2 then y + 1 else y).toInt, m.toInt, d.toInt)

  private val Iso = """(\d{4})-(\d{2})-(\d{2})""".r

  def parse(text: String): CivilDate =
    text match
      case Iso(y, m, d) => CivilDate(y.toInt, m.toInt, d.toInt)
      case other        => throw IllegalArgumentException(s"not a yyyy-MM-dd date: $other")

  def parseOption(text: String): Option[CivilDate] =
    try Some(parse(text))
    catch case _: IllegalArgumentException => None
