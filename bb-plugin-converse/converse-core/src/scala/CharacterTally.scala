package conversecore

/** How many characters a metered speech provider has been sent in a calendar
  * month (UTC, as Google bills). The provider's free allowance is per month,
  * so a tally for another month starts over.
  */
final case class CharacterTally(month: String, characters: Long):
  /** This tally with `count` more characters sent in `inMonth`. */
  def add(inMonth: String, count: Long): CharacterTally =
    if inMonth == month then copy(characters = characters + count) else CharacterTally(inMonth, count)

object CharacterTally:
  /** The month `epochMillis` falls in, as "2026-10". */
  def monthOf(epochMillis: Double): String =
    val days  = Math.floor(epochMillis / 86_400_000.0).toLong
    // Days since 1970-01-01 to a civil date (Howard Hinnant's algorithm).
    val z     = days + 719468
    val era   = (if z >= 0 then z else z - 146096) / 146097
    val doe   = z - era * 146097
    val yoe   = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val doy   = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp    = (5 * doy + 2) / 153
    val month = if mp < 10 then mp + 3 else mp - 9
    val year  = yoe + era * 400 + (if month <= 2 then 1 else 0)
    f"$year%04d-$month%02d"

  /** Google Cloud Text-to-Speech's free WaveNet and Standard characters a
    * month; all of the request's SSML counts.
    */
  val GoogleWaveNetFreeCharacters: Long = 4_000_000L
