package conversecore

/** A string of digits as English words: "3754" as "three thousand seven
  * hundred fifty four". Digits with a leading zero, or too many for the
  * named scales, are read one by one ("007" as "zero zero seven").
  */
object NumberWords:
  private val Ones = Vector(
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve",
    "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
  )
  private val Tens   = Vector("", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  private val Scales = Vector("", "thousand", "million", "billion", "trillion")

  def apply(digits: String): String =
    if (digits.length > 1 && digits.startsWith("0")) || digits.length > 3 * Scales.length then
      digits.map(d => Ones(d.asDigit)).mkString(" ")
    else
      val n = BigInt(digits)
      if n == 0 then "zero"
      else
        val groups = Iterator.iterate(n)(_ / 1000).takeWhile(_ > 0).map(g => (g % 1000).toInt).toVector
        groups.zipWithIndex.reverse
          .collect { case (group, scale) if group > 0 => (belowThousand(group) :+ Scales(scale)).filter(_.nonEmpty) }
          .flatten
          .mkString(" ")

  private def belowThousand(n: Int): Vector[String] =
    val hundreds = if n >= 100 then Vector(Ones(n / 100), "hundred") else Vector.empty
    val rest     = n % 100
    hundreds ++ (
      if rest == 0 then Vector.empty
      else if rest < 20 then Vector(Ones(rest))
      else Vector(Tens(rest / 10)) ++ (if rest % 10 > 0 then Vector(Ones(rest % 10)) else Vector.empty)
    )
