package conversecore

class NumberWordsTest extends munit.FunSuite:
  test("whole numbers are read as words"):
    assertEquals(NumberWords("0"), "zero")
    assertEquals(NumberWords("15"), "fifteen")
    assertEquals(NumberWords("40"), "forty")
    assertEquals(NumberWords("233"), "two hundred thirty three")
    assertEquals(NumberWords("3754"), "three thousand seven hundred fifty four")
    assertEquals(NumberWords("2026"), "two thousand twenty six")
    assertEquals(NumberWords("1000001"), "one million one")

  test("digits with a leading zero are read one by one"):
    assertEquals(NumberWords("007"), "zero zero seven")

  test("ordinals are read as words"):
    assertEquals(NumberWords.ordinal("1"), "first")
    assertEquals(NumberWords.ordinal("2"), "second")
    assertEquals(NumberWords.ordinal("21"), "twenty first")
    assertEquals(NumberWords.ordinal("30"), "thirtieth")
    assertEquals(NumberWords.ordinal("112"), "one hundred twelfth")
