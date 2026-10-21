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
