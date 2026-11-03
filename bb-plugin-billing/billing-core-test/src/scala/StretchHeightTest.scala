package billingcore

class StretchHeightTest extends munit.FunSuite:
  private def min(m: Int): Long = m * 60_000L

  test("height never drops below the minimum and grows with time, slowly"):
    assertEquals(StretchHeight.px(0), StretchHeight.MinPx)
    assert(StretchHeight.px(min(180)) > StretchHeight.px(min(30)))
    assert(StretchHeight.px(min(60)) < 2 * StretchHeight.px(min(30)))
