package activitycore

class ViewSpanTest extends munit.FunSuite:
  private val policy = ViewSpanPolicy(heartbeatMs = 30_000, idleMs = 300_000)

  test("a viewed pane opens a span, extends it after a heartbeat, and stays quiet in between"):
    assertEquals(policy.decide(viewable = true, now = 1000, lastInputAt = 1000, open = None), Some(ViewReport.Open))
    assertEquals(policy.decide(viewable = true, now = 20_000, lastInputAt = 1000, open = Some((1000L, 1000L))), None)
    assertEquals(policy.decide(viewable = true, now = 31_000, lastInputAt = 1000, open = Some((1000L, 1000L))), Some(ViewReport.Extend(31_000)))

  test("losing focus closes the span now"):
    assertEquals(policy.decide(viewable = false, now = 50_000, lastInputAt = 40_000, open = Some((1000L, 31_000L))), Some(ViewReport.Close(50_000)))

  test("the idle timeout closes the span at the last input, even if a heartbeat reported later"):
    assertEquals(
      policy.decide(viewable = true, now = 400_000, lastInputAt = 90_000, open = Some((1000L, 391_000L))),
      Some(ViewReport.Close(90_000))
    )

  test("an idle close never ends before the span started"):
    assertEquals(policy.decide(viewable = true, now = 400_000, lastInputAt = 0, open = Some((50_000L, 50_000L))), Some(ViewReport.Close(50_000)))

  test("nothing is viewed while idle or hidden, and nothing is reported with no span"):
    assertEquals(policy.decide(viewable = true, now = 400_000, lastInputAt = 0, open = None), None)
    assertEquals(policy.decide(viewable = false, now = 1000, lastInputAt = 1000, open = None), None)
