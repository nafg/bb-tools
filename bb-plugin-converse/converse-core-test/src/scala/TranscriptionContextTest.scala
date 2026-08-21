package conversecore

class TranscriptionContextTest extends munit.FunSuite:
  test("preceding transcript is used as the next segment prompt"):
    assertEquals(TranscriptionContext.promptFrom("The first part of the sentence"), "The first part of the sentence")

  test("prompt retains only the most recent bounded context"):
    val preceding = "early words " + "x" * 900 + " recent ending"
    val prompt    = TranscriptionContext.promptFrom(preceding)
    assert(prompt.length <= TranscriptionContext.MaxPromptChars)
    assert(prompt.endsWith(" recent ending"))
    assert(!prompt.startsWith("early words"))

