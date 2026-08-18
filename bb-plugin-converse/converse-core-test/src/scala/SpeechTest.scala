package conversecore

class SpeechTest extends munit.FunSuite:
  test("strips code fences"):
    assertEquals(
      Speech.stripMarkdown("Before\n```scala\nval x = 1\n```\nAfter"),
      "Before\n (code omitted) \nAfter".trim
    )

  test("strips inline markdown"):
    assertEquals(Speech.stripMarkdown("Use `foo` with **bold** and _italic_ text"), "Use foo with bold and italic text")

  test("strips headings, links, and html"):
    assertEquals(Speech.stripMarkdown("## Title\nSee [docs](https://x.dev) <br>"), "Title\nSee docs")

  test("splits sentences with pauses"):
    val chunks = Speech.splitSpeechChunks("First sentence. Second sentence!")
    assertEquals(chunks.map(_.text), List("First sentence.", "Second sentence!"))
    assertEquals(chunks.map(_.pauseSeconds), List(0.25, 0.0))

  test("does not split on abbreviations"):
    val chunks = Speech.splitSpeechChunks("Talk to Dr. Smith today.")
    assertEquals(chunks.map(_.text), List("Talk to Dr. Smith today."))

  test("does not split on single initials"):
    val chunks = Speech.splitSpeechChunks("John F. Kennedy spoke.")
    assertEquals(chunks.map(_.text), List("John F. Kennedy spoke."))

  test("list items become one chunk per line"):
    val chunks = Speech.splitSpeechChunks("Steps:\n\n- First step\n- Second step")
    assertEquals(chunks.map(_.text), List("Steps:", "First step", "Second step"))

  test("paragraph boundaries get the longer pause"):
    val chunks = Speech.splitSpeechChunks("One.\n\nTwo.")
    assertEquals(chunks.map(_.pauseSeconds), List(0.45, 0.0))

  test("empty and whitespace input yields no chunks"):
    assertEquals(Speech.splitSpeechChunks("   \n  "), Nil)

  test("last chunk always has no pause"):
    val chunks = Speech.splitSpeechChunks("A. B. C.")
    assertEquals(chunks.last.pauseSeconds, 0.0)
