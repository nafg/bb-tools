package conversecore

class SpeechTest extends munit.FunSuite:
  test("strips code fences"):
    assertEquals(
      Speech.stripMarkdown("Before\n```scala\nval x = 1\n```\nAfter"),
      "Before\n (code omitted) \nAfter".trim
    )

  test("strips inline markdown"):
    assertEquals(Speech.stripMarkdown("Use `foo` with **bold** and _italic_ text"), "Use foo with bold and italic text")

  test("keeps the asterisks around a word marked to be stressed, and drops those of bold"):
    val chunks = Speech.splitSpeechChunks("I didn't say he *stole* it, **I** said he ***borrowed*** it.")
    assertEquals(chunks.map(_.text), List("I didn't say he *stole* it, I said he borrowed it."))
    assertEquals(chunks.map(_.withoutStressMarks), List("I didn't say he stole it, I said he borrowed it."))

  test("removes stress marks but not a lone asterisk"):
    assertEquals(Speech.withoutStressMarks("He *stole* it, *not once* but 2 * 3 times."), "He stole it, not once but 2 * 3 times.")

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

  test("speaks a period inside an identifier as 'dot' so TTS engines do not end the sentence there"):
    val chunks = Speech.splitSpeechChunks("Call object.method from Widget.scala.")
    assertEquals(chunks.map(_.text), List("Call object dot method from Widget dot scala."))

  test("speaks a number marked with # as 'number', leaving other uses of # alone"):
    val chunks = Speech.splitSpeechChunks("[#233](https://example.com/pull/233) is merged, and C# is fine.")
    assertEquals(chunks.map(_.text), List("number two hundred thirty three is merged, and C# is fine."))

  test("spells out short words with no vowels, except titles and sounds said as words"):
    val chunks = Speech.splitSpeechChunks("Run sbt and npm over SSH, then open a PR. Mr Smith said hmm, mm.")
    assertEquals(chunks.map(_.text), List("Run S B T and N P M over S S H, then open a P R.", "Mr Smith said hmm, mm."))

  test("spells out hashes and ids, and speaks numbers in names as words"):
    val chunks = Speech.splitSpeechChunks("Commit 0db1ea5 fixes uexjg8nei2 for Scala3, ES2018 and sha256, costing $5.")
    assertEquals(
      chunks.map(_.text),
      List(
        "Commit zero D B one E A five fixes U E X J G eight N E I two for Scala three, ES two thousand eighteen " +
          "and sha two hundred fifty six, costing $5."
      )
    )

  test("splits camelCase names into words, each then spoken as any other word"):
    val chunks = Speech.splitSpeechChunks("Set smsChat and EntranceGroupV2SmsConfig.srcNumber on the HTTPServer.")
    assertEquals(
      chunks.map(_.text),
      List("Set S M S Chat and Entrance Group V two S M S Config dot S R C Number on the H T T P Server.")
    )

  test("speaks ordinals as words"):
    val chunks = Speech.splitSpeechChunks("It changed on the 21st, the 2nd and the 3rd.")
    assertEquals(chunks.map(_.text), List("It changed on the twenty first, the second and the third."))

  test("keeps contractions and possessives whole"):
    val chunks = Speech.splitSpeechChunks("I'll check, we'll see, you'd agree it's the PR's and sbt's fault.")
    assertEquals(chunks.map(_.text), List("I'll check, we'll see, you'd agree it's the P R's and S B T's fault."))

  test("spells a plural acronym out with its plural on the last letter"):
    val chunks = Speech.splitSpeechChunks("The IDs, URLs and PRs in EntranceGroupIDs.")
    assertEquals(chunks.map(_.text), List("The I D's, U R L's and P R's in Entrance Group I D's."))

  test("speaks a thread by its title, or as 'a thread' when its title is unknown"):
    val text = "@thread:thr_abc123 confirms it, and thr_def456 too (see @thread:thr_abc123#msg=42)."
    assertEquals(Speech.threadsReferredTo(text), Set("thr_abc123", "thr_def456"))
    val chunks = Speech.splitSpeechChunks(text, Map("thr_abc123" -> "Fix the $5 build"))
    assertEquals(chunks.map(_.text), List("Fix the $5 build confirms it, and a thread too (see Fix the $5 build)."))

  test("keeps periods in decimals and abbreviations"):
    val chunks = Speech.splitSpeechChunks("Version 3.5 ships, e.g. at 9 a.m. in the U.S.")
    assertEquals(chunks.map(_.text), List("Version 3.5 ships, e.g. at nine a.m. in the U.S."))

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

  private def words(n: Int, from: Int = 1) = (from until from + n).map(i => s"w$i").mkString(" ")

  test("leaves a chunk within the word limit whole"):
    val chunk = SpeechChunk("A short sentence,  with two spaces.", 0.25)
    assertEquals(Speech.withinWords(chunk, 6), List(chunk))

  test("cuts a long chunk where a clause ends nearest its middle, and keeps its pause for the last piece"):
    val text   = s"${words(4)}, ${words(9, 5)}; ${words(7, 14)} — ${words(4, 21)}."
    val pieces = Speech.withinWords(SpeechChunk(text, 0.45), 15)
    assertEquals(pieces.map(_.text), List(s"${words(4)}, ${words(9, 5)};", s"${words(7, 14)} — ${words(4, 21)}."))
    assertEquals(pieces.map(_.pauseSeconds), List(0.0, 0.45))

  test("cuts at a dash standing as a word"):
    val pieces = Speech.withinWords(SpeechChunk(s"${words(6)} — ${words(6, 7)}", 0), 8)
    assertEquals(pieces.map(_.text), List(words(6), s"— ${words(6, 7)}"))

  test("cuts between words when no clause end leaves as few pieces"):
    // The only comma would leave a piece of 2 words and one of 18, which needs cutting again.
    val pieces = Speech.withinWords(SpeechChunk(s"${words(2)}, ${words(18, 3)}", 0), 12)
    assertEquals(pieces.map(_.text), List(s"${words(2)}, ${words(8, 3)}", words(10, 11)))

  test("cuts as few pieces as the limit allows, none over it"):
    for n <- 1 to 90; limit <- List(1, 7, 25, 35) do
      val pieces = Speech.withinWords(SpeechChunk(words(n), 0), limit)
      assertEquals(pieces.length, (n + limit - 1) / limit, s"$n words, limit $limit")
      assert(pieces.forall(_.text.split(" ").length <= limit), s"$n words, limit $limit")
      assertEquals(pieces.map(_.text).mkString(" "), words(n))

  test("does not cut between the asterisks of a stress mark"):
    val pieces = Speech.withinWords(SpeechChunk("w1 w2 *w3 w4 w5* w6", 0), 4)
    assertEquals(pieces.map(_.text), List("w1 w2", "*w3 w4 w5* w6"))
