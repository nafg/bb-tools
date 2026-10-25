package conversecore

class SsmlTest extends munit.FunSuite:
  test("a stressed word becomes an emphasis element, the rest text"):
    assertEquals(
      Ssml.withEmphasis("I didn't say he *stole* it."),
      """<speak>I didn&apos;t say he <emphasis level="strong">stole</emphasis> it.</speak>"""
    )

  test("several stressed words and a stressed phrase"):
    assertEquals(
      Ssml.withEmphasis("*Not* once, *not twice*."),
      """<speak><emphasis level="strong">Not</emphasis> once, <emphasis level="strong">not twice</emphasis>.</speak>"""
    )

  test("a lone asterisk is text, not a mark"):
    assertEquals(Ssml.withEmphasis("2 * 3 is 6"), "<speak>2 * 3 is 6</speak>")

  test("reserved characters are escaped, inside and outside emphasis"):
    assertEquals(
      Ssml.withEmphasis("""Use <b> & "quotes" for *a<b*"""),
      """<speak>Use &lt;b&gt; &amp; &quot;quotes&quot; for <emphasis level="strong">a&lt;b</emphasis></speak>"""
    )

  test("a speech chunk's text goes through as the chunk keeps it"):
    val chunk = Speech.splitSpeechChunks("He **really** *stole* it.").head
    assertEquals(Ssml.withEmphasis(chunk.text), """<speak>He really <emphasis level="strong">stole</emphasis> it.</speak>""")
