package conversecore

class InterimTranscriptTest extends munit.FunSuite:
  test("segments are shown with a visible seam between them"):
    assertEquals(InterimTranscript.render(List("so the first thing", "is that we"), stillCapturing = false), "so the first thing … is that we")

  test("while speech is still being captured the preview ends open"):
    assertEquals(InterimTranscript.render(List("so the first thing"), stillCapturing = true), "so the first thing …")

  test("empty segments leave no seam"):
    assertEquals(InterimTranscript.render(List("", "  ", "hello"), stillCapturing = false), "hello")
    assertEquals(InterimTranscript.render(Nil, stillCapturing = true), "")

  test("the transcription prompt gets the words without seams"):
    assertEquals(InterimTranscript.wordsSoFar(List("so the first thing", "", "is that we")), "so the first thing is that we")
