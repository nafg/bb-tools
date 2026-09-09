package conversecore

import VoiceSessionRestore.{Intent, SessionEnd}

class VoiceSessionRestoreTest extends munit.FunSuite:
  private val options = """{"ttsMode":"server","speechProbabilityThreshold":0.4,"endOfSpeechSilenceMs":2500}"""

  test("an intent with a thread survives a round trip"):
    val intent = Intent("sess-1", Some("thread-a"), options)
    assertEquals(VoiceSessionRestore.decode(VoiceSessionRestore.encode(intent)), Some(intent))

  test("an intent without a thread survives a round trip"):
    val intent = Intent("sess-1", None, options)
    assertEquals(VoiceSessionRestore.decode(VoiceSessionRestore.encode(intent)), Some(intent))

  test("text that is not an encoded intent decodes to nothing"):
    assertEquals(VoiceSessionRestore.decode(""), None)
    assertEquals(VoiceSessionRestore.decode("sess-1"), None)
    assertEquals(VoiceSessionRestore.decode("\nthread-a\n{}"), None)

  test("only an end the user did not ask for and can undo by interacting keeps the intent"):
    assertEquals(
      SessionEnd.values.filter(_.keepsIntent).toList,
      List(SessionEnd.PausedUntilUserGesture, SessionEnd.FrontendUnloaded)
    )

  test("an end the server already knows about, or that the session will come back from, is not reported to it"):
    assertEquals(
      SessionEnd.values.filterNot(_.notifiesServer).toList,
      List(SessionEnd.TakenOverByAnotherWindow, SessionEnd.PausedUntilUserGesture, SessionEnd.FrontendUnloaded)
    )
