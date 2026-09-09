package conversecore

import VoiceSessionRestore.SessionEnd
import VoiceState.*

class VoiceStateTest extends munit.FunSuite:
  test("a session that failed to start is off, not in an error phase"):
    val state = Off(Some(Ending(SessionEnd.Failed, Some("could not start voice: no microphone"))))
    assert(!state.isOn)
    assertEquals(state.runningPhase, None)

  test("only failures, takeovers, and browser pauses have something to tell the user"):
    assertEquals(Ending(SessionEnd.Failed, Some("boom")).message, Some("boom"))
    assertEquals(Ending(SessionEnd.Failed, None).message, Some("voice failed"))
    assert(Ending(SessionEnd.TakenOverByAnotherWindow, None).message.isDefined)
    assert(Ending(SessionEnd.PausedUntilUserGesture, None).message.isDefined)
    for end <- List(SessionEnd.StoppedByUser, SessionEnd.Superseded, SessionEnd.FrontendUnloaded) do
      assertEquals(Ending(end, None).message, None, end.toString)

  test("every phase has a distinct wire name"):
    assertEquals(SessionPhase.values.map(_.name).distinct.length, SessionPhase.values.length)
