package conversecore

import RecordingStatus.*

class RecordingStatusTest extends munit.FunSuite:
  test("every status reads back from the columns it is stored in"):
    for status <- List(Received, Sending, Sent, Empty, Failed("transcriber refused it")) do
      val lastError = status match
        case Failed(reason) => reason
        case _              => null
      assertEquals(RecordingStatus.fromColumns(status.code, lastError), Some(status))

  test("a row stored before statuses were tracked has none"):
    assertEquals(RecordingStatus.fromColumns(null, null), None)

  test("after a restart, a recording cut off mid-delivery is failed, not resent"):
    assertEquals(RecordingStatus.afterRestart(Sending), Failed(InterruptedWhileSending))

  test("after a restart, a received recording is processed again and finished ones stay as they were"):
    for status <- List(Received, Sent, Empty, Failed("x")) do assertEquals(RecordingStatus.afterRestart(status), status)
