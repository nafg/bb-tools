package conversecore

class PreviewSegmentHandoffTest extends munit.FunSuite:
  test("starts the replacement preview recorder before stopping the previous segment") {
    val events = collection.mutable.ArrayBuffer.empty[String]

    PreviewSegmentHandoff.replaceBeforeStopping(
      startReplacement = () => events += "start replacement",
      stopPrevious = () => events += "stop previous"
    )

    assertEquals(events.toList, List("start replacement", "stop previous"))
  }
