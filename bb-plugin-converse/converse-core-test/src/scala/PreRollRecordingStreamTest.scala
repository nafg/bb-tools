package conversecore

class PreRollRecordingStreamTest extends munit.FunSuite:
  test("builds a half-second delayed stream for speech-triggered recording"):
    val events = collection.mutable.ArrayBuffer.empty[String]

    val stream = PreRollRecordingStream.build(
      preRollMilliseconds = 500,
      source = "live microphone",
      createDelay = seconds =>
        events += s"create delay $seconds"
        "delay",
      setDelaySeconds = (_, seconds) => events += s"set delay $seconds",
      createDestination = () =>
        events += "create destination"
        "destination",
      connectSourceToDelay = (source, delay) => events += s"connect $source to $delay",
      connectDelayToDestination = (delay, destination) => events += s"connect $delay to $destination",
      streamFromDestination = destination =>
        events += s"read stream from $destination"
        "delayed microphone"
    )

    assertEquals(stream, "delayed microphone")
    assertEquals(
      events.toList,
      List(
        "create delay 0.5",
        "set delay 0.5",
        "create destination",
        "connect live microphone to delay",
        "connect delay to destination",
        "read stream from destination"
      )
    )
