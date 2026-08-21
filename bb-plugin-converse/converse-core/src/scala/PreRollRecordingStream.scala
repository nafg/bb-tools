package conversecore

object PreRollRecordingStream:
  def build[Source, Delay, Destination, Stream](
      preRollMilliseconds: Double,
      source: Source,
      createDelay: Double => Delay,
      setDelaySeconds: (Delay, Double) => Unit,
      createDestination: () => Destination,
      connectSourceToDelay: (Source, Delay) => Unit,
      connectDelayToDestination: (Delay, Destination) => Unit,
      streamFromDestination: Destination => Stream
  ): Stream =
    val delaySeconds = preRollMilliseconds / 1000.0
    val delay        = createDelay(delaySeconds)
    setDelaySeconds(delay, delaySeconds)
    val destination = createDestination()
    connectSourceToDelay(source, delay)
    connectDelayToDestination(delay, destination)
    streamFromDestination(destination)
