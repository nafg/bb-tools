package conversecore

object PreviewSegmentHandoff:
  def replaceBeforeStopping(startReplacement: () => Unit, stopPrevious: () => Unit): Unit =
    startReplacement()
    stopPrevious()
