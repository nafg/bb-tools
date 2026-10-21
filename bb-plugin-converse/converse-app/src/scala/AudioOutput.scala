package converseapp

import scala.scalajs.js

/** Where an AudioContext's sound meant to be heard is sent. */
trait AudioOutput:
  def input: AudioNode
  def close(): Unit

  /** Which output this is, and whether it failed to start, for diagnostics. */
  def report: String

object AudioOutput:
  /** Call-style output on Android, which needs it to reach a Bluetooth
    * headset (see [[CallAudioOutput]]); the context's own output elsewhere,
    * since call-style output passes the sound through lossy compression.
    */
  def forThisDevice(ctx: AudioContext, onError: String => Unit): AudioOutput =
    if Platform.isAndroid then
      CallAudioOutput(ctx, onError)
    else ContextOutput(ctx)

/** The AudioContext's own output. */
final class ContextOutput(ctx: AudioContext) extends AudioOutput:
  def input: AudioNode = ctx.destination
  def close(): Unit    = ()
  def report: String   = "the audio context's own output"
