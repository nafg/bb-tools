package converseapp

import scala.scalajs.js

/** Where an AudioContext's sound meant to be heard is sent. */
trait AudioOutput:
  def input: AudioNode
  def close(): Unit

object AudioOutput:
  /** Call-style output on Android, which needs it to reach a Bluetooth
    * headset (see [[CallAudioOutput]]); the context's own output elsewhere.
    * Call-style output is not used everywhere because Chromium plays a media
    * stream in an audio element at a varying rate: measured on this plugin's
    * replies, a sentence started after the first ten seconds or so came out
    * up to nearly a semitone flat, drifting back over the following seconds.
    */
  def forThisDevice(ctx: AudioContext, onError: String => Unit): AudioOutput =
    if js.Dynamic.global.navigator.userAgent.asInstanceOf[String].contains("Android") then
      CallAudioOutput(ctx, onError)
    else ContextOutput(ctx)

/** The AudioContext's own output. */
final class ContextOutput(ctx: AudioContext) extends AudioOutput:
  def input: AudioNode = ctx.destination
  def close(): Unit    = ()
