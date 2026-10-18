package converseapp

import scala.scalajs.js

/** An AudioContext's sound played through an audio element fed by a
  * MediaStream, the way a web page plays the other side of a call. On Android,
  * a page using a Bluetooth headset's microphone puts the headset in call
  * mode, and the AudioContext's own output then went to the phone's speaker
  * instead of the headset; this sends the sound the way call audio goes.
  * Create it inside a user gesture, since the element must be allowed to play.
  */
final class CallAudioOutput(ctx: AudioContext, onError: String => Unit):
  private val stream  = ctx.createMediaStreamDestination()
  private val element = js.Dynamic.newInstance(js.Dynamic.global.Audio)()
  element.srcObject = stream.stream
  element.play().asInstanceOf[js.Promise[Unit]].toFuture.failed.foreach { e =>
    onError(s"call-style audio output could not start: ${e.getMessage}")
  }

  /** Where sound meant to be heard is connected. */
  def input: AudioNode = stream

  def close(): Unit =
    val _ = element.pause()
    element.srcObject = null
