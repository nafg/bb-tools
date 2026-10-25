package converseapp

import scala.concurrent.Future
import scala.scalajs.js

/** How call-style output gets an AudioContext's sound to its audio element. */
enum CallTransport:
  /** The context's media stream played straight by the element. Chromium
    * plays a local media stream at a varying rate, which shifted replies'
    * pitch by up to nearly a semitone.
    */
  case Direct

  /** Sent through a WebRTC connection within the page and the received track
    * played, as a web call plays the other side; a test tone played this way
    * held its pitch. On one Android phone it played nothing until the browser
    * was restarted.
    */
  case WebRtc

object CallTransport:
  /** The transport a setting value names; direct for anything else. */
  def fromSetting(value: String): CallTransport = if value == "webrtc" then WebRtc else Direct

/** An AudioContext's sound played by an audio element, the way a web page
  * plays the other side of a call. On Android, a page using a Bluetooth
  * headset's microphone puts the headset in call mode, and the AudioContext's
  * own output then went to the phone's speaker instead of the headset; call
  * audio goes to the headset.
  *
  * Create it inside a user gesture, since the element must be allowed to play.
  */
final class CallAudioOutput(ctx: AudioContext, transport: CallTransport, onError: String => Unit) extends AudioOutput:
  private val stream    = ctx.createMediaStreamDestination()
  private val element   = js.Dynamic.newInstance(js.Dynamic.global.Audio)()
  private val sending   = js.Dynamic.newInstance(js.Dynamic.global.RTCPeerConnection)()
  private val receiving = js.Dynamic.newInstance(js.Dynamic.global.RTCPeerConnection)()
  private var startFailure: Option[String] = None

  // Started now, inside the gesture, on the stream itself (which is silent
  // until something plays); through WebRTC, the connection's track replaces
  // it when it comes.
  element.srcObject = stream.stream
  play()
  if transport == CallTransport.WebRtc then connect().failed.foreach(e => failed(s"could not connect: ${e.getMessage}"))

  private def play(): Unit =
    element.play().asInstanceOf[js.Promise[Unit]].toFuture.failed.foreach(e => failed(s"could not play: ${e.getMessage}"))

  private def failed(message: String): Unit =
    startFailure = Some(message)
    onError(s"call-style audio output $message")

  private def connect(): Future[Unit] =
    sending.onicecandidate = (e: js.Dynamic) => if e.candidate != null then receiving.addIceCandidate(e.candidate)
    receiving.onicecandidate = (e: js.Dynamic) => if e.candidate != null then sending.addIceCandidate(e.candidate)
    receiving.ontrack = (e: js.Dynamic) =>
      element.srcObject = js.Dynamic.newInstance(js.Dynamic.global.MediaStream)(js.Array(e.track))
      play()
    stream.stream.getTracks().foreach(track => sending.addTrack(track, stream.stream))
    for
      offer  <- promise(sending.createOffer())
      _      <- promise(sending.setLocalDescription(withFullQuality(offer)))
      _      <- promise(receiving.setRemoteDescription(withFullQuality(offer)))
      answer <- promise(receiving.createAnswer())
      _      <- promise(receiving.setLocalDescription(withFullQuality(answer)))
      _      <- promise(sending.setRemoteDescription(withFullQuality(answer)))
    yield ()

  private def promise(p: js.Dynamic): Future[js.Dynamic] = p.asInstanceOf[js.Promise[js.Dynamic]].toFuture

  /** The description with Opus at its highest bitrate, so speech is not
    * audibly compressed on its way through.
    */
  private def withFullQuality(description: js.Dynamic): js.Dynamic =
    val sdp = description.sdp.asInstanceOf[String]
    js.Dynamic.literal(
      "type" -> description.`type`,
      "sdp"  -> CallAudioOutput.OpusFormatLine.replaceAllIn(sdp, m => s"${m.matched};maxaveragebitrate=510000")
    )

  def input: AudioNode = stream

  def report: String =
    val element = s"element ${if this.element.paused.asInstanceOf[Boolean] then "paused" else "playing"}"
    val how = transport match
      case CallTransport.Direct => s"call-style (a media stream played by an audio element); $element"
      case CallTransport.WebRtc =>
        "call-style (sent through a WebRTC connection, played by an audio element)" +
          s"; connection ${receiving.connectionState}, $element" +
          s", ${if this.element.srcObject eq stream.stream then "on the local stream" else "on the received track"}"
    how + startFailure.fold("")(e => s"; $e")

  def close(): Unit =
    val _ = element.pause()
    element.srcObject = null
    val _ = sending.close()
    val _ = receiving.close()

object CallAudioOutput:
  /** The format parameters line of Opus, which Chromium offers as payload 111. */
  private val OpusFormatLine = """a=fmtp:111 [^\r\n]*""".r
