package converseapp

import scala.scalajs.js
import scala.annotation.unused
import scala.scalajs.js.annotation.{JSGlobal, JSName}

// Handwritten facades for the subset of browser audio APIs this plugin uses,
// in the same style as bb-plugin-facades.

@js.native
trait MediaStream extends js.Object:
  def getTracks(): js.Array[MediaStreamTrack] = js.native
  @JSName("clone")
  def cloneStream(): MediaStream = js.native

@js.native
trait MediaStreamTrack extends js.Object:
  def stop(): Unit = js.native

@js.native
@JSGlobal
class AudioContext extends js.Object:
  def createMediaStreamSource(stream: MediaStream): AudioNode = js.native
  def createDelay(maxDelayTime: Double): DelayNode            = js.native
  def createMediaStreamDestination(): MediaStreamAudioDestinationNode = js.native
  def createOscillator(): OscillatorNode                      = js.native
  def createGain(): GainNode                                  = js.native
  def createBiquadFilter(): BiquadFilterNode                  = js.native
  def createBuffer(channels: Int, length: Int, sampleRate: Double): AudioBuffer = js.native
  def createBufferSource(): AudioBufferSourceNode             = js.native
  val sampleRate: Double                                      = js.native
  def close(): js.Promise[Unit]                               = js.native
  def resume(): js.Promise[Unit]                              = js.native
  val state: String                                           = js.native
  val currentTime: Double                                     = js.native
  val destination: AudioNode                                  = js.native

@js.native
trait AudioNode extends js.Object:
  def connect(node: AudioNode): AudioNode = js.native

@js.native
trait AudioParam extends js.Object:
  var value: Double                                                  = js.native
  def setValueAtTime(value: Double, startTime: Double): AudioParam   = js.native
  def linearRampToValueAtTime(value: Double, endTime: Double): AudioParam = js.native
  def cancelScheduledValues(startTime: Double): AudioParam           = js.native

@js.native
trait GainNode extends AudioNode:
  val gain: AudioParam = js.native

@js.native
trait AudioScheduledSourceNode extends AudioNode:
  def start(when: Double): Unit                = js.native
  def stop(when: Double): Unit                 = js.native
  var onended: js.Function1[js.Dynamic, Unit]  = js.native

@js.native
trait OscillatorNode extends AudioScheduledSourceNode:
  val frequency: AudioParam = js.native

@js.native
trait AudioBuffer extends js.Object:
  def getChannelData(channel: Int): scala.scalajs.js.typedarray.Float32Array = js.native

@js.native
trait AudioBufferSourceNode extends AudioScheduledSourceNode:
  var buffer: AudioBuffer = js.native
  var loop: Boolean       = js.native

@js.native
trait BiquadFilterNode extends AudioNode:
  var `type`: String        = js.native
  val frequency: AudioParam = js.native

@js.native
trait DelayNode extends AudioNode:
  val delayTime: AudioParam = js.native

@js.native
trait MediaStreamAudioDestinationNode extends AudioNode:
  val stream: MediaStream = js.native

@js.native
@JSGlobal
class MediaRecorder(@unused stream: MediaStream, @unused options: js.Any) extends js.Object:
  def start(): Unit                                    = js.native
  def stop(): Unit                                     = js.native
  val state: String                                    = js.native
  var ondataavailable: js.Function1[js.Dynamic, Unit]  = js.native
  var onstop: js.Function1[js.Dynamic, Unit]           = js.native
  var onerror: js.Function1[js.Dynamic, Unit]          = js.native

@js.native
@JSGlobal("MediaRecorder")
object MediaRecorderStatic extends js.Object:
  def isTypeSupported(mimeType: String): Boolean = js.native
