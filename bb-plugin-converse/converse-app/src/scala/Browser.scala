package converseapp

import scala.scalajs.js
import scala.annotation.unused
import scala.scalajs.js.annotation.{JSGlobal, JSName}
import scala.scalajs.js.typedarray.Float32Array

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
  def createAnalyser(): AnalyserNode                          = js.native
  def close(): js.Promise[Unit]                               = js.native
  def resume(): js.Promise[Unit]                              = js.native
  val state: String                                           = js.native

@js.native
trait AudioNode extends js.Object:
  def connect(node: AudioNode): AudioNode = js.native

@js.native
trait AnalyserNode extends AudioNode:
  var fftSize: Int                                     = js.native
  def getFloatTimeDomainData(array: Float32Array): Unit = js.native

@js.native
@JSGlobal
class MediaRecorder(@unused stream: MediaStream, @unused options: js.Any) extends js.Object:
  def start(): Unit                                    = js.native
  def stop(): Unit                                     = js.native
  def requestData(): Unit                              = js.native
  val state: String                                    = js.native
  var ondataavailable: js.Function1[js.Dynamic, Unit]  = js.native
  var onstop: js.Function1[js.Dynamic, Unit]           = js.native
  var onerror: js.Function1[js.Dynamic, Unit]          = js.native

@js.native
@JSGlobal("MediaRecorder")
object MediaRecorderStatic extends js.Object:
  def isTypeSupported(mimeType: String): Boolean = js.native
