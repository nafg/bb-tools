package converseapp

import conversecore.*

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.typedarray.Float32Array

enum SpeechDetectorEvent:
  case CandidateStart
  case CandidateAbandoned(evidence: SpeechDetectorEvidence)
  case SpeechStart
  case PreviewPoint
  case SpeechEnd(recording: js.Dynamic, evidence: SpeechDetectorEvidence)

final case class SpeechDetectorFrame(rms: Double, speechProbability: Double)

/** Scala.js owner of the Silero VAD lifecycle and all frame-derived speech
  * state. TypeScript only supplies the npm package constructor and WAV encoder
  * at the frontend entry point.
  */
final class SpeechDetector private (
    vad: js.Dynamic
)(using ExecutionContext):
  private var destroyed       = false
  private var forcedBoundary  = false
  private var flushInFlight: Future[Unit] = null

  def flush(): Future[Unit] =
    if destroyed then Future.unit
    else if flushInFlight != null then flushInFlight
    else
      forcedBoundary = true
      val _ = vad.setOptions(js.Dynamic.literal("submitUserSpeechOnPause" -> true))
      val flushing = vad.pause().asInstanceOf[js.Promise[Unit]].toFuture.flatMap { _ =>
        val _ = vad.setOptions(js.Dynamic.literal("submitUserSpeechOnPause" -> false))
        if destroyed then Future.unit
        else vad.start().asInstanceOf[js.Promise[Unit]].toFuture
      }
      val completion = Promise[Unit]()
      flushInFlight = completion.future
      flushing.onComplete { outcome =>
        forcedBoundary = false
        flushInFlight = null
        val _ = completion.complete(outcome)
      }
      flushInFlight

  def destroy(): Future[Unit] =
    if destroyed then Future.unit
    else
      destroyed = true
      val _ = vad.setOptions(js.Dynamic.literal("submitUserSpeechOnPause" -> false))
      vad.destroy().asInstanceOf[js.Promise[Unit]].toFuture

  private[converseapp] def isDestroyed: Boolean = destroyed
  private[converseapp] def isForcedBoundary: Boolean = forcedBoundary

object SpeechDetector:
  private val SampleRate                 = 16000
  private val FrameSamples               = 512
  private val PreSpeechPadMs             = 1000
  private val MinSpeechMs                = 192
  private val PreviewPauseMs             = 640
  private val TrailingContextMs          = 320
  private val VadAssetPath               = "https://cdn.jsdelivr.net/npm/@ricky0123/vad-web@0.0.30/dist/"
  private val OnnxWasmPath               = "https://cdn.jsdelivr.net/npm/onnxruntime-web@1.27.0/dist/"

  def create(
      stream: MediaStream,
      audioContext: AudioContext,
      positiveSpeechThreshold: Double,
      endOfSpeechSilenceMs: Double,
      createMicVad: js.Function1[js.Dynamic, js.Promise[js.Dynamic]],
      encodeWav: js.Function1[Float32Array, js.Any],
      onFrame: SpeechDetectorFrame => Unit,
      onEvent: SpeechDetectorEvent => Unit
  )(using ExecutionContext): Future[SpeechDetector] =
    val negativeSpeechThreshold = math.max(0.05, positiveSpeechThreshold - 0.05)
    val state = SpeechDetectorSegmentState(
      SpeechDetectorSegmentStateConfig(
        sampleRate = SampleRate,
        frameSamples = FrameSamples,
        positiveSpeechThreshold = positiveSpeechThreshold,
        negativeSpeechThreshold = negativeSpeechThreshold,
        previewPauseMilliseconds = PreviewPauseMs,
        trailingContextMilliseconds = TrailingContextMs
      )
    )
    var detector: SpeechDetector = null
    var latestSpeechProbability  = 0.0
    var latestRms                = 0.0

    def acceptingCallbacks: Boolean = detector == null || !detector.isDestroyed

    val options = js.Dynamic.literal(
      "model"                      -> "v5",
      "audioContext"               -> audioContext,
      "getStream"                  -> js.Any.fromFunction0(() => js.Promise.resolve(stream)),
      "pauseStream"                -> js.Any.fromFunction1((_: MediaStream) => js.Promise.resolve(())),
      "resumeStream"               -> js.Any.fromFunction1((_: MediaStream) => js.Promise.resolve(stream)),
      "startOnLoad"                -> true,
      "processorType"              -> "auto",
      "baseAssetPath"              -> VadAssetPath,
      "onnxWASMBasePath"           -> OnnxWasmPath,
      "positiveSpeechThreshold"    -> positiveSpeechThreshold,
      "negativeSpeechThreshold"    -> negativeSpeechThreshold,
      "redemptionMs"               -> endOfSpeechSilenceMs,
      "preSpeechPadMs"             -> PreSpeechPadMs,
      "minSpeechMs"                -> MinSpeechMs,
      "submitUserSpeechOnPause"    -> false,
      "onFrameProcessed" -> js.Any.fromFunction2 { (probabilities: js.Dynamic, frame: Float32Array) =>
        if acceptingCallbacks then
          latestSpeechProbability = probabilities.isSpeech.asInstanceOf[Double]
          var sumOfSquares = 0.0
          var index        = 0
          while index < frame.length do
            val sample = frame(index).toDouble
            sumOfSquares += sample * sample
            index += 1
          latestRms = math.sqrt(sumOfSquares / frame.length)
          onFrame(SpeechDetectorFrame(latestRms, latestSpeechProbability))
          if state.processFrame(latestSpeechProbability, latestRms) then
            onEvent(SpeechDetectorEvent.PreviewPoint)
      },
      "onSpeechStart" -> js.Any.fromFunction0 { () =>
        if acceptingCallbacks then
          state.beginCandidate()
          // MicVAD emits FrameProcessed before SpeechStart for the triggering
          // frame, so account for that frame after opening the candidate.
          val _ = state.processFrame(latestSpeechProbability, latestRms)
          onEvent(SpeechDetectorEvent.CandidateStart)
      },
      "onSpeechRealStart" -> js.Any.fromFunction0 { () =>
        if acceptingCallbacks then
          state.confirmSpeech()
          onEvent(SpeechDetectorEvent.SpeechStart)
      },
      "onVADMisfire" -> js.Any.fromFunction0 { () =>
        if acceptingCallbacks then onEvent(SpeechDetectorEvent.CandidateAbandoned(state.evidence))
      },
      "onSpeechEnd" -> js.Any.fromFunction1 { (audio: Float32Array) =>
        if acceptingCallbacks then
          val forcedBoundary = detector != null && detector.isForcedBoundary
          val submittedEnd = state.submittedSampleEndExclusive(audio.length, forcedBoundary)
          val wav = encodeWav(audio.subarray(0, submittedEnd))
          val recording = js.Dynamic.newInstance(js.Dynamic.global.Blob)(
            js.Array(wav),
            js.Dynamic.literal("type" -> "audio/wav")
          )
          onEvent(SpeechDetectorEvent.SpeechEnd(recording, state.evidence))
      }
    )

    createMicVad(options).toFuture.map { vad =>
      detector = SpeechDetector(vad)
      detector
    }
