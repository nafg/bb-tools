package converseapp

import conversecore.*

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.typedarray.*
import scala.util.{Failure, Success}

/** One chunk of speech whose 16-bit PCM WAV audio arrives as a stream. It is
  * read and decoded from the moment it is created, held until `play`, then
  * scheduled piece after piece without gaps as more arrives, so it starts
  * playing long before it has all been generated.
  */
final class StreamedSpeech(ctx: AudioContext, output: AudioNode, body: js.Dynamic):
  import StreamedSpeech.*

  private val reader     = body.getReader()
  private var wav        = WavStream.start
  private var resamplers = Vector.empty[StreamResampler]
  // Decoded audio at the context's rate, one buffer per channel, not yet
  // scheduled.
  private var pending          = Vector.empty[mutable.ArrayBuffer[Float]]
  // Nothing more is coming: the stream ended, or failed with `failure`.
  private var allArrived       = false
  private var failure          = Option.empty[Throwable]
  private var playing          = false
  private var nextStartTime    = 0.0
  private var onFirstSound: Double => Unit = _ => ()
  private val scheduled        = mutable.Set.empty[AudioBufferSourceNode]
  private val arrivedPromise   = Promise[Unit]()
  private val finishedPromise  = Promise[Unit]()

  /** Completes once the whole stream has been received. */
  val arrived: Future[Unit] = arrivedPromise.future

  readNext()

  /** Starts playing what has arrived and what is still to come. Completes
    * once all of it has played; `onFirstSound` gets the time (as Date.now())
    * its first sound is due.
    */
  def play(onFirstSound: Double => Unit): Future[Unit] =
    this.onFirstSound = onFirstSound
    playing = true
    scheduleReady()
    finishedPromise.future

  /** Stops reading and silences everything scheduled. */
  def stop(): Unit =
    playing = false
    try { val _ = reader.cancel() }
    catch { case _: Throwable => () }
    scheduled.foreach(source => try source.stop(0) catch { case _: Throwable => () })
    scheduled.clear()
    val _ = finishedPromise.trySuccess(())

  private def readNext(): Unit =
    val _ = reader.read().asInstanceOf[js.Promise[js.Dynamic]].toFuture.onComplete {
      case Success(result) if result.done.asInstanceOf[Boolean] =>
        resamplers.zipWithIndex.foreach((resampler, channel) => pending(channel) ++= resampler.finish)
        allArrived = true
        arrivedPromise.trySuccess(())
        scheduleReady()
      case Success(result) =>
        val chunk = result.value.asInstanceOf[Uint8Array]
        val bytes = IArray.unsafeFromArray(new Int8Array(chunk.buffer, chunk.byteOffset, chunk.length).toArray)
        wav.feed(bytes) match
          case Left(message) => fail(new RuntimeException(message))
          case Right((next, channels)) =>
            wav = next
            wav match
              case WavStream.InSamples(format, _) if resamplers.isEmpty =>
                resamplers = Vector.fill(format.channels)(StreamResampler(format.sampleRate, ctx.sampleRate.toInt))
                pending = Vector.fill(format.channels)(mutable.ArrayBuffer.empty[Float])
              case _ => ()
            if channels.nonEmpty then
              resamplers = resamplers.zip(channels).zip(pending).map { case ((resampler, samples), held) =>
                val (next, resampled) = resampler.feed(samples)
                held ++= resampled
                next
              }
            scheduleReady()
            readNext()
      case Failure(e) => fail(e)
    }

  /** What arrived before the failure still plays; only then does playing
    * fail, so the next chunk does not start over it.
    */
  private def fail(e: Throwable): Unit =
    failure = Some(e)
    allArrived = true
    val _ = arrivedPromise.tryFailure(e)
    scheduleReady()

  private def pendingSeconds: Double = pending.headOption.fold(0.0)(_.length / ctx.sampleRate)

  /** Schedules what has arrived, once there is enough not to run dry before
    * the next piece comes (or nothing more is coming).
    */
  private def scheduleReady(): Unit =
    if playing then
      val enough = if scheduled.isEmpty then StartAfterSeconds else PieceSeconds
      if pendingSeconds >= enough || (allArrived && pendingSeconds > 0) then schedulePending()
      if allArrived && pendingSeconds == 0 && scheduled.isEmpty then
        val _ = failure.fold(finishedPromise.trySuccess(()))(finishedPromise.tryFailure)

  private def schedulePending(): Unit =
    val frames = pending.head.length
    val buffer = ctx.createBuffer(pending.length, frames, ctx.sampleRate)
    pending.zipWithIndex.foreach { (samples, channel) =>
      val target = buffer.getChannelData(channel)
      var i      = 0
      while i < frames do
        target(i) = samples(i)
        i += 1
      samples.clear()
    }
    val source = ctx.createBufferSource()
    source.buffer = buffer
    val _ = source.connect(output)
    // Back to back with the previous piece, unless it already ran out.
    val startTime = math.max(nextStartTime, ctx.currentTime + LeadSeconds)
    if scheduled.isEmpty && nextStartTime == 0 then
      onFirstSound(js.Date.now() + (startTime - ctx.currentTime) * 1000)
    nextStartTime = startTime + frames / ctx.sampleRate
    scheduled += source
    source.onended = (_: js.Dynamic) =>
      scheduled -= source
      scheduleReady()
    source.start(startTime)

object StreamedSpeech:
  /** Audio held before playing starts, so the stream can stay ahead. Pocket
    * TTS generated about 3.7 times faster than real time when measured.
    */
  private val StartAfterSeconds = 0.3

  /** How much audio each scheduled piece holds, at least. */
  private val PieceSeconds = 0.1

  /** How far ahead of now a piece is scheduled, so it is not late. */
  private val LeadSeconds = 0.05
