package converseapp

import conversecore.*

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout
import scala.util.{Failure, Success}

/** Owns audio output for the tab: splits text into speech chunks (markdown
  * stripped), synthesizes them via browser speechSynthesis or the server TTS
  * endpoint, and plays them. At most one playback at a time; starting a new
  * one stops the current. Session-agnostic: callers drive their own state
  * from the returned future and `onChange` notifications.
  */
final class Player(onChange: () => Unit):
  private val TtsPath = "/api/v1/plugins/converse/http/tts"

  private var ttsMode: String = "browser"
  // Playback routes through this context; the owner creates it inside a user
  // gesture so autoplay policies do not block it. While null, server-TTS
  // playback is silently skipped (browser speechSynthesis is unaffected).
  private var audioContext: AudioContext = null
  private var speakingActive: Boolean    = false
  private var currentThread: String      = null
  private var generation: Int            = 0
  private var currentAudio: js.Dynamic   = null
  private var abortCtl: js.Dynamic       = null

  private def debug(message: String): Unit =
    val _ = js.Dynamic.global.console.debug(s"[converse] $message")

  def setTtsMode(mode: String): Unit = ttsMode = mode

  def setAudioContext(ctx: AudioContext): Unit = audioContext = ctx

  def isSpeaking: Boolean = speakingActive

  /** The thread whose text is playing, or null when nothing is. */
  def speakingThread: String = currentThread

  /** Stops any current playback and speaks `text`. The future resolves true
    * when this playback ran to completion (or the text had nothing to say),
    * false when a later play()/stop() superseded it, and fails only when this
    * playback was still current and synthesis or playback failed.
    */
  def play(text: String, sourceThread: String): Future[Boolean] =
    stop()
    val chunks = Speech.splitSpeechChunks(text)
    if chunks.isEmpty then Future.successful(true)
    else
      generation += 1
      val gen = generation
      speakingActive = true
      currentThread = sourceThread
      onChange()
      val settled = Promise[Boolean]()
      val done    = if ttsMode == "server" then speakServer(gen, chunks) else speakBrowser(gen, chunks)
      done.onComplete {
        case Success(_) if generation == gen =>
          speakingActive = false
          currentThread = null
          onChange()
          settled.success(true)
        case Failure(e) if generation == gen =>
          debug(s"speech failed: ${e.getMessage}")
          speakingActive = false
          currentThread = null
          onChange()
          settled.failure(e)
        case _ => settled.success(false)
      }
      settled.future

  def stop(): Unit =
    generation += 1
    val wasActive = speakingActive
    speakingActive = false
    currentThread = null
    if abortCtl != null then { val _ = abortCtl.abort(); abortCtl = null }
    if !js.isUndefined(js.Dynamic.global.speechSynthesis) then { val _ = js.Dynamic.global.speechSynthesis.cancel() }
    if currentAudio != null then
      // An AudioBufferSourceNode stops with stop(); its onended settles the
      // playback future.
      try { val _ = currentAudio.stop() }
      catch { case _: Throwable => () }
      currentAudio = null
    if wasActive then onChange()

  private def delay(seconds: Double): Future[Unit] =
    if seconds <= 0 then Future.unit
    else
      val p = Promise[Unit]()
      val _ = setTimeout(seconds * 1000) { val _ = p.success(()) }
      p.future

  private def speakBrowser(gen: Int, chunks: List[SpeechChunk]): Future[Unit] =
    chunks.foldLeft(Future.unit) { (acc, chunk) =>
      acc.flatMap { _ =>
        if generation != gen then Future.unit
        else
          val p         = Promise[Unit]()
          val utterance = js.Dynamic.newInstance(js.Dynamic.global.SpeechSynthesisUtterance)(chunk.text)
          utterance.onend = js.Any.fromFunction1((_: js.Any) => { val _ = p.success(()) })
          utterance.onerror = js.Any.fromFunction1((_: js.Any) => { val _ = p.success(()) })
          val _ = js.Dynamic.global.speechSynthesis.speak(utterance)
          p.future.flatMap(_ => delay(chunk.pauseSeconds))
      }
    }

  private def fetchTts(text: String, signal: js.Any): Future[js.Dynamic] =
    js.Dynamic.global
      .fetch(
        TtsPath,
        js.Dynamic.literal(
          "method"  -> "POST",
          "headers" -> js.Dynamic.literal("content-type" -> "application/json"),
          "body"    -> js.JSON.stringify(js.Dynamic.literal("text" -> text)),
          "signal"  -> signal
        )
      )
      .asInstanceOf[js.Promise[js.Dynamic]]
      .toFuture
      .flatMap { response =>
        if !response.ok.asInstanceOf[Boolean] then
          Future.failed(new RuntimeException(s"TTS endpoint returned ${response.status}"))
        else response.blob().asInstanceOf[js.Promise[js.Dynamic]].toFuture
      }

  /** Plays through the owner-supplied AudioContext (created inside a user
    * gesture), which is exempt from autoplay blocking — a standalone
    * Audio.play() minutes after the last user gesture gets rejected by
    * Firefox's autoplay policy and would silently skip every chunk.
    */
  private def playBlob(gen: Int, blob: js.Dynamic): Future[Unit] =
    if generation != gen || audioContext == null then Future.unit
    else
      val ctx = audioContext.asInstanceOf[js.Dynamic]
      blob
        .arrayBuffer()
        .asInstanceOf[js.Promise[js.Any]]
        .toFuture
        .flatMap { bytes =>
          ctx.decodeAudioData(bytes).asInstanceOf[js.Promise[js.Dynamic]].toFuture
        }
        .flatMap { decoded =>
          if generation != gen || audioContext == null then Future.unit
          else
            val p      = Promise[Unit]()
            val source = ctx.createBufferSource()
            source.buffer = decoded
            val _ = source.connect(ctx.destination)
            currentAudio = source
            source.onended = js.Any.fromFunction1 { (_: js.Any) =>
              if currentAudio eq source then currentAudio = null
              if !p.isCompleted then { val _ = p.success(()) }
            }
            val _ = source.start()
            p.future
        }
        .recover { case e => debug(s"playback failed: ${e.getMessage}") }

  /** Sequential playback with a one-chunk synthesis prefetch. */
  private def speakServer(gen: Int, chunks: List[SpeechChunk]): Future[Unit] =
    val ctl = js.Dynamic.newInstance(js.Dynamic.global.AbortController)()
    abortCtl = ctl
    val all = chunks.toVector
    def go(i: Int, fetched: Future[js.Dynamic]): Future[Unit] =
      fetched.flatMap { blob =>
        if generation != gen then Future.unit
        else
          val next =
            if i + 1 < all.length then fetchTts(all(i + 1).text, ctl.signal) else null
          playBlob(gen, blob)
            .flatMap(_ => delay(all(i).pauseSeconds))
            .flatMap { _ =>
              if next == null || generation != gen then Future.unit else go(i + 1, next)
            }
      }
    val result = if all.isEmpty then Future.unit else go(0, fetchTts(all(0).text, ctl.signal))
    result.andThen { case _ => if abortCtl eq ctl then abortCtl = null }
