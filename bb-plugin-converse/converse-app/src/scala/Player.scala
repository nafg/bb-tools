package converseapp

import conversecore.*

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout
import scala.util.{Failure, Success}

/** Owns audio output for the tab: splits text into speech chunks (markdown
  * stripped), synthesizes them via browser speechSynthesis or the server TTS
  * endpoint, and plays them. Agent replies queue across threads; an explicit
  * replay replaces that queue; while held, queued speech waits without
  * affecting playback already underway. Session-agnostic: callers drive their
  * own state from the returned future and `onChange` notifications.
  */
final class Player(
    rpcCall: js.Function2[String, js.Any, js.Promise[js.Dynamic]],
    onChange: () => Unit,
    // Each reply that was heard, once it ends or is cut off: its thread,
    // whether it was session speech or a replay, and when its audio first
    // played and stopped.
    onPlayback: (String, ActivitySource, Double, Double) => Unit
):
  private final case class QueuedSpeech(
      text: String,
      sourceThread: String,
      source: ActivitySource,
      settled: Promise[Boolean]
  )

  private val TtsPath = "/api/v1/plugins/converse/http/tts"

  private var ttsMode: String = "browser"
  // Playback routes through this context; the owner creates it inside a user
  // gesture so autoplay policies do not block it. While null, server-TTS
  // playback is silently skipped (browser speechSynthesis is unaffected).
  private var audioContext: AudioContext = null
  private var speakingActive: Boolean    = false
  private var held: Boolean              = false
  private var currentThread: String      = null
  private var currentVoice: String       = null
  private var currentSentence: String    = null
  private var generation: Int            = 0
  private var currentAudio: js.Dynamic   = null
  private var abortCtl: js.Dynamic       = null
  private var currentSettled: Promise[Boolean] = null
  private val queuedSpeech = mutable.Queue.empty[QueuedSpeech]
  // When the current reply's audio first played; NaN until it does.
  private var playbackStartedAt: Double = Double.NaN
  // Reports the current reply's playback span, at most once.
  private var finishPlayback: () => Unit = null

  private def debug(message: String): Unit =
    val _ = js.Dynamic.global.console.debug(s"[converse] $message")

  def setTtsMode(mode: String): Unit = ttsMode = mode

  private var output: AudioOutput = null

  /** Must be called inside a user gesture, so playback is allowed to start. */
  def setAudioContext(ctx: AudioContext): Unit =
    if output != null then output.close()
    audioContext = ctx
    output = if ctx == null then null else AudioOutput.forThisDevice(ctx, debug)

  def isSpeaking: Boolean = speakingActive

  def isHeld: Boolean = held

  def setHeld(value: Boolean): Unit =
    held = value
    if !held then startNextQueuedSpeech()

  /** The threads with speech queued that has not started playing. */
  def queuedThreads: Set[String] = queuedSpeech.iterator.map(_.sourceThread).toSet

  /** The thread whose text is playing, or null when nothing is. */
  def speakingThread: String = currentThread

  /** The chunk of the reply being spoken right now, or null when nothing is. */
  def speakingSentence: String = currentSentence

  /** The concrete persisted voice used by the current response, or null when
    * browser speech/default provider playback is active.
    */
  def speakingVoice: String = currentVoice

  /** Adds speech to the tab-wide FIFO queue. The future resolves when this
    * particular response finishes, or false if stop()/play() clears it.
    */
  def enqueue(text: String, sourceThread: String): Future[Boolean] =
    enqueueAs(text, sourceThread, ActivitySource.Session)

  /** Explicit replay is immediate: it clears queued agent speech first. */
  def play(text: String, sourceThread: String): Future[Boolean] =
    stop()
    enqueueAs(text, sourceThread, ActivitySource.Replay)

  private def enqueueAs(text: String, sourceThread: String, source: ActivitySource): Future[Boolean] =
    if Speech.splitSpeechChunks(text).isEmpty then Future.successful(true)
    else
      val settled = Promise[Boolean]()
      queuedSpeech.enqueue(QueuedSpeech(text, sourceThread, source, settled))
      startNextQueuedSpeech()
      settled.future

  /** Marks the current reply's audio as playing, the first time. */
  private def markPlaying(gen: Int): Unit =
    if generation == gen && playbackStartedAt.isNaN then playbackStartedAt = js.Date.now()

  private def startNextQueuedSpeech(): Unit =
    if !speakingActive && !held && queuedSpeech.nonEmpty then
      val item   = queuedSpeech.dequeue()
      val chunks = Speech.splitSpeechChunks(item.text)
      generation += 1
      val gen = generation
      speakingActive = true
      currentThread = item.sourceThread
      currentVoice = null
      currentSettled = item.settled
      playbackStartedAt = Double.NaN
      var reported = false
      finishPlayback = () =>
        if !reported then
          reported = true
          // A reply that never started playing (stopped while still fetching)
          // was not heard, so it has no span.
          if !playbackStartedAt.isNaN then onPlayback(item.sourceThread, item.source, playbackStartedAt, js.Date.now())
      onChange()
      val done = resolveThreadVoice(item.sourceThread).flatMap { voice =>
        if generation != gen then Future.unit
        else
          currentVoice = voice
          onChange()
          if ttsMode == "server" then speakServer(gen, chunks, voice) else speakBrowser(gen, chunks, voice)
      }
      done.onComplete {
        case Success(_) if generation == gen =>
          finishPlayback()
          speakingActive = false
          currentThread = null
          currentVoice = null
          currentSentence = null
          if currentSettled eq item.settled then currentSettled = null
          if queuedSpeech.nonEmpty then startNextQueuedSpeech() else onChange()
          val _ = item.settled.trySuccess(true)
        case Failure(e) if generation == gen =>
          debug(s"speech failed: ${e.getMessage}")
          finishPlayback()
          speakingActive = false
          currentThread = null
          currentVoice = null
          currentSentence = null
          if currentSettled eq item.settled then currentSettled = null
          if queuedSpeech.nonEmpty then startNextQueuedSpeech() else onChange()
          val _ = item.settled.tryFailure(e)
        case _ =>
          val _ = item.settled.trySuccess(false)
      }

  def stop(): Unit =
    if finishPlayback != null then finishPlayback()
    generation += 1
    queuedSpeech.dequeueAll(_ => true).foreach(_.settled.success(false))
    if currentSettled != null then
      val _ = currentSettled.trySuccess(false)
      currentSettled = null
    val wasActive = speakingActive
    speakingActive = false
    currentThread = null
    currentVoice = null
    currentSentence = null
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

  private def speakBrowser(gen: Int, chunks: List[SpeechChunk], voice: String): Future[Unit] =
    chunks.foldLeft(Future.unit) { (acc, chunk) =>
      acc.flatMap { _ =>
        if generation != gen then Future.unit
        else
          val p         = Promise[Unit]()
          val utterance = js.Dynamic.newInstance(js.Dynamic.global.SpeechSynthesisUtterance)(chunk.text)
          if voice != null then
            val voices = js.Dynamic.global.speechSynthesis.getVoices().asInstanceOf[js.Array[js.Dynamic]]
            voices.find(v => v.voiceURI.asInstanceOf[String] == voice || v.name.asInstanceOf[String] == voice)
              .foreach(v => utterance.voice = v)
          currentSentence = chunk.text
          utterance.onstart = js.Any.fromFunction1((_: js.Any) => markPlaying(gen))
          utterance.onend = js.Any.fromFunction1((_: js.Any) => { val _ = p.success(()) })
          utterance.onerror = js.Any.fromFunction1((_: js.Any) => { val _ = p.success(()) })
          val _ = js.Dynamic.global.speechSynthesis.speak(utterance)
          p.future.flatMap(_ => delay(chunk.pauseSeconds))
      }
    }

  private def resolveThreadVoice(threadId: String): Future[String] =
    rpcCall("resolveThreadVoice", js.Dynamic.literal("threadId" -> threadId))
      .toFuture
      .map(result => result.voice.asInstanceOf[js.UndefOr[String]].getOrElse(null))
      .recover { case e =>
        debug(s"voice assignment failed: ${e.getMessage}")
        null
      }

  private def fetchTts(text: String, voice: String, signal: js.Any): Future[js.Dynamic] =
    val body = js.Dynamic.literal("text" -> text)
    if voice != null then body.updateDynamic("voice")(voice)
    js.Dynamic.global
      .fetch(
        TtsPath,
        js.Dynamic.literal(
          "method"  -> "POST",
          "headers" -> js.Dynamic.literal("content-type" -> "application/json"),
          "body"    -> js.JSON.stringify(body),
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
            val _ = source.connect(output.input)
            currentAudio = source
            markPlaying(gen)
            source.onended = js.Any.fromFunction1 { (_: js.Any) =>
              if currentAudio eq source then currentAudio = null
              if !p.isCompleted then { val _ = p.success(()) }
            }
            val _ = source.start()
            p.future
        }
        .recover { case e => debug(s"playback failed: ${e.getMessage}") }

  /** Sequential playback with a one-chunk synthesis prefetch. */
  private def speakServer(gen: Int, chunks: List[SpeechChunk], voice: String): Future[Unit] =
    val ctl = js.Dynamic.newInstance(js.Dynamic.global.AbortController)()
    abortCtl = ctl
    val all = chunks.toVector
    def go(i: Int, fetched: Future[js.Dynamic]): Future[Unit] =
      fetched.flatMap { blob =>
        if generation != gen then Future.unit
        else
          val next =
            if i + 1 < all.length then fetchTts(all(i + 1).text, voice, ctl.signal) else null
          currentSentence = all(i).text
          playBlob(gen, blob)
            .flatMap(_ => delay(all(i).pauseSeconds))
            .flatMap { _ =>
              if next == null || generation != gen then Future.unit else go(i + 1, next)
            }
      }
    val result = if all.isEmpty then Future.unit else go(0, fetchTts(all(0).text, voice, ctl.signal))
    result.andThen { case _ => if abortCtl eq ctl then abortCtl = null }
