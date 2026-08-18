package converseapp

import conversecore.*

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.timers.{SetIntervalHandle, SetTimeoutHandle, clearInterval, clearTimeout, setInterval, setTimeout}
import scala.scalajs.js.typedarray.Float32Array
import scala.util.{Failure, Success}

given ExecutionContext = ExecutionContext.parasitic

/** One per browser tab. Owns the microphone, VAD, recording, upload, and
  * playback for a single active voice session bound to one thread; the React
  * composer components are thin views over this. The session survives
  * navigation: any mounted converse component relays realtime signals to this
  * singleton, which filters by session id.
  */
final class Controller(rpcCall: js.Function2[String, js.Any, js.Promise[js.Dynamic]]):
  private val TtsPath        = "/api/v1/plugins/converse/http/tts"
  private val FrameMs        = 30.0
  private val MaxUtteranceMs = 60_000.0

  // Session
  private var sessionId: String   = null
  private var threadId: String    = null
  private var phase: String       = "idle"
  private var errorNote: String   = null
  private var heardText: String   = null
  private var ttsMode: String     = "browser"
  private var lastEventId: String = null

  // Audio graph
  private var micStream: MediaStream        = null
  private var analysisStream: MediaStream   = null
  private var audioContext: AudioContext    = null
  private var analyser: AnalyserNode        = null
  private var frameBuffer: Float32Array     = null
  private var frameTimer: SetIntervalHandle = null
  private var vad: EnergyVad                = null
  private var vadThreshold: Double          = 0.01
  private var mimeType: String              = ""
  private var frameCount: Int               = 0
  private var currentLevel: Double          = 0.0

  // Recording
  private var recorder: MediaRecorder             = null
  private var recorderChunks: js.Array[js.Any]    = null
  private var recorderSubmit: Boolean             = false
  private var maxUtteranceTimer: SetTimeoutHandle = null
  private var previewRecorder: MediaRecorder      = null
  private var previewDiscard: Boolean             = false
  private var previewSegmentStart: Double         = 0
  private var previewInFlight: Boolean            = false
  private var lastPreviewAt: Double               = 0
  private var previewBackoffUntil: Double         = 0
  private var utteranceSeq: Int                   = 0
  private var utteranceTarget: String             = null
  private var interimText: String                 = null
  private var interimAccum: String                = ""

  // The thread the app route currently shows (reported by mounted converse
  // components); utterances are routed here.
  private var viewedThread: String = null

  // Latest effective settings, pushed by the React bridge so the chrome-level
  // widget can start a session without access to hooks.
  private var startOptions: js.Dynamic = js.Dynamic.literal()

  // Playback
  private var speakingActive: Boolean  = false
  private var speakingThread: String   = null
  private var generation: Int          = 0
  private var currentAudio: js.Dynamic = null
  private var abortCtl: js.Dynamic     = null

  // React 18 external-store contract: a stable snapshot object per change.
  private var listeners: List[js.Function0[Unit]] = Nil
  private var snapshot: js.Dynamic                = null
  refreshSnapshot()

  private def debug(message: String): Unit =
    val _ = js.Dynamic.global.console.debug(s"[converse] $message")

  private def refreshSnapshot(): Unit =
    // While an utterance is being captured or transcribed, the banner belongs
    // on the thread it is latched to, not the thread currently in view.
    val displayThread =
      if utteranceTarget != null && (recorder != null || phase == "transcribing") then utteranceTarget
      else threadId
    snapshot = js.Dynamic.literal(
      "phase"     -> phase,
      "threadId"  -> displayThread,
      "viewed"    -> viewedThread,
      "speakingThread" -> speakingThread,
      "error"     -> errorNote,
      "heard"     -> heardText,
      "interim"   -> interimText,
      "level"     -> currentLevel,
      "threshold" -> vadThreshold
    )
    listeners.foreach(_())

  private def setPhase(next: String): Unit =
    if phase != next then
      debug(s"phase: $phase -> $next")
      phase = next
      refreshSnapshot()

  def subscribe(cb: js.Function0[Unit]): js.Function0[Unit] =
    listeners = cb :: listeners
    () => listeners = listeners.filterNot(_ eq cb)

  def getSnapshot(): js.Dynamic = snapshot

  /** The thread currently in view per the app route. Mount order is not a
    * reliable proxy: bb keeps previously visited thread surfaces mounted, so
    * returning to a thread mounts nothing new and a stale thread would win.
    */
  def noteViewed(tid: String): Unit =
    if tid != null && tid != viewedThread then
      viewedThread = tid
      if sessionId != null then retarget() else refreshSnapshot()

  def configure(opts: js.Dynamic): Unit =
    startOptions = opts

  /** Start a session targeting the thread currently in view. */
  def startViewed(): Unit =
    if viewedThread == null then
      errorNote = "open a thread first"
      refreshSnapshot()
    else start(viewedThread, startOptions)

  private def currentTarget: String =
    if viewedThread != null then viewedThread else threadId

  private def retarget(): Unit =
    val target = currentTarget
    if target != null && target != threadId then
      debug(s"routing target -> $target")
      threadId = target
      refreshSnapshot()

  // ---------------------------------------------------------------- session

  def start(newThreadId: String, opts: js.Dynamic): Unit =
    stopInternal(notifyServer = true)
    // Created synchronously inside the click's user gesture: an AudioContext
    // created after an await can start (and stay) suspended under autoplay
    // policies, which reads as permanent silence.
    audioContext = new AudioContext
    val _    = audioContext.resume()
    val sess = js.Dynamic.global.crypto.randomUUID().asInstanceOf[String]
    sessionId = sess
    threadId = newThreadId
    errorNote = null
    heardText = null
    interimText = null
    lastEventId = null
    ttsMode = opts.ttsMode.asInstanceOf[js.UndefOr[String]].getOrElse("browser")
    vadThreshold = opts.vadThreshold.asInstanceOf[js.UndefOr[Double]].filter(t => t > 0).getOrElse(0.01)
    setPhase("starting")
    debug(s"starting session $sess for thread $newThreadId (tts=$ttsMode threshold=$vadThreshold)")
    val started =
      for
        _      <- rpcCall("startSession", js.Dynamic.literal("sessionId" -> sess, "threadId" -> newThreadId)).toFuture
        stream <- js.Dynamic.global.navigator.mediaDevices
                    .getUserMedia(
                      js.Dynamic.literal(
                        "audio" -> js.Dynamic.literal(
                          "echoCancellation" -> true,
                          "noiseSuppression" -> true,
                          "autoGainControl"  -> true
                        )
                      )
                    )
                    .asInstanceOf[js.Promise[MediaStream]]
                    .toFuture
      yield stream
    started.onComplete {
      case _ if sessionId != sess => // superseded while starting
      case Success(stream)        => beginListening(stream)
      case Failure(e)             =>
        debug(s"start failed: ${e.getMessage}")
        errorNote = s"could not start voice: ${e.getMessage}"
        sessionId = null
        threadId = null
        closeAudioContext()
        setPhase("error")
    }

  private def beginListening(stream: MediaStream): Unit =
    if audioContext == null then stream.getTracks().foreach(_.stop()) // stopped while starting
    else
      micStream = stream
      analysisStream = stream.cloneStream()
      audioContext.resume().toFuture.onComplete { result =>
        val state = if audioContext == null then "closed" else audioContext.state
        debug(s"audio context state after resume: $state ($result)")
        if state == "suspended" then
          errorNote = "the browser blocked audio processing (suspended AudioContext)"
          refreshSnapshot()
      }
      analyser = audioContext.createAnalyser()
      analyser.fftSize = 2048
      val _ = audioContext.createMediaStreamSource(analysisStream).connect(analyser)
      frameBuffer = new Float32Array(analyser.fftSize)
      vad = EnergyVad(VadConfig(threshold = vadThreshold))
      mimeType = List("audio/webm;codecs=opus", "audio/webm", "audio/mp4", "audio/ogg;codecs=opus")
        .find(MediaRecorderStatic.isTypeSupported)
        .getOrElse("")
      debug(s"listening (recorder mime: ${if mimeType.isEmpty then "(browser default)" else mimeType})")
      frameTimer = setInterval(FrameMs)(onFrame())
      retarget()
      setPhase("listening")

  def stop(): Unit =
    debug("stopped by user")
    stopInternal(notifyServer = true)
    errorNote = null
    setPhase("idle")

  private def closeAudioContext(): Unit =
    if audioContext != null then
      val ctx = audioContext
      audioContext = null
      ctx.close().toFuture.failed.foreach(_ => ())

  private def stopInternal(notifyServer: Boolean): Unit =
    if frameTimer != null then { clearInterval(frameTimer); frameTimer = null }
    if maxUtteranceTimer != null then { clearTimeout(maxUtteranceTimer); maxUtteranceTimer = null }
    discardRecorder()
    stopSpeaking()
    if micStream != null then { micStream.getTracks().foreach(_.stop()); micStream = null }
    if analysisStream != null then { analysisStream.getTracks().foreach(_.stop()); analysisStream = null }
    closeAudioContext()
    analyser = null
    frameBuffer = null
    vad = null
    heardText = null
    interimText = null
    currentLevel = 0.0
    if sessionId != null then
      val sess = sessionId
      sessionId = null
      threadId = null
      if notifyServer then
        rpcCall("stopSession", js.Dynamic.literal("sessionId" -> sess)).toFuture.failed.foreach(_ => ())

  // ----------------------------------------------------------------- frames

  private def onFrame(): Unit =
    if analyser != null && vad != null then
      analyser.getFloatTimeDomainData(frameBuffer)
      var sum = 0.0
      var i   = 0
      while i < frameBuffer.length do
        val s = frameBuffer(i).toDouble
        sum += s * s
        i += 1
      val rms = math.sqrt(sum / frameBuffer.length)
      currentLevel = rms
      frameCount += 1
      // ~6Hz level updates for the UI meter without a render per frame.
      if frameCount % 5 == 0 then refreshSnapshot()
      vad.pushFrame(rms).foreach(handleVadEvent)
      // Continuous speech never hits a pause, so also cut preview segments on
      // time; otherwise interim feedback only appears after the first pause.
      if previewRecorder != null && !previewInFlight && vad.isInSpeech
        && js.Date.now() >= previewBackoffUntil
        && js.Date.now() - previewSegmentStart >= 3500
      then previewRecorder.stop()

  private def handleVadEvent(event: VadEvent): Unit =
    debug(s"vad: $event (level=${(currentLevel * 1000).round / 1000.0})")
    event match
      case VadEvent.CandidateStart =>
        // Recording proceeds during playback too: echo cancellation keeps the
        // TTS out of the mic, and suppressing here silently ate anything said
        // at normal volume while a reply was speaking.
        startRecorder()
      case VadEvent.CandidateAbandoned =>
        if recorder != null && vad != null && !vad.isInSpeech then discardRecorder()
        if phase == "recording" then setPhase("listening")
      case VadEvent.SpeechStart =>
        startRecorder()
        utteranceTarget = currentTarget
        setPhase("recording")
      case VadEvent.Partial =>
        // Previews are best-effort extra transcription load; throttle them so
        // they can never rate-limit the provider out from under final submits.
        val now = js.Date.now()
        if previewRecorder != null && !previewInFlight && now >= previewBackoffUntil
          && now - lastPreviewAt >= 3000
        then
          // Stopping yields a complete, decodable segment file; onstop
          // transcribes it and starts the next segment recorder.
          previewRecorder.stop()
      case VadEvent.SpeechEnd(longEnough) =>
        if recorder != null then finishRecorder(submit = longEnough)
        else if phase == "recording" then setPhase("listening")
      case VadEvent.BargeIn =>
        if speakingActive then
          debug("barge-in: stopping playback")
          stopSpeaking()
          if phase == "speaking" then setPhase(if recorder != null then "recording" else "listening")

  // -------------------------------------------------------------- recording

  private def startRecorder(): Unit =
    if recorder == null && micStream != null then
      val chunks  = js.Array[js.Any]()
      val options = if mimeType.nonEmpty then js.Dynamic.literal("mimeType" -> mimeType) else js.Dynamic.literal()
      val r       = new MediaRecorder(micStream, options)
      recorderChunks = chunks
      recorderSubmit = false
      utteranceSeq += 1
      interimText = null
      interimAccum = ""
      r.ondataavailable = { e =>
        if e.data.size.asInstanceOf[Double] > 0 then { val _ = chunks.push(e.data.asInstanceOf[js.Any]) }
      }
      r.onstop = { _ =>
        if recorder eq r then
          recorder = null
          recorderChunks = null
          if maxUtteranceTimer != null then { clearTimeout(maxUtteranceTimer); maxUtteranceTimer = null }
          if recorderSubmit then
            val blob = js.Dynamic.newInstance(js.Dynamic.global.Blob)(
              chunks,
              js.Dynamic.literal("type" -> (if mimeType.nonEmpty then mimeType else "audio/webm"))
            )
            debug(s"utterance captured (${blob.size} bytes)")
            submitUtterance(blob)
          else debug("capture discarded")
      }
      r.onerror = { e =>
        debug(s"recorder error: ${js.JSON.stringify(e.asInstanceOf[js.Any])}")
        if recorder eq r then { recorder = null; recorderChunks = null }
      }
      recorder = r
      r.start()
      startPreviewRecorder()
      debug("recorder started")
      maxUtteranceTimer = setTimeout(MaxUtteranceMs) {
        maxUtteranceTimer = null
        // Forced utterance boundary: submit what we have and keep listening.
        if recorder != null then
          debug("utterance length cap reached; submitting")
          if vad != null then vad.reset()
          finishRecorder(submit = true)
      }

  private def finishRecorder(submit: Boolean): Unit =
    stopPreviewRecorder()
    if recorder != null then
      recorderSubmit = submit
      if recorder.state != "inactive" then recorder.stop()

  /** The parallel recorder behind interim previews. The main recorder is
    * never flushed mid-utterance (Firefox requestData produces chunk
    * boundaries strict decoders reject); instead this one is stopped at each
    * preview point — yielding a complete standalone file — and restarted for
    * the next segment. Interim text is the accumulated segment texts.
    */
  private def startPreviewRecorder(): Unit =
    if previewRecorder == null && micStream != null then
      try
        val chunks  = js.Array[js.Any]()
        val options = if mimeType.nonEmpty then js.Dynamic.literal("mimeType" -> mimeType) else js.Dynamic.literal()
        val r       = new MediaRecorder(micStream, options)
        previewDiscard = false
        val seq = utteranceSeq
        r.ondataavailable = { e =>
          if e.data.size.asInstanceOf[Double] > 0 then { val _ = chunks.push(e.data.asInstanceOf[js.Any]) }
        }
        r.onstop = { _ =>
          if previewRecorder eq r then previewRecorder = null
          if !previewDiscard && chunks.length > 0 then
            val blob = js.Dynamic.newInstance(js.Dynamic.global.Blob)(
              chunks,
              js.Dynamic.literal("type" -> (if mimeType.nonEmpty then mimeType else "audio/webm"))
            )
            if blob.size.asInstanceOf[Double] > 2000 then previewSegment(seq, blob)
            // Keep capturing the rest of the utterance as the next segment.
            if recorder != null then startPreviewRecorder()
        }
        r.onerror = { _ => if previewRecorder eq r then previewRecorder = null }
        r.start()
        previewSegmentStart = js.Date.now()
        previewRecorder = r
      catch
        case e: Throwable =>
          debug(s"preview recorder unavailable, disabling previews: ${e.getMessage}")
          previewBackoffUntil = js.Date.now() + 3_600_000

  private def stopPreviewRecorder(): Unit =
    if previewRecorder != null then
      previewDiscard = true
      if previewRecorder.state != "inactive" then previewRecorder.stop()
      previewRecorder = null

  private def discardRecorder(): Unit = finishRecorder(submit = false)

  /** Transcribe one complete preview segment and append it to the interim
    * feedback; never sends to the thread.
    */
  private def previewSegment(seq: Int, blob: js.Dynamic): Unit =
    val sess = sessionId
    if sess != null then
      previewInFlight = true
      lastPreviewAt = js.Date.now()
      val previewed =
        for
          b64    <- blobToBase64(blob)
          result <- rpcCall(
                      "previewUtterance",
                      js.Dynamic.literal("sessionId" -> sess, "mimeType" -> blob.`type`.asInstanceOf[String], "audioBase64" -> b64)
                    ).toFuture
        yield result
      previewed.onComplete { outcome =>
        previewInFlight = false
        outcome match
          case Success(result) if sessionId == sess && utteranceSeq == seq =>
            val segment = result.text.asInstanceOf[String]
            interimAccum = (interimAccum + " " + segment).trim
            interimText = interimAccum
            debug(s"interim: $interimText")
            refreshSnapshot()
          case Failure(e) =>
            // Likely provider rate limiting: stop previewing for a while so
            // final submissions keep working.
            previewBackoffUntil = js.Date.now() + 60000
            debug(s"preview failed (backing off): ${e.getMessage}")
          case _ => ()
      }

  private def blobToBase64(blob: js.Dynamic): Future[String] =
    val p      = Promise[String]()
    val reader = js.Dynamic.newInstance(js.Dynamic.global.FileReader)()
    reader.onload = js.Any.fromFunction1 { (_: js.Any) =>
      val dataUrl = reader.result.asInstanceOf[String]
      p.success(dataUrl.substring(dataUrl.indexOf(",") + 1))
    }
    reader.onerror = js.Any.fromFunction1((_: js.Any) => p.failure(new RuntimeException("could not read recording")))
    val _ = reader.readAsDataURL(blob)
    p.future

  private def submitUtterance(blob: js.Dynamic): Unit =
    val sess = sessionId
    val tid  = if utteranceTarget != null then utteranceTarget else threadId
    if sess != null then
      setPhase("transcribing")
      def send(b64: String): Future[js.Dynamic] =
        rpcCall(
          "submitUtterance",
          js.Dynamic.literal(
            "sessionId"   -> sess,
            "threadId"    -> tid,
            "mimeType"    -> blob.`type`.asInstanceOf[String],
            "audioBase64" -> b64
          )
        ).toFuture
      val submitted =
        for
          b64    <- blobToBase64(blob)
          result <- send(b64).recoverWith {
                      // A plugin reload wipes the backend's session registry;
                      // re-register this session and retry once.
                      case e if e.getMessage != null && e.getMessage.contains("no longer active") =>
                        debug("backend forgot the session; re-registering")
                        rpcCall("startSession", js.Dynamic.literal("sessionId" -> sess, "threadId" -> tid)).toFuture
                          .flatMap(_ => send(b64))
                    }
        yield result
      submitted.onComplete {
        case _ if sessionId != sess => ()
        case Success(result) =>
          if result.sent.asInstanceOf[Boolean] then
            heardText = result.text.asInstanceOf[String]
            interimText = null
            debug(s"heard: $heardText")
            errorNote = null
            setPhase("waiting")
            refreshSnapshot()
          else
            debug("transcription was empty; ignoring")
            if phase == "transcribing" then setPhase("listening")
        case Failure(e) =>
          debug(s"submit failed: ${e.getMessage}")
          errorNote = s"transcription failed: ${e.getMessage}"
          if phase == "transcribing" then setPhase("listening") else refreshSnapshot()
      }

  // ---------------------------------------------------------------- signals

  def handleSignal(payload: js.Any): Unit =
    val p    = payload.asInstanceOf[js.Dynamic]
    val sess = p.sessionId.asInstanceOf[js.UndefOr[String]].getOrElse(null)
    if sessionId != null && sess == sessionId then
      // Every mounted converse component forwards every signal; dedup by event id.
      val eventId = p.eventId.asInstanceOf[js.UndefOr[String]].getOrElse(null)
      if eventId == null || eventId != lastEventId then
        lastEventId = eventId
        p.`type`.asInstanceOf[js.UndefOr[String]].getOrElse("") match
          case "speak" =>
            val text = p.text.asInstanceOf[js.UndefOr[String]].getOrElse("")
            debug(s"speak signal (${text.length} chars)")
            if text.trim.nonEmpty then
              speakText(text, p.threadId.asInstanceOf[js.UndefOr[String]].getOrElse(null))
            else setPhase("listening")
          case "status" =>
            // Transient backend progress (e.g. managed service startup);
            // shown as interim text and naturally replaced by later updates.
            interimText = p.text.asInstanceOf[js.UndefOr[String]].getOrElse("")
            refreshSnapshot()
          case "agent-failed" =>
            errorNote = p.error.asInstanceOf[js.UndefOr[String]].getOrElse("the agent failed")
            setPhase("listening")
          case "revoked" =>
            // Another tab/session took over; tear down without racing it.
            debug("session revoked by another window")
            stopInternal(notifyServer = false)
            errorNote = "voice was taken over by another window"
            setPhase("idle")
          case _ => ()

  // --------------------------------------------------------------- playback

  private def speakText(text: String, sourceThread: String): Unit =
    stopSpeaking()
    val chunks = Speech.splitSpeechChunks(text)
    if chunks.isEmpty then setPhase("listening")
    else
      generation += 1
      val gen = generation
      speakingActive = true
      // The thread whose reply is playing — distinct from the routing target,
      // which follows the thread currently in view.
      speakingThread = sourceThread
      setPhase("speaking")
      refreshSnapshot()
      val done = if ttsMode == "server" then speakServer(gen, chunks) else speakBrowser(gen, chunks)
      done.onComplete {
        case Success(_) if generation == gen =>
          speakingActive = false
          speakingThread = null
          if phase == "speaking" then setPhase("listening") else refreshSnapshot()
        case Failure(e) if generation == gen =>
          debug(s"speech failed: ${e.getMessage}")
          speakingActive = false
          speakingThread = null
          errorNote = s"speech failed: ${e.getMessage}"
          setPhase("listening")
          refreshSnapshot()
        case _ => ()
      }

  private def stopSpeaking(): Unit =
    generation += 1
    speakingActive = false
    speakingThread = null
    if abortCtl != null then { val _ = abortCtl.abort(); abortCtl = null }
    if !js.isUndefined(js.Dynamic.global.speechSynthesis) then { val _ = js.Dynamic.global.speechSynthesis.cancel() }
    if currentAudio != null then
      // An AudioBufferSourceNode stops with stop(); its onended settles the
      // playback future.
      try { val _ = currentAudio.stop() }
      catch { case _: Throwable => () }
      currentAudio = null

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

  /** Plays through the session's AudioContext (created inside the toggle
    * click), which is exempt from autoplay blocking — a standalone
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

@JSExportTopLevel("createController")
def createController(deps: js.Dynamic): js.Dynamic =
  val ctl = new Controller(deps.rpcCall.asInstanceOf[js.Function2[String, js.Any, js.Promise[js.Dynamic]]])
  js.Dynamic.literal(
    "start"        -> js.Any.fromFunction2((threadId: String, opts: js.Dynamic) => ctl.start(threadId, opts)),
    "startViewed"  -> js.Any.fromFunction0(() => ctl.startViewed()),
    "stop"         -> js.Any.fromFunction0(() => ctl.stop()),
    "configure"    -> js.Any.fromFunction1((opts: js.Dynamic) => ctl.configure(opts)),
    "handleSignal" -> js.Any.fromFunction1((p: js.Any) => ctl.handleSignal(p)),
    "noteViewed"   -> js.Any.fromFunction1((tid: String) => ctl.noteViewed(tid)),
    "mountWidget"  -> js.Any.fromFunction0(() => Widget.mount(ctl)),
    "subscribe"    -> js.Any.fromFunction1((cb: js.Function0[Unit]) => ctl.subscribe(cb)),
    "getSnapshot"  -> js.Any.fromFunction0(() => ctl.getSnapshot())
  )
