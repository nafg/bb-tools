package converseapp

import conversecore.*

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.timers.{SetTimeoutHandle, clearTimeout, setTimeout}
import scala.scalajs.js.typedarray.Float32Array
import scala.util.{Failure, Success}

given ExecutionContext = ExecutionContext.parasitic

/** One per browser tab. Owns the microphone, VAD, recording, upload, and
  * playback for a single active voice session bound to one thread; the React
  * composer components are thin views over this. The session survives
  * navigation: any mounted converse component relays realtime signals to this
  * singleton, which filters by session id.
  */
final class Controller(
    rpcCall: js.Function2[String, js.Any, js.Promise[js.Dynamic]],
    createMicVad: js.Function1[js.Dynamic, js.Promise[js.Dynamic]],
    encodeWav: js.Function1[Float32Array, js.Any]
):
  private val PreRollMs      = 500.0
  private val MaxUtteranceMs = 60_000.0

  // Session
  private var sessionId: String   = null
  private var threadId: String    = null
  private var phase: String       = "idle"
  private var errorNote: String   = null
  private var heardText: String   = null
  private var lastEventId: String = null

  // Audio graph
  private var micStream: MediaStream        = null
  private var analysisStream: MediaStream   = null
  private var recordingStream: MediaStream  = null
  private var audioContext: AudioContext    = null
  private var speechDetector: SpeechDetector = null
  private var speechProbabilityThreshold: Double = 0.3
  private var endOfSpeechSilenceMs: Double = 3000.0
  private var mimeType: String              = ""
  private var frameCount: Int               = 0
  private var currentSpeechProbability: Double = 0.0

  // Recording
  private var captureActive: Boolean              = false
  private var maxUtteranceTimer: SetTimeoutHandle = null
  private var previewRecorder: MediaRecorder      = null
  private var previewDiscard: Boolean             = false
  private var previewSegmentStart: Double         = 0
  private var previewInFlight: Boolean            = false
  private var lastPreviewAt: Double               = 0
  private var previewBackoffUntil: Double         = 0
  private var utteranceSeq: Int                   = 0
  private var utteranceRoute: UtteranceRoute      = null
  private var interimText: String                 = null
  private var interimAccum: String                = ""

  // The thread the app route currently shows (reported by mounted converse
  // components); utterances are routed here.
  private var viewedThread: String = null
  private var threadDisplayNamesById = js.Dictionary.empty[String]
  // Set while the New thread screen is in view (reported by its homepage
  // section); utterances then start a new thread instead of going to a thread.
  private var composeSurface: Option[UtteranceRoute.ComposeSurface] = None
  private var projectDisplayNamesById = js.Dictionary.empty[String]
  // Host navigation, captured from a React slot; the plain-DOM widget has no
  // hook access. Null until the first thread surface mounts.
  private var navigateToThread: js.Function1[String, Unit] = null

  // Latest effective settings, pushed by the React bridge so the chrome-level
  // widget can start a session without access to hooks.
  private var startOptions: js.Dynamic = js.Dynamic.literal()

  // Playback is tab-wide rather than part of the voice-session state machine.
  private val player = new Player(rpcCall, () => refreshSnapshot())

  // React 18 external-store contract: a stable snapshot object per change.
  private var listeners: List[js.Function0[Unit]] = Nil
  private var snapshot: js.Dynamic                = null
  refreshSnapshot()

  private def debug(message: String): Unit =
    val _ = js.Dynamic.global.console.debug(s"[converse] $message")

  private def refreshSnapshot(): Unit =
    // While an utterance is being captured or transcribed, the banner belongs
    // on the thread it is latched to, not the thread currently in view.
    val latched = utteranceRoute != null && (captureActive || phase == "transcribing")
    val displayThread =
      if latched then utteranceRoute.existingThreadId.orNull
      else threadId
    val newThreadProject =
      if latched then
        utteranceRoute match
          case UtteranceRoute.NewThread(projectId) => projectId
          case _                                   => null
      else composeSurface.map(_.projectId.orNull).orNull
    snapshot = js.Dynamic.literal(
      "phase"                     -> phase,
      "threadId"                  -> displayThread,
      "viewed"                    -> viewedThread,
      "composeSurface"            -> composeSurface.isDefined,
      "newThreadProjectId"        -> newThreadProject,
      "newThreadProjectDisplayName" -> projectDisplayName(newThreadProject),
      "speakingThread"            -> player.speakingThread,
      "targetThreadDisplayName"   -> threadDisplayName(displayThread),
      "speakingThreadDisplayName" -> threadDisplayName(player.speakingThread),
      "speakingVoice"             -> player.speakingVoice,
      "error"                     -> errorNote,
      "heard"                     -> heardText,
      "interim"                   -> interimText,
      "speechProbability"         -> currentSpeechProbability,
      "speechProbabilityThreshold" -> speechProbabilityThreshold
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

  private def threadDisplayName(tid: String): String =
    if tid == null then null else threadDisplayNamesById.getOrElse(tid, tid)

  private def projectDisplayName(projectId: String): String =
    if projectId == null then null else projectDisplayNamesById.getOrElse(projectId, projectId)

  def setThreadDisplayNames(namesByThreadId: js.Dictionary[String]): Unit =
    threadDisplayNamesById = namesByThreadId
    refreshSnapshot()

  def setProjectDisplayNames(namesByProjectId: js.Dictionary[String]): Unit =
    projectDisplayNamesById = namesByProjectId
    refreshSnapshot()

  /** The New thread screen is in view; `projectId` is its selected project. */
  def noteComposeSurface(projectId: String | Null): Unit =
    composeSurface = Some(UtteranceRoute.ComposeSurface(Option(projectId).filter(_.nonEmpty)))
    refreshSnapshot()

  def leaveComposeSurface(): Unit =
    composeSurface = None
    refreshSnapshot()

  def setNavigateToThread(navigate: js.Function1[String, Unit]): Unit =
    navigateToThread = navigate

  def openThread(tid: String): Unit =
    if navigateToThread != null then navigateToThread(tid)

  def configure(opts: js.Dynamic): Unit =
    startOptions = opts
    player.setTtsMode(opts.ttsMode.asInstanceOf[js.UndefOr[String]].getOrElse("browser"))

  /** Start a session targeting the thread currently in view. */
  def startViewed(): Unit =
    if viewedThread == null && composeSurface.isEmpty then
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

  private def ensureAudioContext(): Unit =
    if audioContext == null then
      audioContext = new AudioContext
      player.setAudioContext(audioContext)
    val _ = audioContext.resume()

  def start(newThreadId: String, opts: js.Dynamic): Unit =
    stopInternal(notifyServer = true)
    // Created synchronously inside the click's user gesture: an AudioContext
    // created after an await can start (and stay) suspended under autoplay
    // policies, which reads as permanent silence.
    ensureAudioContext()
    val sess = js.Dynamic.global.crypto.randomUUID().asInstanceOf[String]
    sessionId = sess
    threadId = newThreadId
    errorNote = null
    heardText = null
    interimText = null
    utteranceRoute = null
    lastEventId = null
    val ttsMode = opts.ttsMode.asInstanceOf[js.UndefOr[String]].getOrElse("browser")
    player.setTtsMode(ttsMode)
    speechProbabilityThreshold = opts.speechProbabilityThreshold
      .asInstanceOf[js.UndefOr[Double]]
      .filter(t => t > 0.05 && t <= 1.0)
      .getOrElse(0.3)
    endOfSpeechSilenceMs = opts.endOfSpeechSilenceMs
      .asInstanceOf[js.UndefOr[Double]]
      .filter(ms => ms >= 32.0)
      .getOrElse(3000.0)
    setPhase("starting")
    debug(
      s"starting session $sess for thread $newThreadId " +
        s"(tts=$ttsMode speechProbabilityThreshold=$speechProbabilityThreshold " +
        s"endSilenceMs=$endOfSpeechSilenceMs)"
    )
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
      case Success(stream)        => beginListening(stream, sess)
      case Failure(e)             =>
        debug(s"start failed: ${e.getMessage}")
        errorNote = s"could not start voice: ${e.getMessage}"
        sessionId = null
        threadId = null
        closeAudioContext()
        setPhase("error")
    }

  private def beginListening(stream: MediaStream, sess: String): Unit =
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
      recordingStream = PreRollRecordingStream.build(
        preRollMilliseconds = PreRollMs,
        source = audioContext.createMediaStreamSource(micStream),
        createDelay = seconds => audioContext.createDelay(seconds),
        setDelaySeconds = (delay, seconds) => delay.delayTime.value = seconds,
        createDestination = () => audioContext.createMediaStreamDestination(),
        connectSourceToDelay = (source, delay) => { val _ = source.connect(delay) },
        connectDelayToDestination = (delay, destination) => { val _ = delay.connect(destination) },
        streamFromDestination = _.stream
      )
      mimeType = List("audio/webm;codecs=opus", "audio/webm", "audio/mp4", "audio/ogg;codecs=opus")
        .find(MediaRecorderStatic.isTypeSupported)
        .getOrElse("")
      SpeechDetector.create(
        stream = analysisStream,
        audioContext = audioContext,
        positiveSpeechThreshold = speechProbabilityThreshold,
        endOfSpeechSilenceMs = endOfSpeechSilenceMs,
        createMicVad = createMicVad,
        encodeWav = encodeWav,
        onFrame = handleSpeechFrame,
        onEvent = handleSpeechDetectorEvent
      ).onComplete {
        case Success(detector) if sessionId == sess =>
          speechDetector = detector
          debug(s"listening (preview recorder mime: ${if mimeType.isEmpty then "(browser default)" else mimeType})")
          retarget()
          setPhase("listening")
        case Success(detector) =>
          detector.destroy().failed.foreach(_ => ())
        case Failure(e) if sessionId == sess =>
          debug(s"speech detector failed to start: ${e.getMessage}")
          errorNote = s"could not start speech detection: ${e.getMessage}"
          stopInternal(notifyServer = true)
          setPhase("error")
        case Failure(_) => ()
      }

  def stop(): Unit =
    debug("stopped by user")
    stopInternal(notifyServer = true)
    errorNote = null
    setPhase("idle")

  private def closeAudioContext(): Unit =
    if audioContext != null then
      val ctx = audioContext
      audioContext = null
      player.setAudioContext(null)
      ctx.close().toFuture.failed.foreach(_ => ())

  private def stopInternal(notifyServer: Boolean): Unit =
    if maxUtteranceTimer != null then { clearTimeout(maxUtteranceTimer); maxUtteranceTimer = null }
    abandonCapture()
    player.stop()
    if speechDetector != null then
      val detector = speechDetector
      speechDetector = null
      detector.destroy().failed.foreach(_ => ())
    if recordingStream != null then { recordingStream.getTracks().foreach(_.stop()); recordingStream = null }
    if micStream != null then { micStream.getTracks().foreach(_.stop()); micStream = null }
    if analysisStream != null then { analysisStream.getTracks().foreach(_.stop()); analysisStream = null }
    closeAudioContext()
    heardText = null
    interimText = null
    utteranceRoute = null
    currentSpeechProbability = 0.0
    if sessionId != null then
      val sess = sessionId
      sessionId = null
      threadId = null
      if notifyServer then
        rpcCall("stopSession", js.Dynamic.literal("sessionId" -> sess)).toFuture.failed.foreach(_ => ())

  // ------------------------------------------------------- speech detection

  private def handleSpeechFrame(frame: SpeechDetectorFrame): Unit =
    currentSpeechProbability = frame.speechProbability
    frameCount += 1
    // Silero V5 frames are 32ms; update the UI around 6Hz rather than render
    // for every inference result.
    if frameCount % 5 == 0 then refreshSnapshot()
    // Continuous speech never reaches a pause-based preview point.
    if captureActive && previewRecorder != null && !previewInFlight
      && js.Date.now() >= previewBackoffUntil
      && js.Date.now() - previewSegmentStart >= 3500
    then rotatePreviewRecorder()

  private def handleSpeechDetectorEvent(event: SpeechDetectorEvent): Unit =
    debug(s"speech detector: $event")
    event match
      case SpeechDetectorEvent.CandidateStart => beginCapture()
      case SpeechDetectorEvent.CandidateAbandoned(evidence) =>
        reportDiscardedCapture(evidence)
        abandonCapture()
        if phase == "recording" then setPhase("listening")
      case SpeechDetectorEvent.SpeechStart =>
        beginCapture()
        if player.isSpeaking then
          debug("barge-in: stopping playback")
          player.stop()
        setPhase("recording")
      case SpeechDetectorEvent.PreviewPoint =>
        // Previews are best-effort extra transcription load; throttle them so
        // they can never rate-limit the provider out from under final submits.
        val now = js.Date.now()
        if previewRecorder != null && !previewInFlight && now >= previewBackoffUntil
          && now - lastPreviewAt >= 3000
        then rotatePreviewRecorder()
      case SpeechDetectorEvent.SpeechEnd(recording, evidence) =>
        if captureActive then
          captureActive = false
          if maxUtteranceTimer != null then { clearTimeout(maxUtteranceTimer); maxUtteranceTimer = null }
          stopPreviewRecorder()
          debug(s"utterance captured (${recording.size.asInstanceOf[Double]} bytes)")
          submitUtterance(recording, evidenceToJs(evidence))
        else if phase == "recording" then setPhase("listening")

  // -------------------------------------------------------------- recording

  private def beginCapture(): Unit =
    if !captureActive && recordingStream != null then
      UtteranceRoute.latch(composeSurface, Option(currentTarget), Option(threadId)) match
        case None =>
          errorNote = "pick a project to start a new thread by voice"
          refreshSnapshot()
        case Some(route) =>
          utteranceRoute = route
          captureActive = true
          utteranceSeq += 1
          interimText = null
          interimAccum = ""
          startPreviewRecorder()
          debug("capture started")
          maxUtteranceTimer = setTimeout(MaxUtteranceMs + PreRollMs) {
            maxUtteranceTimer = null
            if captureActive && speechDetector != null then
              debug("utterance length cap reached; flushing")
              speechDetector.flush().failed.foreach { e =>
                errorNote = s"could not split long utterance: ${e.getMessage}"
                refreshSnapshot()
              }
          }

  private def abandonCapture(): Unit =
    captureActive = false
    if maxUtteranceTimer != null then { clearTimeout(maxUtteranceTimer); maxUtteranceTimer = null }
    stopPreviewRecorder()
    debug("capture discarded")

  private def evidenceToJs(evidence: SpeechDetectorEvidence): js.Dynamic =
    js.Dynamic.literal(
      "classifier"                 -> "silero-v5",
      "frameCount"                 -> evidence.frameCount,
      "speechFrameCount"           -> evidence.speechFrameCount,
      "peakRms"                    -> evidence.peakRms,
      "peakSpeechProbability"      -> evidence.peakSpeechProbability,
      "positiveSpeechThreshold"    -> evidence.positiveSpeechThreshold
    )

  private def reportDiscardedCapture(evidence: SpeechDetectorEvidence): Unit =
    val sess = sessionId
    if sess != null then
      rpcCall(
        "reportDiscardedCapture",
        js.Dynamic.literal(
          "sessionId" -> sess,
          "threadId"  -> currentTarget,
          "evidence"  -> evidenceToJs(evidence)
        )
      ).toFuture.failed.foreach(_ => ())

  /** A separate recorder exists only for interim previews. Silero owns final
    * utterance audio; this recorder is stopped at each preview point to yield
    * a complete standalone file and then restarted for the next segment.
    */
  private def startPreviewRecorder(): Unit =
    if previewRecorder == null && recordingStream != null then
      try
        val chunks  = js.Array[js.Any]()
        val options = if mimeType.nonEmpty then js.Dynamic.literal("mimeType" -> mimeType) else js.Dynamic.literal()
        val r       = new MediaRecorder(recordingStream, options)
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
            if captureActive then startPreviewRecorder()
        }
        r.onerror = { _ => if previewRecorder eq r then previewRecorder = null }
        r.start()
        previewSegmentStart = js.Date.now()
        previewRecorder = r
      catch
        case e: Throwable =>
          debug(s"preview recorder unavailable, disabling previews: ${e.getMessage}")
          previewBackoffUntil = js.Date.now() + 3_600_000

  /** Start the next preview recorder before stopping the current one. Each
    * stopped recorder still yields a complete standalone file, while the brief
    * overlap prevents speech from falling between segments.
    */
  private def rotatePreviewRecorder(): Unit =
    if previewRecorder != null then
      val previous = previewRecorder
      previewRecorder = null
      PreviewSegmentHandoff.replaceBeforeStopping(
        startReplacement = () => startPreviewRecorder(),
        stopPrevious = () => if previous.state != "inactive" then previous.stop()
      )

  private def stopPreviewRecorder(): Unit =
    if previewRecorder != null then
      previewDiscard = true
      if previewRecorder.state != "inactive" then previewRecorder.stop()
      previewRecorder = null

  /** Transcribe one complete preview segment and append it to the interim
    * feedback; never sends to the thread.
    */
  private def previewSegment(seq: Int, blob: js.Dynamic): Unit =
    val sess = sessionId
    val tid  = if utteranceRoute != null then utteranceRoute.existingThreadId.orNull else threadId
    if sess != null then
      previewInFlight = true
      lastPreviewAt = js.Date.now()
      val previewed =
        for
          b64    <- blobToBase64(blob)
          result <- rpcCall(
                    "previewUtterance",
                    js.Dynamic.literal(
                      "sessionId"           -> sess,
                      "threadId"            -> tid,
                      "mimeType"            -> blob.`type`.asInstanceOf[String],
                      "audioBase64"         -> b64,
                      "precedingTranscript" -> TranscriptionContext.promptFrom(interimAccum)
                    )
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

  private def submitUtterance(blob: js.Dynamic, evidence: js.Dynamic): Unit =
    val sess  = sessionId
    val route = if utteranceRoute != null then utteranceRoute else UtteranceRoute.ToThread(threadId)
    val tid   = route.existingThreadId.orNull
    if sess != null then
      setPhase("transcribing")
      val submitted =
        for
          b64 <- blobToBase64(blob)
          result <- {
            def send(): Future[js.Dynamic] =
              val request = js.Dynamic.literal(
                "sessionId"   -> sess,
                "mimeType"    -> blob.`type`.asInstanceOf[String],
                "audioBase64" -> b64,
                "evidence"    -> evidence
              )
              route match
                case UtteranceRoute.ToThread(id)         => request.updateDynamic("threadId")(id)
                case UtteranceRoute.NewThread(projectId) => request.updateDynamic("projectId")(projectId)
              rpcCall("submitUtterance", request).toFuture
            send().recoverWith {
              // A plugin reload wipes the backend's session registry;
              // re-register this session and retry once.
              case e if e.getMessage != null && e.getMessage.contains("no longer active") =>
                debug("backend forgot the session; re-registering")
                rpcCall("startSession", js.Dynamic.literal("sessionId" -> sess, "threadId" -> tid)).toFuture
                  .flatMap(_ => send())
            }
          }
        yield result
      submitted.onComplete {
        case _ if sessionId != sess => ()
        case Success(result) if result == null =>
          interimText = null
          if phase == "transcribing" then setPhase("listening")
        case Success(result) =>
          if result.sent.asInstanceOf[Boolean] then
            heardText = result.text.asInstanceOf[String]
            interimText = null
            debug(s"heard: $heardText")
            errorNote = null
            // A new thread was started for this utterance: it becomes the
            // session's thread and the app opens it.
            result.threadId.asInstanceOf[js.UndefOr[String]].foreach { created =>
              debug(s"new thread $created started by voice")
              threadId = created
              viewedThread = created
              openThread(created)
            }
            setPhase("waiting")
            refreshSnapshot()
          else
            debug("transcription was empty; ignoring")
            if phase == "transcribing" then setPhase("listening")
        case Failure(e) =>
          debug(s"submit failed: ${e.getMessage}")
          // Covers the whole submit pipeline (reading the recording,
          // transcription, and the thread send), so don't blame transcription.
          errorNote = s"voice message failed: ${e.getMessage}"
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

  /** Speak message text without starting or advancing a voice session. */
  def replay(text: String, sourceThread: String): Future[Unit] =
    if Speech.splitSpeechChunks(text).isEmpty then Future.unit
    else
      // Replay replaces session-driven speech, so release that session from
      // its speaking phase before the Player supersedes the old playback.
      if phase == "speaking" then setPhase("listening")
      ensureAudioContext()
      player.play(text, sourceThread).map(_ => ())

  private def speakText(text: String, sourceThread: String): Unit =
    val chunks = Speech.splitSpeechChunks(text)
    if chunks.isEmpty then setPhase("listening")
    else
      setPhase("speaking")
      player.enqueue(text, sourceThread).onComplete {
        case Success(completed) if completed && !player.isSpeaking =>
          if phase == "speaking" then setPhase("listening")
        case Failure(e) =>
          debug(s"speech failed: ${e.getMessage}")
          errorNote = s"speech failed: ${e.getMessage}"
          if player.isSpeaking then refreshSnapshot() else setPhase("listening")
        case _ => ()
      }

@JSExportTopLevel("createController")
def createController(deps: js.Dynamic): js.Dynamic =
  val ctl = new Controller(
    deps.rpcCall.asInstanceOf[js.Function2[String, js.Any, js.Promise[js.Dynamic]]],
    deps.createMicVad.asInstanceOf[js.Function1[js.Dynamic, js.Promise[js.Dynamic]]],
    deps.encodeWav.asInstanceOf[js.Function1[Float32Array, js.Any]]
  )
  js.Dynamic.literal(
    "start"       -> js.Any.fromFunction2((threadId: String, opts: js.Dynamic) => ctl.start(threadId, opts)),
    "startViewed" -> js.Any.fromFunction0(() => ctl.startViewed()),
    "stop"        -> js.Any.fromFunction0(() => ctl.stop()),
    "configure"   -> js.Any.fromFunction1((opts: js.Dynamic) => ctl.configure(opts)),
    "replay"      -> js.Any.fromFunction2((text: String, threadId: String) => ctl.replay(text, threadId).toJSPromise),
    "handleSignal" -> js.Any.fromFunction1((p: js.Any) => ctl.handleSignal(p)),
    "noteViewed" -> js.Any.fromFunction1((tid: String) => ctl.noteViewed(tid)),
    "setThreadDisplayNames" -> js.Any.fromFunction1(
      (namesByThreadId: js.Dictionary[String]) => ctl.setThreadDisplayNames(namesByThreadId)
    ),
    "setProjectDisplayNames" -> js.Any.fromFunction1(
      (namesByProjectId: js.Dictionary[String]) => ctl.setProjectDisplayNames(namesByProjectId)
    ),
    "noteComposeSurface"  -> js.Any.fromFunction1((projectId: String | Null) => ctl.noteComposeSurface(projectId)),
    "leaveComposeSurface" -> js.Any.fromFunction0(() => ctl.leaveComposeSurface()),
    "setNavigateToThread" -> js.Any.fromFunction1(
      (navigate: js.Function1[String, Unit]) => ctl.setNavigateToThread(navigate)
    ),
    "mountWidget"  -> js.Any.fromFunction0(() => Widget.mount(ctl)),
    "subscribe"    -> js.Any.fromFunction1((cb: js.Function0[Unit]) => ctl.subscribe(cb)),
    "getSnapshot"  -> js.Any.fromFunction0(() => ctl.getSnapshot())
  )
