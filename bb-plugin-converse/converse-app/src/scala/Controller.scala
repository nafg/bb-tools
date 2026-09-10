package converseapp

import conversecore.*
import conversecore.VoiceState.{Off, On}

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
private final case class PendingUpload(
    sessionId: String,
    route: UtteranceRoute,
    blob: js.Dynamic,
    evidence: js.Dynamic
)

final class Controller(
    rpcCall: js.Function2[String, js.Any, js.Promise[js.Dynamic]],
    createMicVad: js.Function1[js.Dynamic, js.Promise[js.Dynamic]],
    encodeWav: js.Function1[Float32Array, js.Any]
):
  private val PreRollMs         = 500.0
  // Speech transcribes fine at this Opus rate; the browser default is about four times it.
  private val UploadBitsPerSecond = 32_000
  private val MaxUtteranceMs    = 60_000.0
  private val AudioStartGraceMs = 1000.0

  // Session
  private var sessionId: String     = null
  private var threadId: String      = null
  private var state: VoiceState     = VoiceState.initial
  // A transient message for the user that is not a session ending: a failed
  // utterance, a routing problem, an agent error.
  private var notice: String        = null
  private var heardText: String     = null
  private var lastEventId: String   = null
  // What the backend last said the agent is doing on the thread; forgotten
  // when the session ends.
  private var turnActivity: Option[AgentActivity] = None
  private var awaitingSpeech: Boolean             = false

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
  // Records the utterance itself as compressed audio while Silero captures
  // it as samples; the compressed file is what gets uploaded when it exists.
  private var utteranceRecorder: MediaRecorder    = null
  private var utteranceChunks: js.Array[js.Any]   = js.Array()
  private var utteranceRecording: Promise[Option[js.Dynamic]] = null
  // Uploads go up one at a time in capture order; a failed upload is retried
  // before the recording is given up on.
  private val pendingUploads = scala.collection.mutable.Queue.empty[PendingUpload]
  private var uploading: Boolean = false
  private var previewDiscard: Boolean             = false
  private var previewSegmentStart: Double         = 0
  private var previewInFlight: Boolean            = false
  private var lastPreviewAt: Double               = 0
  private var previewBackoffUntil: Double         = 0
  private var previewRateLimitPauseMs: Option[Double] = None
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
  private val cues   = new SoundCuePlayer
  private var replyHeld: Boolean = false

  // React 18 external-store contract: a stable snapshot object per change.
  private var listeners: List[js.Function0[Unit]] = Nil
  private var snapshot: js.Dynamic                = null
  refreshSnapshot()

  private def debug(message: String): Unit =
    val _ = js.Dynamic.global.console.debug(s"[converse] $message")

  private def refreshSnapshot(): Unit =
    // While an utterance is being captured or transcribed, the banner belongs
    // on the thread it is latched to, not the thread currently in view.
    val latched = utteranceRoute != null && (captureActive || state == On(SessionPhase.Transcribing))
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
      "on"                        -> state.isOn,
      "phase"                     -> state.runningPhase.map(_.name).orNull,
      "endedBecause"              -> lastEndingMessage.orNull,
      "threadId"                  -> displayThread,
      "viewed"                    -> viewedThread,
      "composeSurface"            -> composeSurface.isDefined,
      "newThreadProjectId"        -> newThreadProject,
      "newThreadProjectDisplayName" -> projectDisplayName(newThreadProject),
      "speakingThread"            -> player.speakingThread,
      "targetThreadDisplayName"   -> threadDisplayName(displayThread),
      "speakingThreadDisplayName" -> threadDisplayName(player.speakingThread),
      "speakingVoice"             -> player.speakingVoice,
      "notice"                    -> notice,
      "activity"                  -> activity.map(_.signalName).orNull,
      "heard"                     -> heardText,
      "interim"                   -> interimText,
      "speechProbability"         -> currentSpeechProbability,
      "speechProbabilityThreshold" -> speechProbabilityThreshold
    )
    listeners.foreach(_())

  def voiceState: VoiceState                     = state
  def currentNotice: Option[String]              = Option(notice)
  def agentActivity: Option[AgentActivity]       = activity
  private def lastEndingMessage: Option[String]  = state match
    case Off(ending) => ending.flatMap(_.message)
    case On(_)       => None

  private def setState(next: VoiceState): Unit =
    if state != next then
      debug(s"state: $state -> $next")
      val previous = state
      state = next
      cues.onStateChange(previous, next, activity)
      refreshSnapshot()
      syncReplyHold()

  private def enter(phase: SessionPhase): Unit = setState(On(phase))

  private def turnOff(end: VoiceSessionRestore.SessionEnd, detail: String = null): Unit =
    turnActivity = None
    awaitingSpeech = false
    setState(Off(Some(VoiceState.Ending(end, Option(detail)))))

  /** Leaves `from` for Listening, if that is where the session still is. */
  private def backToListening(from: SessionPhase): Unit =
    if state == On(from) then enter(SessionPhase.Listening)

  private def activity: Option[AgentActivity] =
    if awaitingSpeech then Some(AgentActivity.AwaitingSpeech) else turnActivity

  private def activityChanged(): Unit =
    debug(s"activity: ${activity.fold("none")(_.signalName)}")
    cues.onActivity(state, activity)
    refreshSnapshot()

  private def syncReplyHold(): Unit =
    val hold = ReplySpeechHold.holds(captureActive, state)
    if hold != player.isHeld then
      player.setHeld(hold)
      if !hold && replyHeld then
        replyHeld = false
        if player.isSpeaking then enter(SessionPhase.Speaking)

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
    opts.soundCues.asInstanceOf[js.UndefOr[js.Dynamic]].foreach { soundCues =>
      SoundCueSettings.fromUnchecked(SoundCueSettingsJson.read(soundCues)) match
        case Right(settings) => cues.configure(settings)
        case Left(message)   => debug(s"sound cue settings rejected: $message")
    }

  /** Plays one cue's configured sound on demand, ignoring whether it is enabled. */
  def previewCue(settingKey: String): Unit =
    SoundCue.values.find(_.settingKey == settingKey).foreach(cues.preview)

  /** Plays an unsaved cue configuration exactly as a session would; a
    * repeating cue keeps sounding until `stopAudition`. Returns the reason
    * the configuration was rejected, or null.
    */
  def auditionCue(settingKey: String, config: js.Dynamic): String =
    SoundCue.values.find(_.settingKey == settingKey).fold(s"unknown cue $settingKey") { cue =>
      SoundCueSettings.checkCue(cue, SoundCueSettingsJson.readCue(config)) match
        case Right(setting) => cues.audition(setting); null
        case Left(message)  => message
    }

  def stopAudition(): Unit = cues.stopAudition()

  /** Start a session targeting the thread currently in view. */
  def startViewed(): Unit =
    if viewedThread == null && composeSurface.isEmpty then
      notice = "open a thread first"
      refreshSnapshot()
    else startSession(storedRestoreIntent.map(_.sessionId).getOrElse(freshSessionId()), viewedThread, startOptions)

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
    cues.ensureReady()

  private val RestoreIntentKey = "converse.voiceSessionRestore"

  private def restoreStorage: js.Dynamic = js.Dynamic.global.window.sessionStorage

  private def rememberForRestore(intent: VoiceSessionRestore.Intent): Unit =
    val _ = restoreStorage.setItem(RestoreIntentKey, VoiceSessionRestore.encode(intent))

  private def forgetRestore(): Unit =
    val _ = restoreStorage.removeItem(RestoreIntentKey)

  private def storedRestoreIntent: Option[VoiceSessionRestore.Intent] =
    Option(restoreStorage.getItem(RestoreIntentKey).asInstanceOf[String]).flatMap(VoiceSessionRestore.decode)

  private def freshSessionId(): String = js.Dynamic.global.crypto.randomUUID().asInstanceOf[String]

  def start(newThreadId: String, opts: js.Dynamic): Unit =
    startSession(freshSessionId(), newThreadId, opts)

  /** Bring back the session a previous frontend generation was unloaded under. */
  def resumeInterruptedSession(): Unit =
    storedRestoreIntent.foreach { intent =>
      debug(s"resuming session ${intent.sessionId} after the frontend was reloaded")
      startSession(intent.sessionId, intent.threadId.orNull, js.JSON.parse(intent.startOptionsJson))
    }

  private def startSession(sess: String, newThreadId: String, opts: js.Dynamic): Unit =
    stopInternal(VoiceSessionRestore.SessionEnd.Superseded)
    // Created synchronously inside the click's user gesture: an AudioContext
    // created after an await can start (and stay) suspended under autoplay
    // policies, which reads as permanent silence.
    ensureAudioContext()
    sessionId = sess
    threadId = newThreadId
    notice = null
    heardText = null
    interimText = null
    utteranceRoute = null
    lastEventId = null
    rememberForRestore(VoiceSessionRestore.Intent(sess, Option(newThreadId), js.JSON.stringify(opts)))
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
    enter(SessionPhase.Starting)
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
        endSession(VoiceSessionRestore.SessionEnd.Failed)
        closeAudioContext()
        turnOff(VoiceSessionRestore.SessionEnd.Failed, s"could not start voice: ${e.getMessage}")
    }

  private def beginListening(stream: MediaStream, sess: String): Unit =
    if audioContext == null then stream.getTracks().foreach(_.stop()) // stopped while starting
    else
      micStream = stream
      analysisStream = stream.cloneStream()
      audioContext.resume().toFuture.onComplete { result =>
        val state = if audioContext == null then "closed" else audioContext.state
        debug(s"audio context state after resume: $state ($result)")
      }
      // Checked on a timer: a browser that wants a user gesture first leaves
      // resume() pending rather than rejecting it.
      val _ = setTimeout(AudioStartGraceMs) {
        if sessionId == sess && audioContext != null && audioContext.state == "suspended" then
          pauseUntilUserGesture()
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
          enter(SessionPhase.Listening)
        case Success(detector) =>
          detector.destroy().failed.foreach(_ => ())
        case Failure(e) if sessionId == sess =>
          debug(s"speech detector failed to start: ${e.getMessage}")
          stopInternal(VoiceSessionRestore.SessionEnd.Failed)
          turnOff(VoiceSessionRestore.SessionEnd.Failed, s"could not start speech detection: ${e.getMessage}")
        case Failure(_) => ()
      }

  def stop(): Unit =
    debug("stopped by user")
    stopInternal(VoiceSessionRestore.SessionEnd.StoppedByUser)
    notice = null
    turnOff(VoiceSessionRestore.SessionEnd.StoppedByUser)

  private def pauseUntilUserGesture(): Unit =
    debug("audio context still suspended; pausing until the user clicks")
    stopInternal(VoiceSessionRestore.SessionEnd.PausedUntilUserGesture)
    turnOff(VoiceSessionRestore.SessionEnd.PausedUntilUserGesture)

  /** End the session because the plugin's frontend is unloading; the next
    * frontend generation resumes it.
    */
  def teardown(): Unit =
    debug("frontend unloading; the session resumes on the next mount")
    stopInternal(VoiceSessionRestore.SessionEnd.FrontendUnloaded)
    notice = null
    turnOff(VoiceSessionRestore.SessionEnd.FrontendUnloaded)

  private def closeAudioContext(): Unit =
    if audioContext != null then
      val ctx = audioContext
      audioContext = null
      player.setAudioContext(null)
      ctx.close().toFuture.failed.foreach(_ => ())

  private def stopInternal(end: VoiceSessionRestore.SessionEnd): Unit =
    if maxUtteranceTimer != null then { clearTimeout(maxUtteranceTimer); maxUtteranceTimer = null }
    player.stop()
    replyHeld = false
    abandonCapture()
    pendingUploads.clear()
    uploading = false
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
    endSession(end)

  private def endSession(end: VoiceSessionRestore.SessionEnd): Unit =
    if sessionId != null then
      val sess = sessionId
      sessionId = null
      threadId = null
      if end.notifiesServer then
        rpcCall("stopSession", js.Dynamic.literal("sessionId" -> sess)).toFuture.failed.foreach(_ => ())
      if !end.keepsIntent then forgetRestore()

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
        backToListening(SessionPhase.Recording)
      case SpeechDetectorEvent.SpeechStart =>
        beginCapture()
        if player.isSpeaking then
          debug("barge-in: stopping playback")
          player.stop()
        enter(SessionPhase.Recording)
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
          finishUtteranceRecorder().foreach { compressed =>
            enqueueUpload(compressed.getOrElse(recording), evidenceToJs(evidence))
          }
          syncReplyHold()
        else backToListening(SessionPhase.Recording)

  // -------------------------------------------------------------- recording

  private def beginCapture(): Unit =
    if !captureActive && recordingStream != null then
      UtteranceRoute.latch(composeSurface, Option(currentTarget), Option(threadId)) match
        case None =>
          notice = "pick a project to start a new thread by voice"
          refreshSnapshot()
        case Some(route) =>
          utteranceRoute = route
          captureActive = true
          syncReplyHold()
          noteCapture(active = true)
          utteranceSeq += 1
          interimText = null
          interimAccum = ""
          startPreviewRecorder()
          startUtteranceRecorder()
          debug("capture started")
          maxUtteranceTimer = setTimeout(MaxUtteranceMs + PreRollMs) {
            maxUtteranceTimer = null
            if captureActive && speechDetector != null then
              debug("utterance length cap reached; flushing")
              speechDetector.flush().failed.foreach { e =>
                notice = s"could not split long utterance: ${e.getMessage}"
                refreshSnapshot()
              }
          }

  private def abandonCapture(): Unit =
    if captureActive then noteCapture(active = false)
    captureActive = false
    if maxUtteranceTimer != null then { clearTimeout(maxUtteranceTimer); maxUtteranceTimer = null }
    stopPreviewRecorder()
    discardUtteranceRecorder()
    debug("capture discarded")
    syncReplyHold()

  /** Tell the backend an utterance is being captured for the latched route, so
    * an agent waiting for more speech keeps waiting. A capture that ends in a
    * submission stays active server-side until that submission completes.
    */
  private def noteCapture(active: Boolean): Unit =
    val sess = sessionId
    if sess != null then
      rpcCall(
        "captureState",
        js.Dynamic.literal(
          "sessionId" -> sess,
          "threadId"  -> (if utteranceRoute != null then utteranceRoute.existingThreadId.orNull else null),
          "active"    -> active
        )
      ).toFuture.failed.foreach(_ => ())

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
        val options =
          if mimeType.nonEmpty then js.Dynamic.literal("mimeType" -> mimeType, "audioBitsPerSecond" -> UploadBitsPerSecond)
          else js.Dynamic.literal("audioBitsPerSecond" -> UploadBitsPerSecond)
        val r = new MediaRecorder(recordingStream, options)
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
          result <- withSessionReregistered(sess, tid) { () =>
                      rpcCall(
                        "previewUtterance",
                        js.Dynamic.literal(
                          "sessionId"           -> sess,
                          "threadId"            -> tid,
                          "mimeType"            -> blob.`type`.asInstanceOf[String],
                          "audioBase64"         -> b64,
                          "precedingTranscript" -> TranscriptionContext.promptFrom(interimAccum)
                        )
                      ).toFuture
                    }
        yield result
      previewed.onComplete { outcome =>
        previewInFlight = false
        outcome match
          case Success(result) if sessionId == sess && utteranceSeq == seq =>
            previewRateLimitPauseMs = None
            val segment = result.text.asInstanceOf[String]
            interimAccum = (interimAccum + " " + segment).trim
            interimText = interimAccum
            debug(s"interim: $interimText")
            refreshSnapshot()
          case Failure(e) =>
            PreviewBackoff.pauseAfter(transcriptionFailure(e), previewRateLimitPauseMs) match
              case Some(pauseMs) =>
                previewRateLimitPauseMs = Some(pauseMs)
                previewBackoffUntil = js.Date.now() + pauseMs
                debug(s"preview rate limited (pausing previews ${pauseMs / 1000}s): ${e.getMessage}")
              case None =>
                debug(s"preview failed: ${e.getMessage}")
          case _ => ()
      }

  private def transcriptionFailure(e: Throwable): TranscriptionFailure =
    TranscriptionFailure.classify(Option(e.getMessage).getOrElse(""))

  /** A plugin reload wipes the backend's session registry; when a call is
    * rejected for that reason, re-register the session and retry it once.
    */
  private def withSessionReregistered(sess: String, tid: String)(call: () => Future[js.Dynamic]): Future[js.Dynamic] =
    call().recoverWith {
      case e if transcriptionFailure(e) == TranscriptionFailure.SessionForgotten =>
        debug("backend forgot the session; re-registering")
        rpcCall("startSession", js.Dynamic.literal("sessionId" -> sess, "threadId" -> tid)).toFuture
          .flatMap(_ => call())
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

  /** Records the utterance as compressed audio from capture start; uploads
    * are a tenth the size of the sample recording, which matters on a slow
    * link. Silero's samples remain the fallback when recording is unavailable.
    */
  private def startUtteranceRecorder(): Unit =
    if utteranceRecorder == null && recordingStream != null && mimeType.nonEmpty then
      try
        val chunks   = js.Array[js.Any]()
        val finished = Promise[Option[js.Dynamic]]()
        val r        = new MediaRecorder(recordingStream, js.Dynamic.literal("mimeType" -> mimeType, "audioBitsPerSecond" -> UploadBitsPerSecond))
        r.ondataavailable = { e =>
          if e.data.size.asInstanceOf[Double] > 0 then { val _ = chunks.push(e.data.asInstanceOf[js.Any]) }
        }
        r.onstop = { _ =>
          val blob =
            if chunks.length == 0 then None
            else Some(js.Dynamic.newInstance(js.Dynamic.global.Blob)(chunks, js.Dynamic.literal("type" -> mimeType)))
          val _ = finished.trySuccess(blob)
        }
        r.onerror = { _ => val _ = finished.trySuccess(None) }
        r.start()
        utteranceRecorder = r
        utteranceChunks = chunks
        utteranceRecording = finished
      catch
        case e: Throwable => debug(s"utterance recorder unavailable, uploading samples instead: ${e.getMessage}")

  /** Stops the recorder and yields its file, or None when there is nothing usable. */
  private def finishUtteranceRecorder(): Future[Option[js.Dynamic]] =
    val r        = utteranceRecorder
    val finished = utteranceRecording
    utteranceRecorder = null
    utteranceRecording = null
    if r == null then Future.successful(None)
    else
      if r.state != "inactive" then r.stop() else { val _ = finished.trySuccess(None) }
      finished.future

  private def discardUtteranceRecorder(): Unit =
    val r = utteranceRecorder
    utteranceRecorder = null
    utteranceRecording = null
    if r != null && r.state != "inactive" then r.stop()

  private def enqueueUpload(blob: js.Dynamic, evidence: js.Dynamic): Unit =
    if sessionId != null then
      val route = if utteranceRoute != null then utteranceRoute else UtteranceRoute.ToThread(threadId)
      pendingUploads.enqueue(PendingUpload(sessionId, route, blob, evidence))
      uploadNext()

  private def uploadNext(): Unit =
    if !uploading && pendingUploads.nonEmpty then
      uploading = true
      val upload = pendingUploads.dequeue()
      enter(SessionPhase.Transcribing)
      attemptUpload(upload, attempt = 1)

  private def attemptUpload(upload: PendingUpload, attempt: Int): Unit =
    val sess = upload.sessionId
    val tid  = upload.route.existingThreadId.orNull
    val submitted =
      for
        b64 <- blobToBase64(upload.blob)
        result <- withSessionReregistered(sess, tid) { () =>
                    val request = js.Dynamic.literal(
                      "sessionId"   -> sess,
                      "mimeType"    -> upload.blob.`type`.asInstanceOf[String],
                      "audioBase64" -> b64,
                      "evidence"    -> upload.evidence
                    )
                    upload.route match
                      case UtteranceRoute.ToThread(id)         => request.updateDynamic("threadId")(id)
                      case UtteranceRoute.NewThread(projectId) => request.updateDynamic("projectId")(projectId)
                    rpcCall("submitUtterance", request).toFuture
                  }
      yield result
    submitted.onComplete {
      case _ if sessionId != sess => ()
      case Success(result) =>
        uploaded(result)
        uploading = false
        if pendingUploads.nonEmpty then uploadNext()
      case Failure(e) =>
        UtteranceUploadRetry.delayBeforeRetry(attempt, transcriptionFailure(e)) match
          case Some(delayMs) =>
            debug(s"upload failed (attempt $attempt, retrying in ${delayMs}ms): ${e.getMessage}")
            notice = s"voice message upload failed, retrying…"
            refreshSnapshot()
            val _ = setTimeout(delayMs.toDouble) { if sessionId == sess then attemptUpload(upload, attempt + 1) }
          case None =>
            debug(s"upload given up after $attempt attempts: ${e.getMessage}")
            // Covers the whole submit pipeline (reading the recording,
            // transcription, and the thread send), so don't blame transcription.
            notice = s"voice message failed: ${e.getMessage}"
            uploading = false
            if pendingUploads.nonEmpty then uploadNext()
            else if state == On(SessionPhase.Transcribing) then enter(SessionPhase.Listening)
            else refreshSnapshot()
    }

  private def uploaded(result: js.Dynamic): Unit =
    val moreQueued = pendingUploads.nonEmpty
    if result == null then
      interimText = null
      if !moreQueued then backToListening(SessionPhase.Transcribing)
    else if result.sent.asInstanceOf[Boolean] then
      heardText = result.text.asInstanceOf[String]
      interimText = null
      debug(s"heard: $heardText")
      notice = null
      // A new thread was started for this utterance: it becomes the
      // session's thread and the app opens it.
      result.threadId.asInstanceOf[js.UndefOr[String]].foreach { created =>
        debug(s"new thread $created started by voice")
        threadId = created
        viewedThread = created
        openThread(created)
      }
      enter(SessionPhase.Waiting)
      refreshSnapshot()
    else
      debug("transcription was empty; ignoring")
      if !moreQueued then backToListening(SessionPhase.Transcribing)

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
            else enter(SessionPhase.Listening)
          case "status" =>
            // Transient backend progress (e.g. managed service startup);
            // shown as interim text and naturally replaced by later updates.
            interimText = p.text.asInstanceOf[js.UndefOr[String]].getOrElse("")
            refreshSnapshot()
          case "agent-failed" =>
            notice = p.error.asInstanceOf[js.UndefOr[String]].getOrElse("the agent failed")
            enter(SessionPhase.Listening)
          case "activity" =>
            turnActivity = AgentActivity.fromSignalName(p.activity.asInstanceOf[js.UndefOr[String]].getOrElse(""))
            activityChanged()
          case "awaiting-speech-begin" =>
            awaitingSpeech = true
            activityChanged()
            val _ = cues.play(SoundCue.AwaitingSpeech)
          case "awaiting-speech-end" =>
            awaitingSpeech = false
            activityChanged()
          case "revoked" =>
            // Another tab/session took over; tear down without racing it.
            debug("session revoked by another window")
            stopInternal(VoiceSessionRestore.SessionEnd.TakenOverByAnotherWindow)
            turnOff(VoiceSessionRestore.SessionEnd.TakenOverByAnotherWindow)
          case _ => ()

  // --------------------------------------------------------------- playback

  /** Speak message text without starting or advancing a voice session. */
  def replay(text: String, sourceThread: String): Future[Unit] =
    if Speech.splitSpeechChunks(text).isEmpty then Future.unit
    else
      // Replay replaces session-driven speech, so release that session from
      // its speaking phase before the Player supersedes the old playback.
      backToListening(SessionPhase.Speaking)
      replyHeld = false
      ensureAudioContext()
      player.play(text, sourceThread).map(_ => ())

  private def speakText(text: String, sourceThread: String): Unit =
    val chunks = Speech.splitSpeechChunks(text)
    if chunks.isEmpty then enter(SessionPhase.Listening)
    else
      if player.isHeld then
        debug("reply held until the current utterance is submitted")
        replyHeld = true
      else enter(SessionPhase.Speaking)
      player.enqueue(text, sourceThread).onComplete {
        case Success(completed) if completed && !player.isSpeaking =>
          backToListening(SessionPhase.Speaking)
        case Failure(e) =>
          debug(s"speech failed: ${e.getMessage}")
          notice = s"speech failed: ${e.getMessage}"
          if player.isSpeaking then refreshSnapshot() else enter(SessionPhase.Listening)
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
    "teardown"    -> js.Any.fromFunction0(() => ctl.teardown()),
    "previewCue"  -> js.Any.fromFunction1((settingKey: String) => ctl.previewCue(settingKey)),
    "auditionCue" -> js.Any.fromFunction2((settingKey: String, config: js.Dynamic) => ctl.auditionCue(settingKey, config)),
    "stopAudition" -> js.Any.fromFunction0(() => ctl.stopAudition()),
    "soundCues" -> SoundCue.values.map { cue =>
      js.Dynamic.literal("key" -> cue.settingKey, "label" -> cue.label, "repeating" -> cue.defaultRepeat.isDefined)
    }.toJSArray,
    "resumeInterruptedSession" -> js.Any.fromFunction0(() => ctl.resumeInterruptedSession()),
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
