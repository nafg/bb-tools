package converse

import bbplugin.*
import conversecore.{
  UploadLimit,
  RecordingStatus,
  ActivityKind,
  ActivitySource,
  PlaybackReport,
  RetryBackoff,
  TranscriptSegment,
  TranscriptSegments,
  AssistantMessage,
  AssistantReply,
  IdleOutcome,
  ContinuationWaiters,
  ReplyInterruption,
  SoundCue,
  SoundCueSettings,
  SoundCueSettingsJson,
  Speech,
  ThreadVoiceAssignments,
  TranscriptionFailure,
  TurnActivityTracker,
  TurnEvent
}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.chaining.*

given ExecutionContext = ExecutionContext.parasitic

private def str(value: js.Dynamic): String =
  value.asInstanceOf[js.UndefOr[Any]].fold("")(v => if v == null then "" else v.toString)

/** Longest accepted base64 audio field (~5 MiB decoded): far above a minute of
  * browser-compressed speech, far below transcribeVoice's 25 MiB cap.
  */

private val PocketEnglishVoices = Vector(
  "alba",
  "anna",
  "azelma",
  "bill_boerst",
  "caro_davy",
  "charles",
  "cosette",
  "eponine",
  "eve",
  "fantine",
  "george",
  "jane",
  "javert",
  "jean",
  "marius",
  "mary",
  "michael",
  "paul",
  "peter_yearsley",
  "stuart_bell",
  "vera"
)

private val ThreadVoicesEnabledKey = "voices.byThreadEnabled"
private val EnabledPocketVoicesKey = "voices.pocket.enabled"
private val SoundCuesKey           = "soundCues"

/** A local service the plugin starts in a bb terminal when it is first needed
  * and stops once it has gone unused for `idleMinutes`. `key` names its stored
  * terminal and last use; `files` (paths relative to the home directory) are
  * written before it starts; it is up once `probeUrl` answers at all. With a
  * `versionCheck`, a running instance reporting another version is stopped
  * and started afresh, so a changed service takes effect without waiting for
  * it to go idle.
  */
private final case class ManagedService(
    key: String,
    label: String,
    probeUrl: String,
    startCommand: String => String,
    readyWithinSeconds: Int,
    idleMinutes: Int,
    files: Map[String, String] = Map.empty,
    versionCheck: Option[ServiceVersionCheck] = None
)

/** `url` answers GET with JSON `{"version": ...}`, expected to be `expected`. */
private final case class ServiceVersionCheck(url: String, expected: String)

/** How long a managed service may leave its version check unanswered before
  * it is taken to be hung rather than busy.
  */
private val ServiceHungAfterMs = 60_000.0

/** Activity spans are kept 90 days, independently of the recordings, so a
  * reader that was down for a while can still catch up; rows are tiny.
  */
private val ActivitySpansKeptDays = 90

private val PocketService = ManagedService(
  key = "pocket",
  label = "Pocket TTS",
  probeUrl = "http://127.0.0.1:8000/tts",
  startCommand = _ => "uvx pocket-tts serve --host localhost --port 8000",
  readyWithinSeconds = 90,
  idleMinutes = 15
)

/** Parakeet on `device`. The device is part of the version, so an instance
  * running on the other one is replaced when next needed.
  */
private def parakeetService(device: ParakeetDevice) = ManagedService(
  key = "parakeet",
  label = "Parakeet",
  probeUrl = s"http://127.0.0.1:${ParakeetServer.Port}/v1/audio/transcriptions",
  startCommand = ParakeetServer.startCommand(device),
  // The first start downloads the CUDA libraries and the model, about 3 GB.
  readyWithinSeconds = 900,
  // Starting again loads the model, which delays the next transcription.
  idleMinutes = 60,
  files = Map(ParakeetServer.ScriptRelativePath -> ParakeetServer.Script),
  versionCheck = Some(
    ServiceVersionCheck(s"http://127.0.0.1:${ParakeetServer.Port}/version", ParakeetServer.version(device))
  )
)

/** The speech synthesis provider that runs on the GPU, beside which Parakeet
  * has to run on the CPU: the two do not fit on the card with the desktop.
  */
private val QwenTtsProvider = "qwen3-tts"

private val QwenService = ManagedService(
  key = "qwen",
  label = "Qwen3-TTS",
  probeUrl = s"http://127.0.0.1:${QwenServer.Port}/tts",
  startCommand = QwenServer.startCommand,
  // The first start downloads the model, about 3.4 GB, and quantizes it.
  readyWithinSeconds = 1800,
  // While it runs it holds its GPU memory, so it does not linger long.
  idleMinutes = 15,
  files = Map(QwenServer.ScriptRelativePath -> QwenServer.Script),
  versionCheck = Some(ServiceVersionCheck(s"http://127.0.0.1:${QwenServer.Port}/version", QwenServer.Version))
)

/** The speech synthesis provider that runs on the CPU, for when the GPU is
  * better left alone.
  */
private val ChatterboxNanoProvider = "chatterbox-nano"

private val ChatterboxService = ManagedService(
  key = "chatterbox",
  label = "Chatterbox Nano",
  probeUrl = s"http://127.0.0.1:${ChatterboxServer.Port}/tts",
  startCommand = ChatterboxServer.startCommand,
  // The first start installs PyTorch and downloads the model, about 1.5 GB.
  readyWithinSeconds = 900,
  idleMinutes = 15,
  files = Map(ChatterboxServer.ScriptRelativePath -> ChatterboxServer.Script),
  versionCheck =
    Some(ServiceVersionCheck(s"http://127.0.0.1:${ChatterboxServer.Port}/version", ChatterboxServer.Version))
)

/** Every managed service, in a form that identifies it, for stopping idle
  * ones.
  */
private val ManagedServices =
  List(PocketService, QwenService, ChatterboxService, parakeetService(ParakeetDevice.GpuIfFree))

private def ttsConfigKey(provider: String): String = s"tts.config.$provider"

private final case class ActiveSession(sessionId: String, threadId: String)
private final case class AwaitingReply(sessionId: String, previousAssistantMessageId: Option[String])

/** The threads whose next reply is to be read aloud, by thread, for the
  * session that will speak it. Kept in the database as well, so a backend
  * reload does not forget a reply it was waiting for.
  */
private final class AwaitingReplies(db: SqliteDb):
  private val byThread = mutable.Map.empty[String, AwaitingReply]
  db.prepare("SELECT thread_id, session_id, previous_assistant_message_id FROM awaiting_replies").all().foreach { row =>
    byThread.update(
      str(row.thread_id),
      AwaitingReply(str(row.session_id), Option(str(row.previous_assistant_message_id)).filter(_.nonEmpty))
    )
  }

  def get(threadId: String): Option[AwaitingReply] = byThread.get(threadId)
  def contains(threadId: String): Boolean           = byThread.contains(threadId)
  def threads: Iterable[String]                     = byThread.keys
  def bySession(sessionId: String): List[String]    = byThread.collect { case (t, r) if r.sessionId == sessionId => t }.toList

  def put(threadId: String, reply: AwaitingReply): Unit =
    byThread.update(threadId, reply)
    val _ = db
      .prepare(
        "INSERT OR REPLACE INTO awaiting_replies (thread_id, session_id, previous_assistant_message_id) VALUES (?, ?, ?)"
      )
      .run(threadId, reply.sessionId, reply.previousAssistantMessageId.orNull)

  def remove(threadId: String): Option[AwaitingReply] =
    val removed = byThread.remove(threadId)
    if removed.isDefined then { val _ = db.prepare("DELETE FROM awaiting_replies WHERE thread_id = ?").run(threadId) }
    removed

  /** Keeps only the replies awaited for sessions `keep` accepts. */
  def retainSessions(keep: String => Boolean): Unit =
    byThread.collect { case (t, r) if !keep(r.sessionId) => t }.toList.foreach(remove)

/** How long after a turn ends without answering the user's latest message a
  * turn answering it may take to begin (one began 4 s later in practice).
  */
private val FollowUpTurnWithinMs = 20_000.0

/** What the browser's speech detector reported for an upload. */
private final case class DetectorEvidence(frames: Int, speechFrames: Int, peakSpeechProbability: Double, peakRms: Double)

/** A recording as it arrives, before it is transcribed. Exactly one of
  * `threadId` and `projectId` is non-empty: the thread it goes to, or the
  * project to start a thread in. `notes` are what the target thread is to be
  * told with it, decided when it was spoken.
  */
private final case class ReceivedRecording(
    id: String,
    at: Double,
    sessionId: String,
    threadId: String,
    projectId: String,
    mimeType: String,
    audioBase64: String,
    detector: Option[DetectorEvidence],
    notes: List[String],
    speechStartAt: Option[Double],
    speechEndAt: Option[Double]
)

/** The transcriber called the request itself malformed (400, 413, 415 or
  * 422): bad audio or a bad parameter. Sending the same request again gets the
  * same answer. Other client errors (a bad key, a wrong URL or model) are
  * configuration, which can be fixed while the recording waits.
  */
private class TranscriberRejectedRequest(message: String) extends RuntimeException(message)

/** The transcriber refused the audio as too long or too large (413). */
private final class TranscriberRejectedLength(message: String) extends TranscriberRejectedRequest(message)

/** A transcript, which service produced it, and, when the service reports
  * them, its per-segment timing, silence and confidence figures as one
  * summary line.
  */
private final case class Transcription(text: String, transcriber: String, segments: Option[String])

/** Voice-conversation state for the whole server: at most one active browser
  * session, plus the threads whose next idle transition should be spoken.
  */
private final class State:
  var active: Option[ActiveSession]     = None
  // The thread an utterance is currently being captured or submitted for;
  // "" while it will start a new thread.
  var capturingFor: Option[String] = None
  val continuationWaiters = ContinuationWaiters()
  // What a thread should be told with its next utterance: replies of its that
  // the user cut off while speaking to another thread.
  val pendingNotes: mutable.Map[String, List[String]] = mutable.Map.empty
  // Stored recordings are transcribed and delivered one at a time, in the
  // order they were stored, so messages reach threads in the order spoken.
  var recordingQueue: Future[Unit] = Future.unit
  // Managed-service checks in progress, by key, so concurrent callers share
  // one check and a replacement starts once.
  val servicesStarting: mutable.Map[String, Future[Either[String, String]]] = mutable.Map.empty
  // What each browser session last reported about itself, and when.
  val browserStates: mutable.Map[String, (js.Dynamic, Double)] = mutable.Map.empty
  // When each managed service's version checks began timing out, if they are.
  val serviceBusySince: mutable.Map[String, Double] = mutable.Map.empty
  private var eventCounter              = 0
  def nextEventId(): String =
    eventCounter += 1
    eventCounter.toString

private object Schema:
  /** Minimal Standard Schema v1 value: bb's RPC layer only needs `~standard.validate`. */
  private def make(validate: js.Function1[js.Any, js.Any]): js.Any =
    js.Dynamic.literal(
      "~standard" -> js.Dynamic.literal("version" -> 1, "vendor" -> "bb-plugin-converse", "validate" -> validate)
    )

  private def invalid(message: String): js.Any =
    js.Dynamic.literal("issues" -> js.Array(js.Dynamic.literal("message" -> message)))

  val passthrough: js.Any =
    make(js.Any.fromFunction1(value => js.Dynamic.literal("value" -> value.asInstanceOf[js.Any])))

  /** An object with exactly these non-empty string fields (extra fields pass through). */
  def strings(fields: (String, Int)*): js.Any = stringsWithOptional(fields, Nil)

  /** Like `strings`; `optional` fields may also be absent, null, or empty. */
  def stringsWithOptional(required: Seq[(String, Int)], optional: Seq[(String, Int)]): js.Any =
    make(js.Any.fromFunction1 { value =>
      if value == null || js.typeOf(value) != "object" then invalid("expected an object")
      else
        val d = value.asInstanceOf[js.Dynamic]
        def tooLong(name: String, maxLength: Int): Boolean =
          val v = d.selectDynamic(name)
          js.typeOf(v) == "string" && v.asInstanceOf[String].length > maxLength
        def missing(name: String): Boolean =
          val v = d.selectDynamic(name)
          js.typeOf(v) != "string" || v.asInstanceOf[String].isEmpty
        def wrongType(name: String): Boolean =
          val v = d.selectDynamic(name)
          !js.isUndefined(v) && v != null && js.typeOf(v) != "string"
        required
          .collectFirst {
            case (name, maxLength) if missing(name) || tooLong(name, maxLength) =>
              invalid(s"$name must be a non-empty string of at most $maxLength characters")
          }
          .orElse(optional.collectFirst {
            case (name, maxLength) if wrongType(name) || tooLong(name, maxLength) =>
              invalid(s"$name must be a string of at most $maxLength characters")
          })
          .getOrElse(js.Dynamic.literal("value" -> value.asInstanceOf[js.Any]))
    })

private def rejected(message: String): js.Promise[js.Dynamic] =
  js.Promise.reject(new js.Error(message)).asInstanceOf[js.Promise[js.Dynamic]]

private def jsonResponse(status: Int, message: String): js.Dynamic =
  js.Dynamic.newInstance(js.Dynamic.global.Response)(
    js.JSON.stringify(js.Dynamic.literal("error" -> message)),
    js.Dynamic.literal("status" -> status, "headers" -> js.Dynamic.literal("content-type" -> "application/json"))
  )

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit =
  val state = new State
  val db    = bb.storage.database()
  bb.storage.migrate(
    db,
    js.Array(
      """CREATE TABLE IF NOT EXISTS thread_voice_assignments (
        |  thread_id TEXT PRIMARY KEY,
        |  voice TEXT NOT NULL
        |)""".stripMargin,
      """CREATE TABLE IF NOT EXISTS thread_provider_voice_assignments (
        |  provider TEXT NOT NULL,
        |  thread_id TEXT NOT NULL,
        |  voice TEXT NOT NULL,
        |  PRIMARY KEY (provider, thread_id)
        |)""".stripMargin,
      """INSERT OR IGNORE INTO thread_provider_voice_assignments (provider, thread_id, voice)
        |SELECT 'pocket-tts', thread_id, voice FROM thread_voice_assignments""".stripMargin,
      """CREATE TABLE IF NOT EXISTS utterances (
        |  id TEXT PRIMARY KEY,
        |  at REAL NOT NULL,
        |  thread_id TEXT NOT NULL,
        |  mime_type TEXT NOT NULL,
        |  audio BLOB NOT NULL,
        |  text TEXT NOT NULL,
        |  segments TEXT
        |)""".stripMargin,
      "ALTER TABLE utterances ADD COLUMN detector_frames INTEGER",
      "ALTER TABLE utterances ADD COLUMN detector_speech_frames INTEGER",
      "ALTER TABLE utterances ADD COLUMN detector_peak_speech_probability REAL",
      "ALTER TABLE utterances ADD COLUMN detector_peak_rms REAL",
      "ALTER TABLE utterances ADD COLUMN transcriber TEXT",
      "ALTER TABLE utterances ADD COLUMN status TEXT",
      "ALTER TABLE utterances ADD COLUMN last_error TEXT",
      "ALTER TABLE utterances ADD COLUMN session_id TEXT",
      "ALTER TABLE utterances ADD COLUMN project_id TEXT",
      "ALTER TABLE utterances ADD COLUMN notes TEXT",
      """CREATE TABLE IF NOT EXISTS awaiting_replies (
        |  thread_id TEXT PRIMARY KEY,
        |  session_id TEXT NOT NULL,
        |  previous_assistant_message_id TEXT
        |)""".stripMargin,
      "ALTER TABLE utterances ADD COLUMN speech_start_at REAL",
      "ALTER TABLE utterances ADD COLUMN speech_end_at REAL",
      """CREATE TABLE IF NOT EXISTS activity_spans (
        |  seq INTEGER PRIMARY KEY AUTOINCREMENT,
        |  id TEXT NOT NULL UNIQUE,
        |  kind TEXT NOT NULL,
        |  source TEXT NOT NULL,
        |  thread_id TEXT NOT NULL,
        |  start_at REAL NOT NULL,
        |  end_at REAL NOT NULL,
        |  session_id TEXT
        |)""".stripMargin,
      // Recordings stored before speech was stamped in the browser: their span
      // is taken from the store time and the detector's frame count (32 ms
      // each), so it runs about 4 s long and late by the upload's delay.
      """INSERT OR IGNORE INTO activity_spans (id, kind, source, thread_id, start_at, end_at, session_id)
        |SELECT id, 'speech', 'backfill', thread_id, at - detector_frames * 32, at, session_id
        |FROM utterances
        |WHERE thread_id != '' AND detector_frames IS NOT NULL AND speech_start_at IS NULL
        |ORDER BY at""".stripMargin,
      "ALTER TABLE utterances ADD COLUMN say_again INTEGER"
    )
  )
  val awaitingReplies = AwaitingReplies(db)

  // Recordings are stored before anything else is done with them, and kept
  // with their transcripts so a wrong ending can be checked against the audio
  // that was actually sent, days later and across reloads. Ones not yet
  // delivered are kept until they are.
  val UtterancesKeptDays = 7
  val UtterancesListed   = 50
  val UtteranceColumns =
    "id, at, thread_id, project_id, mime_type, length(audio) AS bytes, text, segments, detector_frames, " +
      "detector_speech_frames, detector_peak_speech_probability, detector_peak_rms, transcriber, status, last_error"
  def recordingStored(id: String): Boolean =
    db.prepare("SELECT 1 FROM utterances WHERE id = ?").get(id).isDefined
  def storeRecording(r: ReceivedRecording): Unit =
    val _ = db
      .prepare(
        "INSERT INTO utterances (id, at, session_id, thread_id, project_id, mime_type, audio, text, detector_frames, " +
          "detector_speech_frames, detector_peak_speech_probability, detector_peak_rms, notes, status, " +
          "speech_start_at, speech_end_at) VALUES (?, ?, ?, ?, ?, ?, ?, '', ?, ?, ?, ?, ?, ?, ?, ?)"
      )
      .run(
        r.id,
        r.at,
        r.sessionId,
        r.threadId,
        r.projectId,
        r.mimeType,
        js.Dynamic.global.Buffer.from(r.audioBase64, "base64"),
        r.detector.map(_.frames: js.Any).orNull,
        r.detector.map(_.speechFrames: js.Any).orNull,
        r.detector.map(_.peakSpeechProbability: js.Any).orNull,
        r.detector.map(_.peakRms: js.Any).orNull,
        js.JSON.stringify(r.notes.toJSArray),
        RecordingStatus.Received.code,
        r.speechStartAt.map(t => t: js.Any).orNull,
        r.speechEndAt.map(t => t: js.Any).orNull
      )
    val _ = db
      .prepare(
        "DELETE FROM utterances WHERE at < ? AND (status IS NULL OR status IN (?, ?))"
      )
      .run(js.Date.now() - UtterancesKeptDays * 86_400_000.0, RecordingStatus.Sent.code, RecordingStatus.Empty.code)
  def setRecordingStatus(id: String, status: RecordingStatus): Unit =
    status match
      case RecordingStatus.Failed(reason) =>
        val _ = db.prepare("UPDATE utterances SET status = ?, last_error = ? WHERE id = ?").run(status.code, reason, id)
      case _ =>
        val _ = db.prepare("UPDATE utterances SET status = ? WHERE id = ?").run(status.code, id)
    bb.realtime.publish("converse-utterances", js.Dynamic.literal())
  def noteRecordingError(id: String, message: String): Unit =
    val _ = db.prepare("UPDATE utterances SET last_error = ? WHERE id = ?").run(message, id)
    bb.realtime.publish("converse-utterances", js.Dynamic.literal())
  def saveTranscription(id: String, transcription: Transcription): Unit =
    val _ = db
      .prepare("UPDATE utterances SET text = ?, transcriber = ?, segments = ? WHERE id = ?")
      .run(transcription.text, transcription.transcriber, transcription.segments.orNull, id)
  def setRecordingThread(id: String, threadId: String): Unit =
    val _ = db.prepare("UPDATE utterances SET thread_id = ? WHERE id = ?").run(threadId, id)
    recordSpeechSpan(id)

  // What the user did and heard, as time spans per thread, for other plugins
  // to read through the activitySpans RPC.
  def recordActivitySpan(
      id: String,
      kind: ActivityKind,
      source: ActivitySource,
      threadId: String,
      startAt: Double,
      endAt: Double,
      sessionId: Option[String]
  ): Unit =
    val _ = db
      .prepare(
        "INSERT OR IGNORE INTO activity_spans (id, kind, source, thread_id, start_at, end_at, session_id) " +
          "VALUES (?, ?, ?, ?, ?, ?, ?)"
      )
      .run(id, kind.code, source.code, threadId, startAt, endAt, sessionId.orNull)
    val _ = db.prepare("DELETE FROM activity_spans WHERE end_at < ?").run(js.Date.now() - ActivitySpansKeptDays * 86_400_000.0)
  /** The span of a stored recording's speech, once its thread is known; one
    * that will start a thread gets it when the thread is started.
    */
  def recordSpeechSpan(recordingId: String): Unit =
    db.prepare("SELECT thread_id, session_id, speech_start_at, speech_end_at FROM utterances WHERE id = ?")
      .get(recordingId)
      .toOption
      .filter(row => str(row.thread_id).nonEmpty && row.speech_start_at != null && row.speech_end_at != null)
      .foreach { row =>
        recordActivitySpan(
          recordingId,
          ActivityKind.Speech,
          ActivitySource.Session,
          str(row.thread_id),
          row.speech_start_at.asInstanceOf[Double],
          row.speech_end_at.asInstanceOf[Double],
          Option(str(row.session_id)).filter(_.nonEmpty)
        )
      }
  def recordingStatus(row: js.Dynamic): Option[RecordingStatus] =
    RecordingStatus.fromColumns(row.status.asInstanceOf[String | Null], row.last_error.asInstanceOf[String | Null])
  /** Whether a recording for this thread is stored but not yet transcribed. */
  def recordingAwaitingTranscriptionFor(threadId: String): Boolean =
    db.prepare("SELECT 1 FROM utterances WHERE thread_id = ? AND status IN (?, ?) AND transcriber IS NULL LIMIT 1")
      .get(threadId, RecordingStatus.Received.code, RecordingStatus.Sending.code)
      .isDefined
  /** Whether a recording for this thread is stored but not yet delivered. */
  def recordingInProgressFor(threadId: String): Boolean =
    db.prepare("SELECT 1 FROM utterances WHERE thread_id = ? AND status IN (?, ?) LIMIT 1")
      .get(threadId, RecordingStatus.Received.code, RecordingStatus.Sending.code)
      .isDefined
  def listUtterances(): js.Array[js.Dynamic] =
    db.prepare(s"SELECT $UtteranceColumns FROM utterances ORDER BY at DESC LIMIT ?").all(UtterancesListed).map(utteranceRow)
  def utteranceRow(row: js.Dynamic): js.Dynamic =
    js.Dynamic.literal(
      "id"       -> row.id,
      "at"       -> row.at,
      "threadId" -> row.thread_id,
      "mimeType" -> row.mime_type,
      "bytes"    -> row.bytes,
      "projectId" -> row.project_id,
      "text"        -> row.text,
      "transcriber" -> row.transcriber,
      "status"      -> row.status,
      "lastError"   -> row.last_error,
      "segments"    -> row.segments,
      "detector" -> (
        if row.detector_frames == null then null
        else
          js.Dynamic.literal(
            "frames"                -> row.detector_frames,
            "speechFrames"          -> row.detector_speech_frames,
            "peakSpeechProbability" -> row.detector_peak_speech_probability,
            "peakRms"               -> row.detector_peak_rms
          )
      )
    )
  def utteranceById(id: String): Option[js.Dynamic] =
    db.prepare(s"SELECT $UtteranceColumns FROM utterances WHERE id = ?").get(id).toOption.map(utteranceRow)
  /** The latest upload whose transcript became this message's text. */
  def utteranceForMessage(threadId: String, text: String): Option[js.Dynamic] =
    db.prepare(s"SELECT $UtteranceColumns FROM utterances WHERE thread_id = ? AND text = ? ORDER BY at DESC LIMIT 1")
      .get(threadId, text.trim)
      .toOption
      .map(utteranceRow)
  def utteranceAudio(id: String): Option[(String, js.Dynamic)] =
    db.prepare("SELECT mime_type, audio FROM utterances WHERE id = ?").get(id).toOption.map(row => (str(row.mime_type), row.audio))

  val settings = bb.settings.define(
    js.Dynamic.literal(
      "ttsProvider" -> js.Dynamic.literal(
        "type"    -> "select",
        "label"   -> "TEXT-TO-SPEECH PROVIDER",
        "options" -> js.Array("browser", "pocket-tts", QwenTtsProvider, ChatterboxNanoProvider, "kokoro", "openai-compatible"),
        "default" -> "browser"
      ),
      "sttProvider" -> js.Dynamic.literal(
        "type"    -> "select",
        "label"   -> "SPEECH-TO-TEXT PROVIDER",
        "options" -> js.Array("bb", "groq", "openai-compatible", "parakeet"),
        "default" -> "bb"
      ),
      "groqApiKey" -> js.Dynamic.literal(
        "type"   -> "string",
        "label"  -> "GROQ: API key",
        "secret" -> true
      ),
      "groqModel" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "GROQ: transcription model",
        "default" -> "whisper-large-v3-turbo"
      ),
      "sttCustomUrl" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "CUSTOM STT: OpenAI-compatible transcription endpoint (also covers whisper.cpp, Speaches)",
        "default" -> ""
      ),
      "sttCustomModel" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "CUSTOM STT: model",
        "default" -> ""
      ),
      "sttCustomApiKey" -> js.Dynamic.literal(
        "type"   -> "string",
        "label"  -> "CUSTOM STT: API key (Bearer; empty for local services)",
        "secret" -> true
      ),
      "sttLanguage" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "STT: language hint (ISO code like en; empty = auto-detect)",
        "default" -> "en"
      ),
      "speechProbabilityThreshold" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "MIC: minimum speech probability (0–1; raise if noise triggers it)",
        "default" -> "0.3"
      ),
      "endOfSpeechSilenceMs" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "MIC: silence before submitting an utterance (milliseconds)",
        "default" -> "3000"
      ),
      "continuationWaitMs" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "AGENT: maximum time to wait for continued speech (milliseconds)",
        "default" -> "8000"
      )
    )
  )


  /** The Parakeet to use under `currentSettings`. */
  def parakeetFor(currentSettings: js.Dynamic): ManagedService =
    parakeetService(
      if str(currentSettings.ttsProvider) == QwenTtsProvider then ParakeetDevice.Cpu else ParakeetDevice.GpuIfFree
    )

  def stringArray(value: js.Dynamic): Vector[String] =
    value.asInstanceOf[js.UndefOr[js.Array[String]]].fold(Vector.empty)(_.toVector)
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinct

  def defaultTtsConfig(provider: String): js.Dynamic = provider match
    case "pocket-tts" => js.Dynamic.literal("defaultVoice" -> "alba")
    case QwenTtsProvider =>
      js.Dynamic.literal("defaultVoice" -> QwenServer.DefaultVoice, "voices" -> js.Array(QwenServer.DefaultVoice))
    case ChatterboxNanoProvider =>
      js.Dynamic.literal(
        "defaultVoice" -> ChatterboxServer.BuiltInVoice,
        "voices"       -> js.Array(ChatterboxServer.BuiltInVoice)
      )
    case "kokoro" =>
      js.Dynamic.literal(
        "url"          -> "http://127.0.0.1:8880/v1/audio/speech",
        "defaultVoice" -> "af_heart",
        "voices"       -> js.Array("af_heart")
      )
    case "openai-compatible" =>
      js.Dynamic.literal(
        "url" -> "", "model" -> "", "defaultVoice" -> "", "apiKey" -> "", "voices" -> js.Array()
      )
    case _ => js.Dynamic.literal("defaultVoice" -> "", "voices" -> js.Array())

  def ttsConfig(provider: String): Future[js.Dynamic] =
    bb.storage.kv.get(ttsConfigKey(provider)).toFuture.map(
      _.asInstanceOf[js.UndefOr[js.Dynamic]].getOrElse(defaultTtsConfig(provider))
    )

  def availableVoices(provider: String, config: js.Dynamic): Vector[String] =
    if provider == "pocket-tts" then PocketEnglishVoices else stringArray(config.voices)

  def enabledVoicesKey(provider: String): String =
    if provider == "pocket-tts" then EnabledPocketVoicesKey else s"voices.$provider.enabled"

  def enabledVoices(provider: String, catalog: Vector[String]): Future[Vector[String]] =
    bb.storage.kv.get(enabledVoicesKey(provider)).toFuture.map { stored =>
      stored.asInstanceOf[js.UndefOr[js.Array[String]]]
        .map(_.toVector.filter(catalog.contains).distinct)
        .getOrElse(catalog)
    }

  def threadVoicesEnabled(): Future[Boolean] =
    bb.storage.kv.get(ThreadVoicesEnabledKey).toFuture.map(
      _.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false)
    )

  def storedThreadVoice(provider: String, threadId: String): Option[String] =
    db.prepare("SELECT voice FROM thread_provider_voice_assignments WHERE provider = ? AND thread_id = ?")
      .get(provider, threadId)
      .toOption
      .map(row => str(row.voice))
      .filter(_.nonEmpty)

  def voiceSettingsResponse(provider: String): Future[js.Dynamic] =
    for
      config         <- ttsConfig(provider)
      enabled        <- threadVoicesEnabled()
      catalog         = availableVoices(provider, config)
      enabledCatalog <- enabledVoices(provider, catalog)
    yield js.Dynamic.literal(
      "enabled"          -> enabled,
      "provider"         -> provider,
      "url"              -> str(config.url),
      "model"            -> str(config.model),
      "defaultVoice"     -> str(config.defaultVoice),
      "apiKeyConfigured" -> str(config.apiKey).nonEmpty,
      "voices" -> catalog.map { voice =>
        js.Dynamic.literal("id" -> voice, "enabled" -> enabledCatalog.contains(voice))
      }.toJSArray
    )

  def assignThreadVoice(provider: String, threadId: String, enabledCatalog: Vector[String]): Option[String] =
    storedThreadVoice(provider, threadId).filter(enabledCatalog.contains).orElse {
      val counts = db.prepare(
        "SELECT voice, COUNT(*) AS count FROM thread_provider_voice_assignments WHERE provider = ? GROUP BY voice"
      ).all(provider)
        .toVector
        .map(row => str(row.voice) -> str(row.count).toIntOption.getOrElse(0))
        .toMap
      ThreadVoiceAssignments.chooseVoice(threadId, enabledCatalog, counts).map { voice =>
        val _ = db.prepare(
          "INSERT INTO thread_provider_voice_assignments (provider, thread_id, voice) VALUES (?, ?, ?) " +
            "ON CONFLICT(provider, thread_id) DO UPDATE SET voice = excluded.voice"
        ).run(provider, threadId, voice)
        voice
      }
    }

  def publishVoiceChanged(threadId: String = ""): Unit =
    bb.realtime.publish("converse-voices", js.Dynamic.literal("threadId" -> threadId))

  def publishSignal(signalType: String, sessionId: String, threadId: String, extra: (String, js.Any)*): Unit =
    val payload = js.Dynamic.literal(
      "type"      -> signalType,
      "sessionId" -> sessionId,
      "threadId"  -> threadId,
      "eventId"   -> state.nextEventId()
    )
    extra.foreach((name, value) => payload.updateDynamic(name)(value))
    bb.realtime.publish("converse", payload)

  // --------------------------------------------------------- turn activity

  /** Follows the session's thread while the session is active and publishes
    * an "activity" signal to it whenever the activity its turn events say the
    * agent is in changes. Only turn and item boundaries are fetched; deltas
    * are ignored, so a streaming reply costs no requests.
    */
  final class TurnActivityWatch(session: ActiveSession):
    private val BoundaryEventTypes = js.Array("turn/started", "turn/completed", "item/started", "item/completed")
    private var tracker             = TurnActivityTracker.initial
    private var lastSeq: Option[String] = None
    private var fetching            = false
    private var fetchAgain          = false
    private var stopped             = false
    private val unsubscribe = bb.sdk.subscribe(
      js.Dynamic.literal(
        "event"    -> "thread:changed",
        "threadId" -> session.threadId,
        "callback" -> js.Any.fromFunction1 { (change: js.Dynamic) =>
          val eventTypes = change.metadata
            .asInstanceOf[js.UndefOr[js.Dynamic]]
            .flatMap(metadata => metadata.eventTypes.asInstanceOf[js.UndefOr[js.Array[String]]])
          if eventTypes.exists(_.exists(BoundaryEventTypes.contains)) then fetch()
        }
      )
    )
    fetch()

    def stop(): Unit =
      stopped = true
      unsubscribe()

    private def turnEvent(row: js.Dynamic): Option[TurnEvent] = str(row.`type`) match
      case "turn/started"   => Some(TurnEvent.TurnStarted)
      case "turn/completed" => Some(TurnEvent.TurnCompleted)
      case "item/started"   => Some(TurnEvent.ItemStarted(str(row.data.item.id), str(row.data.item.`type`)))
      case "item/completed" => Some(TurnEvent.ItemCompleted(str(row.data.item.id)))
      case _                => None

    /** One request at a time; a change arriving mid-request queues one more. */
    private def fetch(): Unit =
      if fetching then fetchAgain = true
      else
        fetching = true
        val request = js.Dynamic.literal("threadId" -> session.threadId, "types" -> BoundaryEventTypes)
        // The first request seeds from the newest boundaries, so a session
        // started mid-turn knows the current activity.
        lastSeq match
          case Some(seq) => request.updateDynamic("afterSeq")(seq); request.updateDynamic("order")("asc")
          case None      => request.updateDynamic("order")("desc"); request.updateDynamic("limit")("100")
        val seeding = lastSeq.isEmpty
        bb.sdk.threads.events.list(request).toFuture
          .map { rows =>
            if !stopped then
              val before  = tracker.activity
              val ordered = if seeding then rows.toList.reverse else rows.toList
              ordered.foreach { row =>
                lastSeq = Some(str(row.seq))
                turnEvent(row).foreach(event => tracker = tracker(event))
              }
              // The seed is published even when unchanged: the tab may hold
              // the activity of the thread this session targeted before.
              if seeding || tracker.activity != before then
                publishSignal(
                  "activity",
                  session.sessionId,
                  session.threadId,
                  "activity" -> tracker.activity.map(_.signalName).orNull
                )
          }
          .recover { case e => bb.log.warn(s"could not read turn events of thread ${session.threadId}: ${e.getMessage}") }
          .foreach { _ =>
            fetching = false
            if fetchAgain && !stopped then
              fetchAgain = false
              fetch()
          }

  var activityWatch: TurnActivityWatch = null

  /** Sets the active session and keeps the activity watch on its thread. */
  def activate(next: Option[ActiveSession]): Unit =
    if state.active != next then
      state.active = next
      if activityWatch != null then
        activityWatch.stop()
        activityWatch = null
      next.filter(_.threadId.nonEmpty).foreach(session => activityWatch = TurnActivityWatch(session))

  def soundCueSettings(): Future[SoundCueSettings] =
    bb.storage.kv.get(SoundCuesKey).toFuture.map { stored =>
      stored.asInstanceOf[js.UndefOr[js.Dynamic]].toOption.fold(SoundCueSettings.default) { value =>
        SoundCueSettings.fromUnchecked(SoundCueSettingsJson.read(value)) match
          case Right(settings) => settings
          case Left(message) =>
            bb.log.warn(s"stored sound cue settings ignored: $message")
            SoundCueSettings.default
      }
    }

  def soundCueSettingsResponse(settings: SoundCueSettings): js.Dynamic =
    val response = SoundCueSettingsJson.write(settings.unchecked)
    SoundCue.values.foreach { cue =>
      response.cues.selectDynamic(cue.settingKey).updateDynamic("isDefault")(settings.isDefault(cue))
    }
    response

  def storeSoundCueSettings(settings: SoundCueSettings): Future[js.Dynamic] =
    bb.storage.kv.set(SoundCuesKey, SoundCueSettingsJson.write(settings.unchecked)).toFuture.map { _ =>
      bb.realtime.publish("converse-sound-cues", js.Dynamic.literal())
      soundCueSettingsResponse(settings)
    }

  val StoredRecordingWaitMs = 3 * 60_000.0

  /** A wait whose tool call can no longer return ends, so the next utterance
    * goes to the thread as a message instead of into the dead call.
    */
  def endWaitForMoreSpeech(threadId: String, because: String): Unit =
    if state.continuationWaiters.expire(threadId) then
      bb.log.info(s"wait for more speech on $threadId ended: $because")

  def waitForMoreSpeech(threadId: String): Future[String] =
    awaitingReplies.get(threadId) match
      case None =>
        Future.successful(
          "No active voice turn is awaiting a response. Continue normally without waiting for speech."
        )
      case Some(awaitedReply) =>
        settings.get().toFuture.flatMap { currentSettings =>
          val waitMs = str(currentSettings.continuationWaitMs).toDoubleOption
            .filter(ms => ms >= 1000 && ms <= 30000)
            .getOrElse(8000.0)
          val waiting = state.continuationWaiters.begin(threadId)
          publishSignal("awaiting-speech-begin", awaitedReply.sessionId, threadId)
          // The wait covers speech that has not started yet. Speech already
          // being captured is waited for to completion, however long it runs;
          // a recording stored and not yet delivered, only for a few minutes,
          // since its transcription can stall until the transcriber is fixed.
          val storedRecordingWaitEndsAt = js.Date.now() + waitMs + StoredRecordingWaitMs
          var timer: js.Dynamic = null
          def scheduleExpiry(delayMs: Double): Unit =
            timer = js.Dynamic.global.setTimeout(
              js.Any.fromFunction0 { () =>
                val keepWaiting = state.capturingFor.contains(threadId) ||
                  (recordingInProgressFor(threadId) && js.Date.now() < storedRecordingWaitEndsAt)
                if keepWaiting then scheduleExpiry(1000)
                else { val _ = state.continuationWaiters.expire(threadId) }
              },
              delayMs
            )
          scheduleExpiry(waitMs)
          waiting.map { continuation =>
            val _ = js.Dynamic.global.clearTimeout(timer)
            publishSignal("awaiting-speech-end", awaitedReply.sessionId, threadId)
            continuation match
              case true =>
                "The user continued speaking. What they said arrives with this result or right after it, " +
                  "as a voice message marked as continuing this one; treat the two as one message."
              case false if recordingInProgressFor(threadId) =>
                val stage =
                  if recordingAwaitingTranscriptionFor(threadId) then "it has not been transcribed yet"
                  else "it has been transcribed but not yet delivered"
                s"The user said more, and it was recorded, but $stage. " +
                  "It will arrive as a new message once it is; end your turn rather than waiting for it."
              case false =>
                s"No additional speech arrived within ${waitMs.toInt} milliseconds. " +
                  "Treat the preceding voice message as complete and continue."
          }
        }

  bb.agents.registerTool(
    js.Dynamic.literal(
      "name" -> "converse_wait_for_more_speech",
      "description" ->
        ("Wait briefly for the user to continue the current voice-transcribed message, and learn whether " +
          "they did and what they said. Use it when what you do next depends on that."),
      "instructions" ->
        ("Voice messages are enclosed in agent-only [heard] tags. " +
          "Treat their contents as speech recognition that may contain filler words, homophones, " +
          "punctuation errors, or spoken phrasing. " +
          "Your reply is read aloud; put a word you would stress in speech between single *asterisks* " +
          "and a voice that can stress it will, as in: I didn't say he *stole* it. " +
          "Call converse_wait_for_more_speech when your next reply or action depends on whether the user " +
          "says more and on what they say, so you can decide with that in hand. " +
          "That is the only reason to call it. An unfinished-sounding message is not by itself a reason: " +
          "if you are sure more is coming, or you have nothing to say yet, end your turn without replying " +
          "and the next utterance arrives as a new message. " +
          "Never wait by other means, such as sleeping: the tool has a bounded timeout and reports whether " +
          "speech arrived. " +
          "Prefer the tool over `bb converse wait`; the CLI exists for providers without native tools and, " +
          "under a sandboxed shell, cannot reach the bb server."),
      "presentation" -> js.Dynamic.literal(
        "label" -> js.Dynamic.literal(
          "pending"   -> "Listening for continued speech",
          "completed" -> "Finished listening for continued speech"
        )
      ),
      "parameters" -> js.Dynamic.literal(
        "type"                 -> "object",
        "properties"           -> js.Dynamic.literal(),
        "additionalProperties" -> false
      ),
      "execute" -> js.Any.fromFunction2 { (_: js.Dynamic, toolContext: js.Dynamic) =>
        val threadId = str(toolContext.threadId)
        // A tool call torn down mid-wait (the turn stopped or interrupted)
        // can no longer return speech; the wait ends, so the next utterance
        // goes to the thread as a message instead of into the dead call.
        toolContext.signal.asInstanceOf[js.UndefOr[js.Dynamic]].toOption.filter(_ != null).foreach { signal =>
          val _ = signal.addEventListener(
            "abort",
            js.Any.fromFunction0 { () =>
              endWaitForMoreSpeech(threadId, "its tool call was cancelled")
            },
            js.Dynamic.literal("once" -> true)
          )
        }
        waitForMoreSpeech(threadId).toJSPromise
      }
    )
  )

  bb.cli.register(
    js.Dynamic.literal(
      "name"    -> "converse",
      "summary" -> "Converse voice-session utilities",
      "commands" -> js.Array(
        js.Dynamic.literal(
          "name"    -> "wait",
          "summary" -> "Wait briefly for continued speech in the current thread (prefer the converse_wait_for_more_speech tool when it is available)",
          "usage"   -> "bb converse wait"
        ),
        js.Dynamic.literal(
          "name"    -> "status",
          "summary" -> "Print the live voice session as JSON: its thread and whether the user is mid-utterance",
          "usage"   -> "bb converse status"
        )
      ),
      "run" -> js.Any.fromFunction2 { (argv: js.Array[String], context: js.Dynamic) =>
        val result = argv.toList match
          case "wait" :: Nil =>
            val threadId = str(context.threadId)
            if threadId.isEmpty then
              Future.successful(
                js.Dynamic.literal(
                  "exitCode" -> 1,
                  "stderr"   -> "bb converse wait requires a current bb thread"
                )
              )
            else
              waitForMoreSpeech(threadId).map(message => js.Dynamic.literal("exitCode" -> 0, "stdout" -> message))
          case "status" :: Nil =>
            val status = js.Dynamic.literal(
              "active"    -> state.active.isDefined,
              "threadId"  -> state.active.map(_.threadId).orNull,
              "capturing" -> state.capturingFor.isDefined,
              "awaiting"  -> awaitingReplies.threads.toJSArray,
              "browser" -> state.active.flatMap(a => state.browserStates.get(a.sessionId)).fold(null: js.Any) {
                (reported, at) =>
                  val copy = js.Object.assign(js.Dynamic.literal(), reported.asInstanceOf[js.Object])
                  copy.asInstanceOf[js.Dynamic].updateDynamic("reportedAt")(new js.Date(at).toISOString())
                  copy
              }
            )
            Future.successful(js.Dynamic.literal("exitCode" -> 0, "stdout" -> js.JSON.stringify(status)))
          case _ =>
            Future.successful(
              js.Dynamic.literal(
                "exitCode" -> 1,
                "stderr"   -> "usage: bb converse wait | bb converse status"
              )
            )
        result.toJSPromise
      }
    )
  )

  /** Every conversation row of a timeline, nested ones included. */
  def conversationRows(timeline: js.Dynamic): Vector[js.Dynamic] =
    def array(value: js.Dynamic): Vector[js.Dynamic] =
      if value == null || js.isUndefined(value) || !js.Array.isArray(value) then Vector.empty
      else value.asInstanceOf[js.Array[js.Dynamic]].toVector
    def rows(row: js.Dynamic): Vector[js.Dynamic] =
      (if str(row.kind) == "conversation" then Vector(row) else Vector.empty) ++ array(row.children).flatMap(rows)
    array(timeline.rows).flatMap(rows)

  def rowSeq(row: js.Dynamic): Long = str(row.sourceSeqEnd).toDoubleOption.fold(0L)(_.toLong)

  def assistantMessagesFromTimeline(timeline: js.Dynamic): Vector[AssistantMessage] =
    conversationRows(timeline)
      .filter(row => str(row.role) == "assistant" && str(row.id).nonEmpty)
      .map(row => AssistantMessage(str(row.id), str(row.text), rowSeq(row)))

  def latestUserMessageSeq(timeline: js.Dynamic): Option[Long] =
    conversationRows(timeline).filter(row => str(row.role) == "user").map(rowSeq).maxOption

  /** A spoken utterance as thread input: the transcript between agent-only
    * markers so the agent knows it was heard, not typed.
    */
  /** The transcript as the thread's input; notes about what the user was
    * hearing are agent-only, like the markers, and come first.
    */
  def heardInput(text: String, notes: List[ReplyInterruption.Note] = Nil): js.Array[js.Dynamic] =
    val opener = "[heard]\n" + ReplyInterruption.preface(notes).fold("")(_ + "\n")
    js.Array(
      js.Dynamic.literal("type" -> "text", "text" -> opener, "visibility" -> "agent-only"),
      js.Dynamic.literal("type" -> "text", "text" -> text),
      js.Dynamic.literal("type" -> "text", "text" -> "[/heard]", "visibility" -> "agent-only")
    )

  def recentTimeline(threadId: String): Future[js.Dynamic] =
    bb.sdk.threads
      .timeline(
        js.Dynamic.literal(
          "threadId"         -> threadId,
          "includeNestedRows" -> "true",
          "segmentLimit"     -> "50"
        )
      )
      .toFuture

  def latestAssistantMessage(threadId: String): Future[Option[AssistantMessage]] =
    recentTimeline(threadId).map(timeline => AssistantReply.freshSince(None, assistantMessagesFromTimeline(timeline)))

  def audioFile(mimeType: String, audioBase64: String): js.Dynamic =
    val bytes = js.Dynamic.global.Buffer.from(audioBase64, "base64")
    // The transcription provider detects the container from the filename
    // extension; a bare Blob (filename "blob") is rejected as unavailable.
    val extension =
      if mimeType.startsWith("audio/webm") then "webm"
      else if mimeType.startsWith("audio/mp4") then "mp4"
      else if mimeType.startsWith("audio/ogg") then "ogg"
      else if mimeType.startsWith("audio/wav") then "wav"
      else "webm"
    js.Dynamic.newInstance(js.Dynamic.global.File)(
      js.Array(bytes),
      s"utterance.$extension",
      js.Dynamic.literal("type" -> mimeType)
    )

  def delayMs(ms: Double): Future[Unit] =
    val p = scala.concurrent.Promise[Unit]()
    val _ = js.Dynamic.global.setTimeout(js.Any.fromFunction0(() => { val _ = p.success(()) }), ms)
    p.future

  def transcribeViaBb(file: js.Dynamic, signal: js.UndefOr[js.Dynamic]): Future[Transcription] =
    bb.sdk.system
      .transcribeVoice(js.Dynamic.literal("file" -> file, "signal" -> signal))
      .toFuture
      .map(result => Transcription(str(result.text).trim, "bb", None))

  def transcribeViaEndpoint(
      provider: String,
      url: String,
      model: String,
      apiKey: String,
      language: String,
      file: js.Dynamic,
      precedingTranscript: String,
      diagnosticContext: String,
      signal: js.UndefOr[js.Dynamic]
  ): Future[Transcription] =
    val form = js.Dynamic.newInstance(js.Dynamic.global.FormData)()
    val _    = form.set("file", file)
    if model.nonEmpty then { val _ = form.set("model", model) }
    if language.nonEmpty then { val _ = form.set("language", language) }
    if precedingTranscript.nonEmpty then { val _ = form.set("prompt", precedingTranscript) }
    val _    = form.set("response_format", "verbose_json")
    val init = js.Dynamic.literal("method" -> "POST", "body" -> form, "signal" -> signal)
    if apiKey.nonEmpty then
      init.updateDynamic("headers")(js.Dynamic.literal("Authorization" -> s"Bearer $apiKey"))
    js.Dynamic.global
      .fetch(url, init)
      .asInstanceOf[js.Promise[js.Dynamic]]
      .toFuture
      .flatMap { response =>
        if !response.ok.asInstanceOf[Boolean] then
          response.text().asInstanceOf[js.Promise[String]].toFuture.flatMap { body =>
            val detail = body.trim.replaceAll("\\s+", " ").take(300)
            bb.log.warn(s"transcription endpoint ${str(response.status)}: $detail")
            val status = str(response.status)
            Future.failed(
              if status == "429" then new RuntimeException(s"${TranscriptionFailure.RateLimitedMessage}: $detail")
              else if status == "413" then new TranscriberRejectedLength(s"transcription endpoint returned $status: $detail")
              else if Set("400", "415", "422").contains(status) then new TranscriberRejectedRequest(s"transcription endpoint returned $status: $detail")
              else new RuntimeException(s"transcription endpoint returned $status: $detail")
            )
          }
        else response.json().asInstanceOf[js.Promise[js.Dynamic]].toFuture
      }
      .map { payload =>
        // Whisper hallucinates stock phrases on silence/noise. Apply its own
        // reference heuristic per segment: high no-speech probability combined
        // with low average log-probability means the segment is not speech.
        val segments = payload.segments.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]]
        val transcriber = s"$provider:$model"
        segments.fold(Transcription(str(payload.text).trim, transcriber, None)) { segs =>
          val reported = segs.toList.map { seg =>
            val start = seg.start.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
            TranscriptSegment(
              startSec = start,
              endSec = seg.end.asInstanceOf[js.UndefOr[Double]].getOrElse(start),
              noSpeechProbability = seg.no_speech_prob.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0),
              avgLogProb = seg.avg_logprob.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0),
              text = str(seg.text).trim
            )
          }
          val audioDurationSec = payload.duration.asInstanceOf[js.UndefOr[Double]].toOption
          val evidence =
            audioDurationSec.fold("")(d => f"audio=$d%.2fs ") +
              reported.zipWithIndex.map { (seg, index) =>
                f"$index%d:${seg.startSec}%.2f-${seg.endSec}%.2fs,noSpeech=${seg.noSpeechProbability}%.3f," +
                  f"avgLogProb=${seg.avgLogProb}%.3f,chars=${seg.text.length}%d"
              }.mkString("[", "; ", "]")
          bb.log.info(s"transcription segment evidence ($diagnosticContext): $evidence")
          val kept = TranscriptSegments.keep(reported, audioDurationSec)
          if kept.length < reported.length then
            bb.log.info(s"dropped ${reported.length - kept.length} segment(s) as not speech ($diagnosticContext): " +
              reported.filterNot(kept.contains).map(_.text).mkString(" | "))
          Transcription(kept.map(_.text).filter(_.nonEmpty).mkString(" ").trim, transcriber, Some(evidence))
        }
      }

  /** `isFinal` marks the transcription of a whole utterance, which becomes a
    * message; it is worth extra effort not to lose, unlike a preview.
    */
  def transcribeAudio(
      mimeType: String,
      audioBase64: String,
      isFinal: Boolean,
      precedingTranscript: String,
      diagnosticContext: String,
      signal: js.UndefOr[js.Dynamic] = js.undefined
  ): Future[Transcription] =
    val file = audioFile(mimeType, audioBase64)
    def viaBb: Future[Transcription] =
      transcribeViaBb(file, signal).recoverWith {
        case e if isFinal && e.getMessage != null && e.getMessage.contains("503") =>
          bb.log.warn(s"transcription unavailable; retrying once: ${e.getMessage}")
          delayMs(1500).flatMap(_ => transcribeViaBb(file, signal))
      }
    // A provider's quota refills slowly, so retrying it would lose the
    // utterance; bb's own transcriber has a separate allowance.
    def viaBbWhenRateLimited(attempt: Future[Transcription]): Future[Transcription] =
      attempt.recoverWith {
        case e if isFinal && TranscriptionFailure.classify(Option(e.getMessage).getOrElse("")) == TranscriptionFailure.RateLimited =>
          bb.log.warn(s"transcription rate limited ($diagnosticContext); transcribing with bb instead")
          viaBb
      }
    settings.get().toFuture.flatMap { s =>
      val language = str(s.sttLanguage)
      if isFinal then bb.log.info(s"transcribing with ${str(s.sttProvider)} ($diagnosticContext)")
      str(s.sttProvider) match
        case "groq" =>
          viaBbWhenRateLimited(
            transcribeViaEndpoint(
              "groq",
              "https://api.groq.com/openai/v1/audio/transcriptions",
              str(s.groqModel),
              str(s.groqApiKey),
              language,
              file,
              precedingTranscript,
              diagnosticContext,
              signal
            )
          )
        case "openai-compatible" =>
          val url = str(s.sttCustomUrl)
          if url.isEmpty then Future.failed(new RuntimeException("sttCustomUrl is not configured"))
          else
            viaBbWhenRateLimited(
              transcribeViaEndpoint(
                "openai-compatible",
                url,
                str(s.sttCustomModel),
                str(s.sttCustomApiKey),
                language,
                file,
                precedingTranscript,
                diagnosticContext,
                signal
              )
            )
        case "parakeet" =>
          // When Parakeet cannot take a recording: Groq's Whisper when a key is
          // set, else bb's. Whisper seldom invents words in long speech.
          def viaWhisper: Future[Transcription] =
            val groqKey = str(s.groqApiKey)
            if groqKey.isEmpty then viaBb
            else
              viaBbWhenRateLimited(
                transcribeViaEndpoint(
                  "groq",
                  "https://api.groq.com/openai/v1/audio/transcriptions",
                  str(s.groqModel),
                  groqKey,
                  language,
                  file,
                  precedingTranscript,
                  diagnosticContext,
                  signal
                )
              )
          ensureService(parakeetFor(s)).flatMap {
            case Left(message) =>
              bb.log.warn(s"Parakeet unavailable ($diagnosticContext): $message")
              viaWhisper
            case Right(url) =>
              // Parakeet's own deadline, inside the caller's: when it passes
              // first, the recording goes to Whisper instead of waiting on a
              // server that may have hung.
              val parakeetDeadline = js.Dynamic.global.AbortSignal.timeout(ParakeetServer.AnswerWithinMs)
              val parakeetSignal =
                if js.isUndefined(signal) then parakeetDeadline
                else js.Dynamic.global.AbortSignal.any(js.Array(signal, parakeetDeadline))
              transcribeViaEndpoint(
                "parakeet",
                url,
                ParakeetServer.Model,
                apiKey = "",
                language = "",
                file,
                precedingTranscript = "",
                diagnosticContext,
                parakeetSignal
              ).recoverWith {
                case e: TranscriberRejectedLength =>
                  bb.log.info(s"too long for Parakeet ($diagnosticContext): ${e.getMessage}")
                  viaWhisper
                case _ if parakeetDeadline.aborted.asInstanceOf[Boolean] &&
                    !signal.toOption.exists(_.aborted.asInstanceOf[Boolean]) =>
                  bb.log.warn(
                    s"Parakeet did not answer within ${ParakeetServer.AnswerWithinMs.toInt / 1000} s ($diagnosticContext); " +
                      "transcribing with Whisper instead"
                  )
                  viaWhisper
              }
          }
        case _ => viaBb
    }

  // ------------------------------------------------------- managed services

  /** Any HTTP response (even an error status) means the service is up;
    * only a network failure means it is down.
    */
  def probeService(url: String): Future[Boolean] =
    js.Dynamic.global
      .fetch(url, js.Dynamic.literal("method" -> "POST", "signal" -> js.Dynamic.global.AbortSignal.timeout(1500)))
      .asInstanceOf[js.Promise[js.Dynamic]]
      .toFuture
      .map(_ => true)
      .recover(_ => false)

  def connectedHostId(): Future[Option[String]] =
    bb.sdk.hosts.list(js.Dynamic.literal()).toFuture.map { result =>
      val rows =
        if js.Array.isArray(result.asInstanceOf[js.Any]) then result.asInstanceOf[js.Array[js.Dynamic]]
        else result.hosts.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].getOrElse(js.Array[js.Dynamic]())
      rows.find(h => str(h.status) == "connected").map(h => str(h.id))
    }

  def touchServiceUse(service: ManagedService): Unit =
    bb.storage.kv.set(s"${service.key}.lastUseAt", js.Date.now()).toFuture.failed.foreach { e =>
      bb.log.warn(s"could not record when ${service.label} was last used: ${e.getMessage}")
    }

  def homeDirectory: String = str(js.Dynamic.global.process.env.HOME) match
    case "" => "/tmp"
    case h  => h

  def writeServiceFiles(service: ManagedService): Unit =
    val fs   = js.Dynamic.global.process.getBuiltinModule("node:fs")
    val path = js.Dynamic.global.process.getBuiltinModule("node:path")
    service.files.foreach { (relative, content) =>
      val file = path.join(homeDirectory, relative)
      val _    = fs.mkdirSync(path.dirname(file), js.Dynamic.literal("recursive" -> true))
      val _    = fs.writeFileSync(file, content)
    }

  /** What a check of a managed service found. Stale needs positive evidence:
    * a different version, or none reported by a server too old to have one.
    * A version check that times out means the service is busy, unless it has
    * gone unanswered for longer than any real job keeps it busy, when it is
    * unresponsive. Unverified: it answered, but not with a version.
    */
  enum ServiceState:
    case Down, Busy, Unverified, Unresponsive, Stale, Current

  def serviceState(service: ManagedService): Future[ServiceState] =
    service.versionCheck match
      case None => probeService(service.probeUrl).map(up => if up then ServiceState.Current else ServiceState.Down)
      case Some(check) =>
        js.Dynamic.global
          .fetch(check.url, js.Dynamic.literal("signal" -> js.Dynamic.global.AbortSignal.timeout(1500)))
          .asInstanceOf[js.Promise[js.Dynamic]]
          .toFuture
          .flatMap { response =>
            if str(response.status) == "404" then Future.successful(ServiceState.Stale)
            else if !response.ok.asInstanceOf[Boolean] then Future.successful(ServiceState.Unverified)
            else
              response.json().asInstanceOf[js.Promise[js.Dynamic]].toFuture.map { body =>
                if str(body.version) == check.expected then ServiceState.Current else ServiceState.Stale
              }
          }
          .recover {
            case js.JavaScriptException(error) if str(error.asInstanceOf[js.Dynamic].name) == "TimeoutError" =>
              ServiceState.Busy
            case _ => ServiceState.Down
          }
          .map {
            case ServiceState.Busy =>
              val since = state.serviceBusySince.getOrElseUpdate(service.key, js.Date.now())
              if js.Date.now() - since > ServiceHungAfterMs then ServiceState.Unresponsive
              else ServiceState.Busy
            case other =>
              val _ = state.serviceBusySince.remove(service.key)
              other
          }

  def stopService(service: ManagedService): Future[Unit] =
    for
      terminalId <- bb.storage.kv.get(s"${service.key}.terminalId").toFuture
      _ <- str(terminalId) match
        case "" => Future.unit
        case tid =>
          bb.sdk.terminals
            .close(js.Dynamic.literal("terminalId" -> tid, "mode" -> "force"))
            .toFuture
            .map(_ => ())
            .recover { case e => bb.log.warn(s"could not close the ${service.label} terminal $tid: ${e.getMessage}") }
      _ <- bb.storage.kv.delete(s"${service.key}.terminalId").toFuture
      _ <- bb.storage.kv.delete(s"${service.key}.lastUseAt").toFuture
    yield ()

  /** The service's URL once it answers, starting it first if it is down.
    * Concurrent callers share one check, so a replacement starts once.
    */
  def ensureService(service: ManagedService): Future[Either[String, String]] =
    state.servicesStarting.getOrElse(
      service.key, {
        val ensured = ensureServiceNow(service)
        state.servicesStarting.update(service.key, ensured)
        ensured.onComplete(_ => state.servicesStarting.remove(service.key))
        ensured
      }
    )

  def ensureServiceNow(service: ManagedService): Future[Either[String, String]] =
    val url = service.probeUrl
    def awaitDown(attempt: Int): Future[Unit] =
      serviceState(service).flatMap {
        case ServiceState.Down => Future.unit
        case _ if attempt < 30 => delayMs(1000).flatMap(_ => awaitDown(attempt + 1))
        case _                 => Future.unit
      }
    // Right(running): whether a usable instance is already up; Left: the port
    // is held by an instance this plugin cannot stop.
    val running: Future[Either[String, Boolean]] =
      serviceState(service).flatMap {
        case ServiceState.Down                        => Future.successful(Right(false))
        case ServiceState.Current | ServiceState.Busy | ServiceState.Unverified => Future.successful(Right(true))
        case found @ (ServiceState.Stale | ServiceState.Unresponsive) =>
          val reason =
            if found == ServiceState.Stale then "the running instance is not the current version"
            else s"the running instance has not answered for over ${(ServiceHungAfterMs / 1000).toInt} s"
          bb.log.warn(s"replacing managed ${service.label}: $reason")
          stopService(service).flatMap(_ => awaitDown(0)).flatMap(_ => serviceState(service)).map {
            case ServiceState.Down => Right(false)
            case _                 => Left(s"the port of ${service.label} is held by an instance this plugin did not start")
          }
      }
    running
      .flatMap {
        case Left(message) =>
          bb.log.error(s"$message; stop that process so the plugin can start its own")
          Future.successful(Left(message))
        case Right(true) =>
          touchServiceUse(service)
          Future.successful(Right(url))
        case Right(false) =>
          val home    = homeDirectory
          val command = service.startCommand(home)
          bb.log.info(s"starting managed ${service.label}: $command")
          state.active.foreach { a =>
            publishSignal("status", a.sessionId, a.threadId, "text" -> s"Starting ${service.label}…")
          }
          val started =
            connectedHostId()
              .flatMap {
                case None => Future.successful(Left(s"no connected host to start ${service.label} on"))
                case Some(hostId) =>
                  writeServiceFiles(service)
                  bb.sdk.terminals
                    .create(
                      js.Dynamic.literal(
                        "cols"  -> 120,
                        "rows"  -> 30,
                        "scope" -> js.Dynamic.literal("kind" -> "host_path", "hostId" -> hostId, "cwd" -> home),
                        "title" -> s"${service.key} (converse)",
                        "start" -> js.Dynamic.literal("mode" -> "command", "command" -> command)
                      )
                    )
                    .toFuture
                    .flatMap { result =>
                      val terminal =
                        result.terminal.asInstanceOf[js.UndefOr[js.Dynamic]].getOrElse(result)
                      val terminalId = str(terminal.id)
                      bb.storage.kv.set(s"${service.key}.terminalId", terminalId).toFuture.map(_ => terminalId)
                    }
                    .flatMap { terminalId =>
                      def exited: Future[Boolean] =
                        bb.sdk.terminals.get(js.Dynamic.literal("terminalId" -> terminalId)).toFuture
                          .map { result =>
                            val terminal = result.terminal.asInstanceOf[js.UndefOr[js.Dynamic]].getOrElse(result)
                            // A terminal that has stopped carries its exit code or the reason it closed.
                            str(terminal.exitCode).nonEmpty || str(terminal.closeReason).nonEmpty
                          }
                          .recover(_ => true)
                      def awaitReady(attempt: Int): Future[Either[String, String]] =
                        serviceState(service).map(_ == ServiceState.Current).flatMap {
                          case true =>
                            bb.log.info(s"${service.label} is up (terminal $terminalId)")
                            touchServiceUse(service)
                            Future.successful(Right(url))
                          case false if attempt >= service.readyWithinSeconds =>
                            Future.successful(
                              Left(s"${service.label} did not become ready within ${service.readyWithinSeconds}s")
                            )
                          case false =>
                            exited.flatMap {
                              case true =>
                                bb.log.warn(s"${service.label} exited before it was ready (terminal $terminalId)")
                                Future.successful(Left(s"${service.label} exited before it was ready"))
                              case false => delayMs(1000).flatMap(_ => awaitReady(attempt + 1))
                            }
                        }
                      awaitReady(0)
                    }
              }
              .recover { case e => Left(s"could not start ${service.label}: ${e.getMessage}") }
          started
    }

  /** Qwen3-TTS's URL once it answers. When Parakeet is the transcriber it is
    * seen to first, so one still on the GPU moves to the CPU before Qwen3-TTS
    * needs that memory.
    */
  def ensureQwenTts(currentSettings: js.Dynamic): Future[Either[String, String]] =
    val parakeetSettled =
      if str(currentSettings.sttProvider) == "parakeet" then ensureService(parakeetFor(currentSettings)).map(_ => ())
      else Future.unit
    parakeetSettled.flatMap(_ => ensureService(QwenService))

  bb.background.schedule(
    "service-idle",
    "*/5 * * * *",
    () =>
      Future
        .traverse(ManagedServices) { service =>
          for
            terminalId <- bb.storage.kv.get(s"${service.key}.terminalId").toFuture
            lastUse    <- bb.storage.kv.get(s"${service.key}.lastUseAt").toFuture
            _ <- {
              val tid  = str(terminalId)
              val last = lastUse.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
              if tid.nonEmpty && last > 0 && js.Date.now() - last > service.idleMinutes * 60_000.0 then
                bb.log.info(s"stopping managed ${service.label} after ${service.idleMinutes} idle minutes")
                stopService(service)
              else Future.unit
            }
          yield ()
        }
        .map(_ => ())
        .toJSPromise
  )

  // ------------------------------------------------------ stored recordings

  /** What became of a stored recording. */
  enum RecordingOutcome:
    case Delivered(text: String, createdThreadId: Option[String])
    case NothingSaid
    /** `sayingAgainHelps` is false when the same words would meet the same
      * obstacle, so the user should not be asked to repeat them.
      */
    case NotDelivered(reason: String, sayingAgainHelps: Boolean)

  /** Delivery errors carry no type to tell a passing fault from a lasting one,
    * so delivery is tried this many times before the recording is marked
    * failed and left for a manual retry.
    */
  val DeliveryAttempts = 6
  val TranscriptionTimeoutMs = 90_000.0

  def enqueueRecording(id: String): Unit =
    state.recordingQueue = state.recordingQueue.transformWith { _ =>
      processRecording(id).recover { case e =>
        bb.log.error(s"recording $id could not be processed: ${e.getMessage}")
      }
    }

  def processRecording(id: String): Future[Unit] =
    db.prepare("SELECT * FROM utterances WHERE id = ?").get(id).toOption match
      case Some(row) if recordingStatus(row).contains(RecordingStatus.Received) =>
        val sessionId = str(row.session_id)
        val threadId  = str(row.thread_id)
        val projectId = str(row.project_id)
        val context =
          if threadId.nonEmpty then s"final thread=$threadId recording=$id" else s"final project=$projectId recording=$id"
        val notes = Option(row.notes.asInstanceOf[String | Null])
          .map(json => js.JSON.parse(json).asInstanceOf[js.Array[String]].toList)
          .getOrElse(Nil)
          .map(ReplyInterruption.Note(threadId, _))
        val transcribed: Future[Either[String, String]] =
          if row.transcriber != null then Future.successful(Right(str(row.text)))
          else
            val audioBase64 = js.Dynamic.global.Buffer.from(row.audio).applyDynamic("toString")("base64").asInstanceOf[String]
            transcribeUntilDone(id, str(row.mime_type), audioBase64, context, attempt = 1).map(_.map { transcription =>
              bb.log.info(s"transcribed ($context) ${transcription.text.length} chars")
              saveTranscription(id, transcription)
              transcription.text
            })
        transcribed
          .flatMap {
            case Left(reason)                => Future.successful(RecordingOutcome.NotDelivered(reason, sayingAgainHelps = true))
            case Right(text) if text.isEmpty => Future.successful(RecordingOutcome.NothingSaid)
            case Right(text) =>
              deliverUntilDone(id, sessionId, threadId, projectId, text, notes, attempt = 1)
          }
          .map { outcome =>
            setRecordingStatus(
              id,
              outcome match
                case RecordingOutcome.Delivered(_, _)      => RecordingStatus.Sent
                case RecordingOutcome.NothingSaid          => RecordingStatus.Empty
                case RecordingOutcome.NotDelivered(reason, _) => RecordingStatus.Failed(reason)
            )
            outcome match
              case RecordingOutcome.NotDelivered(_, sayingAgainHelps) =>
                val _ = db.prepare("UPDATE utterances SET say_again = ? WHERE id = ?").run(if sayingAgainHelps then 1 else 0, id)
              case _ => ()
            val signalThread = outcome match
              case RecordingOutcome.Delivered(_, Some(created)) => created
              case _                                            => threadId
            val details: Seq[(String, js.Any)] = outcome match
              case RecordingOutcome.Delivered(text, created) =>
                Seq("outcome" -> "sent", "text" -> text, "createdThreadId" -> created.orNull)
              case RecordingOutcome.NothingSaid          => Seq("outcome" -> "empty")
              case RecordingOutcome.NotDelivered(reason, sayingAgainHelps) =>
                Seq("outcome" -> "failed", "error" -> reason, "sayAgain" -> sayingAgainHelps)
            publishSignal("utterance-processed", sessionId, signalThread, (("recordingId" -> (id: js.Any)) +: details)*)
          }
      case _ => Future.unit

  /** Transcribes until it succeeds, or until the transcriber calls the request
    * malformed (Left with the reason). Any other failure is retried: the
    * recording is stored, so waiting costs nothing but time.
    */
  def transcribeUntilDone(
      id: String,
      mimeType: String,
      audioBase64: String,
      context: String,
      attempt: Int
  ): Future[Either[String, Transcription]] =
    bb.log.info(s"transcription attempt $attempt started ($context): ${audioBase64.length} b64 chars")
    // A transcriber that never answers would otherwise hold every later
    // recording behind this one; the request is cancelled and retried.
    val signal = js.Dynamic.global.AbortSignal.timeout(TranscriptionTimeoutMs)
    transcribeAudio(mimeType, audioBase64, isFinal = true, precedingTranscript = "", diagnosticContext = context, signal = signal)
      .map(Right(_))
      .recoverWith {
        case e: TranscriberRejectedRequest => Future.successful(Left(e.getMessage))
        case e =>
          val delay = RetryBackoff.delayBeforeRetry(attempt)
          bb.log.warn(s"transcription failed ($context, attempt $attempt; retrying in ${delay}ms): ${e.getMessage}")
          noteRecordingError(id, e.getMessage)
          delayMs(delay).flatMap(_ => transcribeUntilDone(id, mimeType, audioBase64, context, attempt + 1))
      }

  /** The `code` bb puts on the errors its API answers with, if `e` is one. */
  def bbErrorCode(e: Throwable): Option[String] = e match
    case js.JavaScriptException(error) =>
      error.asInstanceOf[js.Dynamic].code.asInstanceOf[js.UndefOr[String]].toOption
    case _ => None

  def deliverUntilDone(
      id: String,
      sessionId: String,
      threadId: String,
      projectId: String,
      text: String,
      notes: List[ReplyInterruption.Note],
      attempt: Int
  ): Future[RecordingOutcome] =
    bb.log.info(s"delivering recording $id (attempt $attempt)")
    setRecordingStatus(id, RecordingStatus.Sending)
    deliver(id, sessionId, threadId, projectId, text, notes)
      .map(created => RecordingOutcome.Delivered(text, created))
      .recoverWith { case e =>
        setRecordingStatus(id, RecordingStatus.Received)
        noteRecordingError(id, e.getMessage)
        if bbErrorCode(e).contains("thread_environment_unavailable") then
          // The thread's workspace was destroyed or never attached; nothing
          // reaches it until the workspace is restored.
          Future.successful(
            RecordingOutcome.NotDelivered(
              s"the thread's workspace is unavailable, so it cannot take messages (${e.getMessage})",
              sayingAgainHelps = false
            )
          )
        else if attempt >= DeliveryAttempts then
          Future.successful(
            RecordingOutcome.NotDelivered(
              s"could not be delivered after $attempt attempts: ${e.getMessage}",
              sayingAgainHelps = true
            )
          )
        else
          val delay = RetryBackoff.delayBeforeRetry(attempt)
          bb.log.warn(s"delivery of recording $id failed (attempt $attempt; retrying in ${delay}ms): ${e.getMessage}")
          delayMs(delay).flatMap(_ => deliverUntilDone(id, sessionId, threadId, projectId, text, notes, attempt + 1))
      }

  /** Hands the transcript to its thread, or starts a thread with it; the
    * started thread's id, if one was started.
    */
  def deliver(
      id: String,
      sessionId: String,
      threadId: String,
      projectId: String,
      text: String,
      notes: List[ReplyInterruption.Note]
  ): Future[Option[String]] =
    if threadId.isEmpty then
      bb.sdk.threads
        .spawn(
          js.Dynamic.literal(
            "projectId"   -> projectId,
            "environment" -> js.Dynamic.literal("type" -> "project-default"),
            "input"       -> heardInput(text)
          )
        )
        .toFuture
        .map { thread =>
          val newThreadId = str(thread.id)
          bb.log.info(s"started thread $newThreadId in project $projectId by voice")
          setRecordingThread(id, newThreadId)
          if state.active.exists(_.sessionId == sessionId) then activate(state.active.map(_.copy(threadId = newThreadId)))
          awaitingReplies.put(newThreadId, AwaitingReply(sessionId, None))
          Some(newThreadId)
        }
    else
      // Speech that continues a message an agent is waiting on goes to the
      // thread as a message, visible there and never lost to a wait whose
      // tool call has ended. The wait is told only once the message is in, so
      // the agent never looks for words not yet sent.
      val continuing = state.continuationWaiters.isWaiting(threadId)
      val notesToSend = if continuing then ReplyInterruption.forContinuation(threadId, notes) else notes
      def send(mode: String): Future[js.Dynamic] =
        bb.sdk.threads
          .send(js.Dynamic.literal("threadId" -> threadId, "mode" -> mode, "input" -> heardInput(text, notesToSend)))
          .toFuture
      latestAssistantMessage(threadId).flatMap { previousAssistantMessage =>
        // An utterance sent while an earlier one still awaits its reply keeps
        // the earlier baseline: the reply already written to the earlier one
        // has not been read aloud yet, and must not count as old.
        awaitingReplies.put(
          threadId,
          awaitingReplies.get(threadId) match
            case Some(earlier) => earlier.copy(sessionId = sessionId)
            case None          => AwaitingReply(sessionId, previousAssistantMessage.map(_.id))
        )
        send("steer-if-active")
          .recoverWith {
            // steer-if-active 409s ("Thread is not active") when the
            // thread is starting or errored; auto resolves those to a
            // plain start, so the utterance is not dropped.
            case e if e.getMessage != null && e.getMessage.contains("Thread is not active") =>
              send("auto")
          }
          .map { _ =>
            if continuing && state.continuationWaiters.continued(threadId) then
              bb.log.info(s"ended the wait for more speech on $threadId: the user continued")
            None
          }
          .recoverWith { case e =>
            val _ = awaitingReplies.remove(threadId)
            Future.failed(e)
          }
      }

  /** Logs why a browser session ended; `stopped` says the server's session
    * was stopped with it, rather than kept for the browser to resume.
    */
  def logSessionEnd(input: js.Dynamic, stopped: Boolean): Unit =
    val end    = Option(str(input.end)).filter(_.nonEmpty).getOrElse("unknown")
    val detail = Option(str(input.detail)).filter(_.nonEmpty).fold("")(d => s": $d")
    bb.log.info(s"voice session ${str(input.sessionId)} ended in the browser ($end$detail)" + (if stopped then "" else "; kept on the server"))

  bb.rpc.register(
    js.Dynamic.literal(
      // threadId is empty while the New thread screen is in view.
      "startSession" -> js.Dynamic.literal(
        "input"  -> Schema.stringsWithOptional(Seq("sessionId" -> 100), Seq("threadId" -> 100)),
        "output" -> Schema.passthrough
      ),
      "reportUiStall" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "reportBrowserState" -> js.Dynamic.literal(
        "input"  -> Schema.strings("sessionId" -> 100),
        "output" -> Schema.passthrough
      ),
      "reportSessionEnd" -> js.Dynamic.literal(
        "input"  -> Schema.strings("sessionId" -> 100),
        "output" -> Schema.passthrough
      ),
      "stopSession" -> js.Dynamic.literal(
        "input"  -> Schema.strings("sessionId" -> 100),
        "output" -> Schema.passthrough
      ),
      "submitUtterance" -> js.Dynamic.literal(
        // threadId or projectId: exactly one; the handler checks.
        "input" -> Schema.stringsWithOptional(
          Seq("sessionId" -> 100, "recordingId" -> 100, "mimeType" -> 100, "audioBase64" -> UploadLimit.MaxAudioBase64Chars),
          Seq("threadId" -> 100, "projectId" -> 100)
        ),
        "output" -> Schema.passthrough
      ),
      "previewUtterance" -> js.Dynamic.literal(
        "input" -> Schema.strings(
          "sessionId"   -> 100,
          "mimeType"    -> 100,
          "audioBase64" -> UploadLimit.MaxAudioBase64Chars
        ),
        "output" -> Schema.passthrough
      ),
      "reportDiscardedCapture" -> js.Dynamic.literal(
        "input"  -> Schema.stringsWithOptional(Seq("sessionId" -> 100), Seq("threadId" -> 100)),
        "output" -> Schema.passthrough
      ),
      "captureState" -> js.Dynamic.literal(
        "input"  -> Schema.stringsWithOptional(Seq("sessionId" -> 100), Seq("threadId" -> 100)),
        "output" -> Schema.passthrough
      ),
      "voiceSettings" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "updateVoiceSettings" -> js.Dynamic.literal(
        "input" -> Schema.passthrough,
        "output" -> Schema.passthrough
      ),
      "threadVoiceSettings" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "soundCueSettings" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "recentUtterances" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "retryRecording" -> js.Dynamic.literal("input" -> Schema.strings("id" -> 100), "output" -> Schema.passthrough),
      "sessionSync" -> js.Dynamic.literal("input" -> Schema.strings("sessionId" -> 100), "output" -> Schema.passthrough),
      "activitySpans" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "reportPlayback" -> js.Dynamic.literal(
        "input"  -> Schema.stringsWithOptional(Seq("id" -> 100, "threadId" -> 100, "source" -> 20), Seq("sessionId" -> 100)),
        "output" -> Schema.passthrough
      ),
      "utterance" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "utteranceForMessage" -> js.Dynamic.literal("input" -> Schema.strings("threadId" -> 100, "text" -> 100_000), "output" -> Schema.passthrough),
      "updateSoundCueSettings" -> js.Dynamic.literal(
        "input"  -> Schema.passthrough,
        "output" -> Schema.passthrough
      ),
      "resetSoundCue" -> js.Dynamic.literal(
        "input"  -> Schema.strings("key" -> 100),
        "output" -> Schema.passthrough
      ),
      "updateThreadVoiceSettings" -> js.Dynamic.literal(
        "input" -> Schema.passthrough,
        "output" -> Schema.passthrough
      ),
      "threadVoices" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "threadTitles" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "setThreadVoice" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "resolveThreadVoice" -> js.Dynamic.literal(
        "input" -> Schema.passthrough,
        "output" -> Schema.passthrough
      )
    ),
    js.Dynamic.literal(
      "startSession" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        val threadId  = str(input.threadId)
        state.active.foreach { old =>
          if old.sessionId != sessionId then publishSignal("revoked", old.sessionId, old.threadId)
        }
        activate(Some(ActiveSession(sessionId, threadId)))
        awaitingReplies.retainSessions(_ == sessionId)
        // Turns that ended while this session was not registered, as across a
        // backend reload: their replies are read aloud now.
        awaitingReplies.bySession(sessionId).foreach { awaitedThread =>
          bb.sdk.threads.get(js.Dynamic.literal("threadId" -> awaitedThread)).toFuture.foreach { thread =>
            if str(thread.status) == "idle" then replyOnceIdle(awaitedThread)
          }
        }
        state.continuationWaiters.expireAll()
        val how =
          Option(str(input.how)).filter(_.nonEmpty).getOrElse("unspecified") +
            input.attempt.asInstanceOf[js.UndefOr[Int]].toOption.fold("")(n => s", attempt $n")
        bb.log.info(s"voice session $sessionId started for thread $threadId ($how)")
        // Warm the managed services while the user starts talking so the first
        // transcription and spoken reply are not delayed by service startup.
        settings.get().toFuture.foreach { s =>
          val speechService = str(s.ttsProvider) match
            case "pocket-tts"            => Some(ensureService(PocketService))
            case QwenTtsProvider         => Some(ensureQwenTts(s))
            case ChatterboxNanoProvider  => Some(ensureService(ChatterboxService))
            case _                       => None
          speechService.foreach(_.foreach {
            case Left(message) => bb.log.warn(s"TTS warm-up: $message")
            case Right(_)      => ()
          })
          if str(s.sttProvider) == "parakeet" then
            ensureService(parakeetFor(s)).foreach {
              case Left(message) => bb.log.warn(s"transcriber warm-up: $message")
              case Right(_)      => ()
            }
        }
        js.Dynamic.literal("ok" -> true)
      },
      "reportUiStall" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        bb.log.warn(
          s"bb's UI thread was blocked for about ${str(input.lateMs).toDoubleOption.fold("?")(ms => f"${ms / 1000}%.1f")} s " +
            s"(voice ${if input.voice.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false) then "on" else "off"}, " +
            s"phase ${str(input.phase)}, capturing ${str(input.capturing)})"
        )
        js.Dynamic.literal("ok" -> true)
      },
      "reportBrowserState" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        if sessionId.nonEmpty then
          // Logged so a recording that reached an unexpected thread can be
          // traced to where the session was routing speech at the time.
          val target   = str(input.state.threadId)
          val previous = state.browserStates.get(sessionId).map((reported, _) => str(reported.threadId))
          if !previous.contains(target) then
            bb.log.info(s"session $sessionId now routes speech to ${if target.isEmpty then "no thread" else target}")
          state.browserStates.update(sessionId, (input.state, js.Date.now()))
        js.Dynamic.literal("ok" -> true)
      },
      "reportSessionEnd" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        logSessionEnd(input, stopped = false)
        js.Dynamic.literal("ok" -> true)
      },
      "stopSession" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        logSessionEnd(input, stopped = true)
        if state.active.exists(_.sessionId == sessionId) then
          activate(None)
          state.capturingFor = None
        awaitingReplies.retainSessions(_ != sessionId)
        state.continuationWaiters.expireAll()
        js.Dynamic.literal("ok" -> true)
      },
      "submitUtterance" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId   = str(input.sessionId)
        val recordingId = str(input.recordingId)
        val threadId    = str(input.threadId)
        // A projectId instead of a threadId starts a new thread in that project.
        val projectId = str(input.projectId)
        if !state.active.exists(_.sessionId == sessionId) then rejected(TranscriptionFailure.SessionForgottenMessage)
        else if threadId.isEmpty == projectId.isEmpty then rejected("exactly one of threadId or projectId is required")
        else
          state.capturingFor = None
          // A retry after a lost response: the recording is already stored.
          if !recordingStored(recordingId) then
            if threadId.nonEmpty then activate(state.active.map(_.copy(threadId = threadId)))
            val diagnosticContext =
              if threadId.nonEmpty then s"final thread=$threadId recording=$recordingId"
              else s"final project=$projectId recording=$recordingId"
            val evidence = input.evidence.asInstanceOf[js.UndefOr[js.Dynamic]].toOption.filter(_ != null)
            evidence.foreach { e =>
              bb.log.info(
                s"speech detector evidence ($diagnosticContext): " +
                  s"classifier=${str(e.classifier)}, frames=${str(e.frameCount)}, " +
                  s"speechFrames=${str(e.speechFrameCount)}, peakRms=${str(e.peakRms)}, " +
                  s"peakSpeechProbability=${str(e.peakSpeechProbability)}, " +
                  s"positiveSpeechThreshold=${str(e.positiveSpeechThreshold)}"
              )
            }
            bb.log.info(
              s"utterance received ($diagnosticContext): ${str(input.audioBase64).length} b64 chars, ${str(input.mimeType)}"
            )
            val heardContext = input.heardContext.asInstanceOf[js.UndefOr[js.Dynamic]].toOption.filter(_ != null)
            val speaking = heardContext
              .map(_.interrupted)
              .filter(i => i != null && !js.isUndefined(i))
              .map(i => ReplyInterruption.SpeakingReply(str(i.threadId), str(i.sentence)))
            // Decided now, as spoken, not when the recording is delivered: by
            // then the reply may have been read aloud. Unheard: the thread is
            // still answering an earlier utterance, has one not yet delivered
            // to it, or its answer is queued unplayed.
            val targetReplyUnheard =
              awaitingReplies.contains(threadId) || recordingInProgressFor(threadId) ||
                heardContext.exists(_.targetReplyUnplayed.asInstanceOf[Any] == true)
            val notes = ReplyInterruption.notes(threadId, speaking, targetReplyUnheard)
            notes.filter(_.threadId != threadId).foreach { note =>
              state.pendingNotes.updateWith(note.threadId)(prior => Some(prior.getOrElse(Nil) :+ note.text))
            }
            val notesForThread =
              if threadId.isEmpty then Nil
              else state.pendingNotes.remove(threadId).getOrElse(Nil) ++ notes.filter(_.threadId == threadId).map(_.text)
            storeRecording(
              ReceivedRecording(
                id = recordingId,
                at = js.Date.now(),
                sessionId = sessionId,
                threadId = threadId,
                projectId = projectId,
                mimeType = str(input.mimeType),
                audioBase64 = str(input.audioBase64),
                detector = evidence.map { e =>
                  DetectorEvidence(
                    frames = e.frameCount.asInstanceOf[Int],
                    speechFrames = e.speechFrameCount.asInstanceOf[Int],
                    peakSpeechProbability = e.peakSpeechProbability.asInstanceOf[Double],
                    peakRms = e.peakRms.asInstanceOf[Double]
                  )
                },
                notes = notesForThread,
                speechStartAt = input.speechStartAt.asInstanceOf[js.UndefOr[Double]].toOption,
                speechEndAt = input.speechEndAt.asInstanceOf[js.UndefOr[Double]].toOption
              )
            )
            recordSpeechSpan(recordingId)
            bb.realtime.publish("converse-utterances", js.Dynamic.literal())
            enqueueRecording(recordingId)
          Future.successful(js.Dynamic.literal("stored" -> true)).toJSPromise
      },
      "reportPlayback" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val threadId = str(input.threadId)
        val startAt  = input.startAt.asInstanceOf[js.UndefOr[Double]].getOrElse(Double.NaN)
        val endAt    = input.endAt.asInstanceOf[js.UndefOr[Double]].getOrElse(Double.NaN)
        (ActivitySource.fromCode(str(input.source)).filter(ActivitySource.Reported.contains), PlaybackReport.validate(threadId, startAt, endAt)) match
          case (None, _)            => rejected(s"unknown source ${str(input.source)}")
          case (_, Left(message))   => rejected(message)
          case (Some(source), Right(_)) =>
            recordActivitySpan(
              str(input.id),
              ActivityKind.Playback,
              source,
              threadId,
              startAt,
              endAt,
              Option(str(input.sessionId)).filter(_.nonEmpty)
            )
            Future.successful(js.Dynamic.literal("ok" -> true)).toJSPromise
      },
      "activitySpans" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        // Spans recorded after `afterSeq`, oldest first; a reader keeps the
        // last seq it saw and asks for those after it. `oldestSeq` is the
        // smallest seq still held, so a reader whose last seq is older can tell
        // that spans it never saw were purged.
        val afterSeq = input.afterSeq.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
        val limit    = input.limit.asInstanceOf[js.UndefOr[Double]].toOption.map(_.toInt).filter(_ > 0).fold(500)(_.min(1000))
        val items = db
          .prepare(
            "SELECT seq, id, kind, source, thread_id, start_at, end_at FROM activity_spans " +
              "WHERE seq > ? ORDER BY seq LIMIT ?"
          )
          .all(afterSeq, limit)
          .map { row =>
            js.Dynamic.literal(
              "seq"      -> row.seq,
              "id"       -> row.id,
              "threadId" -> row.thread_id,
              "kind"     -> row.kind,
              "source"   -> row.source,
              "start"    -> row.start_at,
              "end"      -> row.end_at
            )
          }
        val oldestSeq = db.prepare("SELECT MIN(seq) AS seq FROM activity_spans").get().toOption.map(_.seq).orNull
        Future.successful(js.Dynamic.literal("items" -> items, "oldestSeq" -> oldestSeq)).toJSPromise
      },
      "sessionSync" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        // The backend's records for a page's session: how the given recordings
        // ended, for those that have (the facts the utterance-processed signal
        // carries), and which threads owe the session a reply. Safe to ask
        // repeatedly, for a page that missed a signal.
        val sessionId = str(input.sessionId)
        val ids       = input.recordingIds.asInstanceOf[js.UndefOr[js.Array[String]]].fold(Vector.empty[String])(_.toVector).take(50)
        val outcomes = ids.flatMap { id =>
          db.prepare("SELECT id, status, text, thread_id, project_id, last_error, say_again FROM utterances WHERE id = ?")
            .get(id)
            .toOption
            .flatMap { row =>
              recordingStatus(row).collect {
                case RecordingStatus.Sent =>
                  js.Dynamic.literal(
                    "recordingId"     -> row.id,
                    "outcome"         -> "sent",
                    "threadId"        -> row.thread_id,
                    "text"            -> row.text,
                    "createdThreadId" -> (if str(row.project_id).nonEmpty then row.thread_id else null)
                  )
                case RecordingStatus.Empty => js.Dynamic.literal("recordingId" -> row.id, "outcome" -> "empty")
                case RecordingStatus.Failed(reason) =>
                  js.Dynamic.literal(
                    "recordingId" -> row.id,
                    "outcome"     -> "failed",
                    "error"       -> reason,
                    "sayAgain"    -> (row.say_again == null || row.say_again.asInstanceOf[Int] != 0)
                  )
              }
            }
        }
        val awaiting = awaitingReplies.bySession(sessionId)
        Future.successful(js.Dynamic.literal("outcomes" -> outcomes.toJSArray, "awaiting" -> awaiting.toJSArray)).toJSPromise
      },
      "retryRecording" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val id = str(input.id)
        db.prepare("SELECT status, last_error FROM utterances WHERE id = ?").get(id).toOption.flatMap(recordingStatus) match
          case Some(RecordingStatus.Failed(_)) =>
            // Its outcome is announced to whichever session is active now.
            state.active.foreach(session => db.prepare("UPDATE utterances SET session_id = ? WHERE id = ?").run(session.sessionId, id))
            setRecordingStatus(id, RecordingStatus.Received)
            enqueueRecording(id)
            Future.successful(js.Dynamic.literal("ok" -> true)).toJSPromise
          case _ => rejected("only a recording that failed can be retried")
      },
      "captureState" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        if state.active.exists(_.sessionId == sessionId) then
          val active = input.active.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false)
          state.capturingFor = if active then Some(str(input.threadId)) else None
        js.Dynamic.literal("ok" -> true)
      },
      "reportDiscardedCapture" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        val threadId  = str(input.threadId)
        val evidence  = input.evidence.asInstanceOf[js.UndefOr[js.Dynamic]]
        if state.active.exists(_.sessionId == sessionId) then evidence.foreach { e =>
          bb.log.info(
            s"speech candidate discarded (thread=$threadId): " +
              s"classifier=${str(e.classifier)}, frames=${str(e.frameCount)}, " +
              s"speechFrames=${str(e.speechFrameCount)}, peakRms=${str(e.peakRms)}, " +
              s"peakSpeechProbability=${str(e.peakSpeechProbability)}"
          )
        }
        js.Dynamic.literal("ok" -> true)
      },
      "previewUtterance" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        if !state.active.exists(_.sessionId == sessionId) then rejected(TranscriptionFailure.SessionForgottenMessage)
        else
          val requestedThreadId = str(input.threadId)
          val threadId =
            if requestedThreadId.nonEmpty then requestedThreadId
            else state.active.map(_.threadId).getOrElse("unknown")
          if requestedThreadId.nonEmpty then activate(state.active.map(_.copy(threadId = requestedThreadId)))
          val diagnosticContext = s"preview thread=$threadId request=${state.nextEventId()}"
          transcribeAudio(
            str(input.mimeType),
            str(input.audioBase64),
            isFinal = false,
            precedingTranscript = str(input.precedingTranscript),
            diagnosticContext = diagnosticContext
          )
            .map(transcription => js.Dynamic.literal("text" -> transcription.text))
            .toJSPromise
      },
      "voiceSettings" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val requestedProvider = str(input.provider)
        val providerF =
          if requestedProvider.nonEmpty then Future.successful(requestedProvider)
          else settings.get().toFuture.map(s => str(s.ttsProvider))
        providerF.flatMap(voiceSettingsResponse).toJSPromise
      },
      "utterance" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        Future.successful(js.Dynamic.literal("utterance" -> utteranceById(str(input.id)).orNull)).toJSPromise
      },
      "utteranceForMessage" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        Future.successful(js.Dynamic.literal("utterance" -> utteranceForMessage(str(input.threadId), str(input.text)).orNull)).toJSPromise
      },
      "recentUtterances" -> js.Any.fromFunction1 { (_: js.Dynamic) =>
        Future.successful(js.Dynamic.literal("utterances" -> listUtterances())).toJSPromise
      },
      "soundCueSettings" -> js.Any.fromFunction1 { (_: js.Dynamic) =>
        soundCueSettings().map(soundCueSettingsResponse).toJSPromise
      },
      "updateSoundCueSettings" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        SoundCueSettings.fromUnchecked(SoundCueSettingsJson.read(input)) match
          case Left(message)   => rejected(message)
          case Right(settings) => storeSoundCueSettings(settings).toJSPromise
      },
      "resetSoundCue" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val key = str(input.key)
        SoundCue.values.find(_.settingKey == key) match
          case None => rejected(s"unknown cue $key")
          case Some(cue) =>
            soundCueSettings().flatMap { current =>
              storeSoundCueSettings(current.copy(cues = current.cues.updated(cue, SoundCueSettings.default.cues(cue))))
            }.toJSPromise
      },
      "threadVoiceSettings" -> js.Any.fromFunction1 { (_: js.Dynamic) =>
        threadVoicesEnabled().map(enabled => js.Dynamic.literal("enabled" -> enabled)).toJSPromise
      },
      "updateThreadVoiceSettings" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val enabled = input.enabled.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false)
        bb.storage.kv.set(ThreadVoicesEnabledKey, enabled).toFuture.map { _ =>
          publishVoiceChanged()
          js.Dynamic.literal("ok" -> true)
        }.toJSPromise
      },
      "updateVoiceSettings" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val provider = str(input.provider)
        if !Set("browser", "pocket-tts", QwenTtsProvider, ChatterboxNanoProvider, "kokoro", "openai-compatible")
            .contains(provider)
        then
          rejected("unknown TTS provider")
        else
          (for
            previous <- ttsConfig(provider)
            enabledUpdate = input.enabled.asInstanceOf[js.UndefOr[Boolean]]
            requestedCatalog =
              if provider == "pocket-tts" then PocketEnglishVoices else stringArray(input.catalog)
            requestedEnabled = stringArray(input.voices).filter(requestedCatalog.contains)
            apiKey =
              if js.typeOf(input.apiKey) == "string" then str(input.apiKey) else str(previous.apiKey)
            config = js.Dynamic.literal(
              "url"          -> str(input.url),
              "model"        -> str(input.model),
              "defaultVoice" -> str(input.defaultVoice),
              "apiKey"       -> apiKey,
              "voices"       -> requestedCatalog.toJSArray
            )
            _ <- bb.storage.kv.set(ttsConfigKey(provider), config).toFuture
            _ <- enabledUpdate.fold(Future.successful(()))(enabled =>
              bb.storage.kv.set(ThreadVoicesEnabledKey, enabled).toFuture.map(_ => ())
            )
            _ <- bb.storage.kv.set(enabledVoicesKey(provider), requestedEnabled.toJSArray).toFuture
          yield
            val disabled = requestedCatalog.filterNot(requestedEnabled.contains)
            disabled.foreach { voice =>
              val _ = db.prepare(
                "DELETE FROM thread_provider_voice_assignments WHERE provider = ? AND voice = ?"
              ).run(provider, voice)
            }
            publishVoiceChanged()
            js.Dynamic.literal("ok" -> true)).toJSPromise
      },
      // The title of each of `threadIds` that can be read.
      "threadTitles" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val threadIds = input.threadIds.asInstanceOf[js.UndefOr[js.Array[String]]].getOrElse(js.Array()).toSeq
        Future
          .sequence(threadIds.map { id =>
            bb.sdk.threads
              .get(js.Dynamic.literal("threadId" -> id))
              .toFuture
              .map(thread => Option(str(thread.title)).filter(_.nonEmpty).orElse(Option(str(thread.titleFallback)).filter(_.nonEmpty)).map(id -> _))
              .recover(_ => None)
          })
          .map(found => js.Dynamic.literal("titles" -> js.Dictionary(found.flatten*)))
          .toJSPromise
      },
      // The voice catalog, and the voice assigned to each of `threadIds` that
      // has one.
      "threadVoices" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val threadIds = input.threadIds.asInstanceOf[js.UndefOr[js.Array[String]]].getOrElse(js.Array())
        settings.get().toFuture.flatMap { currentSettings =>
          val provider = str(currentSettings.ttsProvider)
          voiceSettingsResponse(provider).map { voiceSettings =>
            val assigned = js.Dynamic.literal()
            threadIds.foreach(id => storedThreadVoice(provider, id).foreach(assigned.updateDynamic(id)(_)))
            js.Dynamic.literal(
              "enabled"  -> voiceSettings.enabled,
              "provider" -> provider,
              "voices"   -> voiceSettings.voices,
              "assigned" -> assigned
            )
          }
        }.toJSPromise
      },
      "setThreadVoice" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val threadId = str(input.threadId)
        val voice    = str(input.voice)
        if threadId.isEmpty then rejected("threadId is required")
        else
          settings.get().toFuture.flatMap { currentSettings =>
            val provider = str(currentSettings.ttsProvider)
            ttsConfig(provider).map { config =>
              val catalog = availableVoices(provider, config)
              if voice.nonEmpty && !catalog.contains(voice) then throw new RuntimeException("unknown voice")
              if voice.isEmpty then
                val _ = db.prepare(
                  "DELETE FROM thread_provider_voice_assignments WHERE provider = ? AND thread_id = ?"
                ).run(provider, threadId)
              else
                val _ = db.prepare(
                  "INSERT INTO thread_provider_voice_assignments (provider, thread_id, voice) VALUES (?, ?, ?) " +
                    "ON CONFLICT(provider, thread_id) DO UPDATE SET voice = excluded.voice"
                ).run(provider, threadId, voice)
              publishVoiceChanged(threadId)
              js.Dynamic.literal("ok" -> true)
            }
          }.toJSPromise
      },
      "resolveThreadVoice" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val threadId = str(input.threadId)
        if threadId.isEmpty then rejected("threadId is required")
        else
          (for
            currentSettings <- settings.get().toFuture
            provider         = str(currentSettings.ttsProvider)
            enabled         <- threadVoicesEnabled()
            config          <- ttsConfig(provider)
            catalog          = availableVoices(provider, config)
            voices          <- enabledVoices(provider, catalog)
          yield
            val voice = if enabled then assignThreadVoice(provider, threadId, voices) else None
            voice.foreach(_ => publishVoiceChanged(threadId))
            js.Dynamic.literal("provider" -> provider, "voice" -> voice.orNull)).toJSPromise
      }
    )
  )

  bb.events.on(
    "thread.idle",
    { payload =>
      val threadId = str(payload.thread.id)
      endWaitForMoreSpeech(threadId, "its turn ended")
      replyOnceIdle(threadId)
      ()
    }
  )

  /** Handles the end of a thread's turn for a reply the user is waiting for:
    * reads aloud what it wrote, or waits for the turn that answers them.
    */
  def replyOnceIdle(threadId: String): Unit =
      awaitingReplies.get(threadId).foreach { awaitedReply =>
        def finish(error: String): Unit =
          awaitingReplies.remove(threadId)
          publishSignal("agent-failed", awaitedReply.sessionId, threadId, "error" -> error)
        recentTimeline(threadId).onComplete {
          // Another check of the same turn's end got there first.
          case _ if !awaitingReplies.get(threadId).contains(awaitedReply) => ()
          case scala.util.Success(timeline) =>
            AssistantReply.afterIdle(
              awaitedReply.previousAssistantMessageId,
              assistantMessagesFromTimeline(timeline),
              latestUserMessageSeq(timeline)
            ) match
              case IdleOutcome.Reply(reply, latestUnanswered) =>
                if latestUnanswered then
                  // The reply is heard now; the user's later message is
                  // answered in a turn of its own, awaited from this reply on.
                  val next = awaitedReply.copy(previousAssistantMessageId = Some(reply.id))
                  awaitingReplies.put(threadId, next)
                  expectFollowUpTurn(threadId, next)
                else awaitingReplies.remove(threadId)
                publishSignal("speak", awaitedReply.sessionId, threadId, "text" -> reply.text)
              case IdleOutcome.Unanswered => expectFollowUpTurn(threadId, awaitedReply)
              case IdleOutcome.NoReply =>
                // The agent may end a turn silently on purpose, so this is
                // not reported as a failure.
                awaitingReplies.remove(threadId)
                publishSignal("turn-ended", awaitedReply.sessionId, threadId)
          case scala.util.Failure(error) =>
            bb.log.error(s"could not read the assistant reply for thread $threadId: ${error.getMessage}")
            finish("could not read the agent response")
        }
      }

  /** A message of the user's was left unanswered when the turn ended; the
    * agent answers it in a turn of its own, whose end is handled like any
    * other. If none has begun after a while, the agent has chosen not to.
    */
  def expectFollowUpTurn(threadId: String, awaited: AwaitingReply): Unit =
    delayMs(FollowUpTurnWithinMs).foreach { _ =>
      if awaitingReplies.get(threadId).contains(awaited) then
        bb.sdk.threads.get(js.Dynamic.literal("threadId" -> threadId)).toFuture.foreach { thread =>
          if str(thread.status) == "idle" && awaitingReplies.get(threadId).contains(awaited) then
            awaitingReplies.remove(threadId)
            publishSignal("turn-ended", awaited.sessionId, threadId)
        }
    }

  bb.events.on(
    "thread.failed",
    { payload =>
      val threadId = str(payload.thread.id)
      endWaitForMoreSpeech(threadId, "its turn failed")
      awaitingReplies.remove(threadId).foreach { awaitedReply =>
        val error = str(payload.error)
        publishSignal(
          "agent-failed",
          awaitedReply.sessionId,
          threadId,
          "error" -> (if error.isEmpty then "the agent failed" else error)
        )
      }
      ()
    }
  )

  bb.http.route(
    "GET",
    "/recent-utterance",
    { context =>
      utteranceAudio(str(context.req.query("id"))) match
        case None => jsonResponse(404, "no such utterance")
        case Some((mimeType, audio)) =>
          js.Dynamic.newInstance(js.Dynamic.global.Response)(
            audio,
            js.Dynamic.literal("status" -> 200, "headers" -> js.Dynamic.literal("content-type" -> mimeType))
          )
    }
  )

  bb.http.route(
    "POST",
    "/tts",
    { context =>
      val handled =
        for
          body <- context.req.json().asInstanceOf[js.Promise[js.Dynamic]].toFuture
          s    <- settings.get().toFuture
          requestedProvider = str(body.provider)
          provider = if requestedProvider.nonEmpty then requestedProvider else str(s.ttsProvider)
          config <- ttsConfig(provider)
          response <- {
            // The text comes with the agent's stress marks (*word*); a
            // provider that cannot stress a word gets it without them.
            val markedText = str(body.text)
            val text       = Speech.withoutStressMarks(markedText)
            if text.isEmpty then Future.successful(jsonResponse(400, "text is required"))
            else
              // Each provider owns its settings block; unused blocks persist
              // untouched, so switching providers is never destructive.
              val catalog        = availableVoices(provider, config)
              val requestedVoice = str(body.voice)
              val voice =
                if requestedVoice.nonEmpty && catalog.contains(requestedVoice) then requestedVoice
                else str(config.defaultVoice)
              val requestF: Future[Either[String, (String, js.Dynamic)]] = provider match
                case "pocket-tts" =>
                  ensureService(PocketService).map(_.map { url =>
                    val form = js.Dynamic.newInstance(js.Dynamic.global.FormData)()
                    val _    = form.set("text", text)
                    if voice.nonEmpty then { val _ = form.set("voice_url", voice) }
                    (url, js.Dynamic.literal("method" -> "POST", "body" -> form))
                  })
                case QwenTtsProvider =>
                  // The one provider that stresses a marked word, so it gets the marks.
                  ensureQwenTts(s).map(_.map { url =>
                    val payload = js.Dynamic.literal(
                      "text"  -> markedText,
                      "voice" -> (if voice.nonEmpty then voice else QwenServer.DefaultVoice)
                    )
                    (
                      url,
                      js.Dynamic.literal(
                        "method"  -> "POST",
                        "headers" -> js.Dynamic.literal("content-type" -> "application/json"),
                        "body"    -> js.JSON.stringify(payload)
                      )
                    )
                  })
                case ChatterboxNanoProvider =>
                  ensureService(ChatterboxService).map(_.map { url =>
                    val payload = js.Dynamic.literal(
                      "text"  -> text,
                      "voice" -> (if voice.nonEmpty then voice else ChatterboxServer.BuiltInVoice)
                    )
                    (
                      url,
                      js.Dynamic.literal(
                        "method"  -> "POST",
                        "headers" -> js.Dynamic.literal("content-type" -> "application/json"),
                        "body"    -> js.JSON.stringify(payload)
                      )
                    )
                  })
                case "kokoro" =>
                  Future.successful {
                    val payload = js.Dynamic.literal(
                      "model"           -> "kokoro",
                      "input"           -> text,
                      "response_format" -> "mp3"
                    )
                    if voice.nonEmpty then payload.updateDynamic("voice")(voice)
                    Right(
                      (
                        str(config.url),
                        js.Dynamic.literal(
                          "method"  -> "POST",
                          "headers" -> js.Dynamic.literal("content-type" -> "application/json"),
                          "body"    -> js.JSON.stringify(payload)
                        )
                      )
                    )
                  }
                case "openai-compatible" =>
                  Future.successful {
                    val url = str(config.url)
                    if url.isEmpty then Left("the OpenAI-compatible TTS endpoint is not configured")
                    else
                      val headers = js.Dynamic.literal("content-type" -> "application/json")
                      val apiKey  = str(config.apiKey)
                      if apiKey.nonEmpty then headers.updateDynamic("Authorization")(s"Bearer $apiKey")
                      val payload = js.Dynamic.literal("input" -> text, "response_format" -> "mp3")
                      val model   = str(config.model)
                      if model.nonEmpty then payload.updateDynamic("model")(model)
                      if voice.nonEmpty then payload.updateDynamic("voice")(voice)
                      Right(
                        (url, js.Dynamic.literal("method" -> "POST", "headers" -> headers, "body" -> js.JSON.stringify(payload)))
                      )
                  }
                case other =>
                  Future.successful(Left(s"ttsProvider is '$other'; the /tts route serves only server-side providers"))
              requestF.flatMap {
                case Left(message) => Future.successful(jsonResponse(400, message))
                case Right((url, init)) =>
                  js.Dynamic.global
                    .fetch(url, init)
                    .asInstanceOf[js.Promise[js.Dynamic]]
                    .toFuture
                    .map { upstream =>
                      if !upstream.ok.asInstanceOf[Boolean] then
                        jsonResponse(502, s"TTS provider returned ${str(upstream.status)}")
                      else
                        val contentType = upstream.headers.get("content-type")
                        js.Dynamic.newInstance(js.Dynamic.global.Response)(
                          upstream.body,
                          js.Dynamic.literal(
                            "status" -> 200,
                            "headers" -> js.Dynamic.literal(
                              "content-type" -> (if contentType == null then "audio/mpeg" else contentType)
                            )
                          )
                        )
                    }
              }
          }
        yield response
      handled.toJSPromise
    }
  )

  // Recordings stored before the last shutdown and not yet delivered.
  db.prepare("SELECT id, status, last_error FROM utterances WHERE status IN (?, ?) ORDER BY at")
    .all(RecordingStatus.Received.code, RecordingStatus.Sending.code)
    .foreach { row =>
      val id = str(row.id)
      recordingStatus(row).map(RecordingStatus.afterRestart) match
        case Some(RecordingStatus.Received) => enqueueRecording(id)
        case Some(other)                    => setRecordingStatus(id, other)
        case None                           => ()
    }

  // A page that was not reloaded along with this backend still holds a session
  // this backend has forgotten; this tells it to register again.
  bb.realtime.publish("converse", js.Dynamic.literal("type" -> "backend-started", "eventId" -> js.Date.now().toString))

  bb.onDispose { () =>
    bb.log.info(
      state.active.fold("plugin unloading with no voice session active")(a =>
        s"plugin unloading with voice session ${a.sessionId} active for thread ${a.threadId}"
      )
    )
    activate(None)
    state.continuationWaiters.expireAll()
    ()
  }
