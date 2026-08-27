package converse

import bbplugin.*
import conversecore.{AssistantMessage, AssistantReply, ContinuationWaiters, ThreadVoiceAssignments}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

private def str(value: js.Dynamic): String =
  value.asInstanceOf[js.UndefOr[Any]].fold("")(v => if v == null then "" else v.toString)

/** Longest accepted base64 audio field (~5 MiB decoded): far above a minute of
  * browser-compressed speech, far below transcribeVoice's 25 MiB cap.
  */
private val MaxAudioBase64Chars = 7 * 1024 * 1024

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

private val ManagedPocketPort         = 8000
private val ManagedPocketStartCommand = "uvx pocket-tts serve --host localhost --port 8000"
private val ManagedPocketIdleMinutes  = 15

private def ttsConfigKey(provider: String): String = s"tts.config.$provider"

private final case class ActiveSession(sessionId: String, threadId: String)
private final case class AwaitingReply(sessionId: String, previousAssistantMessageId: Option[String])

/** Voice-conversation state for the whole server: at most one active browser
  * session, plus the threads whose next idle transition should be spoken.
  */
private final class State:
  var active: Option[ActiveSession]     = None
  val awaiting: mutable.Map[String, AwaitingReply] = mutable.Map.empty // threadId -> pending voice reply
  val continuationWaiters = ContinuationWaiters()
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
  def strings(fields: (String, Int)*): js.Any =
    make(js.Any.fromFunction1 { value =>
      if value == null || js.typeOf(value) != "object" then invalid("expected an object")
      else
        val d = value.asInstanceOf[js.Dynamic]
        fields
          .collectFirst {
            case (name, maxLength)
                if {
                  val v = d.selectDynamic(name)
                  js.typeOf(v) != "string" ||
                  v.asInstanceOf[String].isEmpty ||
                  v.asInstanceOf[String].length > maxLength
                } =>
              invalid(s"$name must be a non-empty string of at most $maxLength characters")
          }
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
        |SELECT 'pocket-tts', thread_id, voice FROM thread_voice_assignments""".stripMargin
    )
  )

  val settings = bb.settings.define(
    js.Dynamic.literal(
      "ttsProvider" -> js.Dynamic.literal(
        "type"    -> "select",
        "label"   -> "TEXT-TO-SPEECH PROVIDER",
        "options" -> js.Array("browser", "pocket-tts", "kokoro", "openai-compatible"),
        "default" -> "browser"
      ),
      "sttProvider" -> js.Dynamic.literal(
        "type"    -> "select",
        "label"   -> "SPEECH-TO-TEXT PROVIDER",
        "options" -> js.Array("bb", "groq", "openai-compatible"),
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


  def stringArray(value: js.Dynamic): Vector[String] =
    value.asInstanceOf[js.UndefOr[js.Array[String]]].fold(Vector.empty)(_.toVector)
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinct

  def defaultTtsConfig(provider: String): js.Dynamic = provider match
    case "pocket-tts" => js.Dynamic.literal("defaultVoice" -> "alba")
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

  def waitForMoreSpeech(threadId: String): Future[String] =
    if !state.awaiting.contains(threadId) then
      Future.successful(
        "No active voice turn is awaiting a response. Continue normally without waiting for speech."
      )
    else
      settings.get().toFuture.flatMap { currentSettings =>
        val waitMs = str(currentSettings.continuationWaitMs).toDoubleOption
          .filter(ms => ms >= 1000 && ms <= 30000)
          .getOrElse(8000.0)
        val waiting = state.continuationWaiters.begin(threadId)
        val timer = js.Dynamic.global.setTimeout(
          js.Any.fromFunction0(() => { val _ = state.continuationWaiters.expire(threadId) }),
          waitMs
        )
        waiting.map { continuation =>
          val _ = js.Dynamic.global.clearTimeout(timer)
          continuation match
            case Some(text) =>
              ("The user continued speaking. Treat this as part of the preceding voice message:\n" +
                "<voice-continuation>\n" + text + "\n</voice-continuation>")
            case None =>
              s"No additional speech arrived within ${waitMs.toInt} milliseconds. " +
                "Treat the preceding voice message as complete and continue."
        }
      }

  bb.agents.registerTool(
    js.Dynamic.literal(
      "name" -> "converse_wait_for_more_speech",
      "description" ->
        ("Wait briefly for the user to continue the current voice-transcribed message. " +
          "Call this only when whether to act depends on whether more speech immediately follows."),
      "instructions" ->
        ("Voice messages are enclosed in agent-only [heard] tags and introduced with Transcribed:. " +
          "Treat their contents as speech recognition that may contain filler words, homophones, " +
          "punctuation errors, or spoken phrasing. " +
          "If the latest voice message appears unfinished or genuinely ambiguous, call " +
          "converse_wait_for_more_speech before replying or taking action. Do not silently stop and " +
          "wait without the tool: it has a bounded timeout and tells you whether speech arrived. " +
          "If the native tool is not available in this provider instance, run `bb converse wait` " +
          "instead; it invokes the same bounded waiter."),
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
          "summary" -> "Wait briefly for continued speech in the current thread",
          "usage"   -> "bb converse wait"
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
          case _ =>
            Future.successful(
              js.Dynamic.literal(
                "exitCode" -> 1,
                "stderr"   -> "usage: bb converse wait"
              )
            )
        result.toJSPromise
      }
    )
  )

  def publishSignal(signalType: String, sessionId: String, threadId: String, extra: (String, js.Any)*): Unit =
    val payload = js.Dynamic.literal(
      "type"      -> signalType,
      "sessionId" -> sessionId,
      "threadId"  -> threadId,
      "eventId"   -> state.nextEventId()
    )
    extra.foreach((name, value) => payload.updateDynamic(name)(value))
    bb.realtime.publish("converse", payload)

  def assistantMessagesFromTimeline(timeline: js.Dynamic): Vector[AssistantMessage] =
    def array(value: js.Dynamic): Vector[js.Dynamic] =
      if value == null || js.isUndefined(value) || !js.Array.isArray(value) then Vector.empty
      else value.asInstanceOf[js.Array[js.Dynamic]].toVector

    def messages(row: js.Dynamic): Vector[AssistantMessage] =
      val current =
        if str(row.kind) == "conversation" && str(row.role) == "assistant" then
          val id = str(row.id)
          if id.isEmpty then Vector.empty
          else
            Vector(
              AssistantMessage(
                id,
                str(row.text),
                str(row.sourceSeqEnd).toDoubleOption.fold(0L)(_.toLong)
              )
            )
        else Vector.empty
      current ++ array(row.children).flatMap(messages)

    array(timeline.rows).flatMap(messages)

  def latestAssistantMessage(threadId: String): Future[Option[AssistantMessage]] =
    bb.sdk.threads
      .timeline(
        js.Dynamic.literal(
          "threadId"         -> threadId,
          "includeNestedRows" -> "true",
          "segmentLimit"     -> "50"
        )
      )
      .toFuture
      .map(timeline => AssistantReply.freshSince(None, assistantMessagesFromTimeline(timeline)))

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

  def transcribeViaBb(file: js.Dynamic): Future[String] =
    bb.sdk.system
      .transcribeVoice(js.Dynamic.literal("file" -> file))
      .toFuture
      .map(result => str(result.text).trim)

  def transcribeViaEndpoint(
      url: String,
      model: String,
      apiKey: String,
      language: String,
      file: js.Dynamic,
      precedingTranscript: String,
      diagnosticContext: String
  ): Future[String] =
    val form = js.Dynamic.newInstance(js.Dynamic.global.FormData)()
    val _    = form.set("file", file)
    if model.nonEmpty then { val _ = form.set("model", model) }
    if language.nonEmpty then { val _ = form.set("language", language) }
    if precedingTranscript.nonEmpty then { val _ = form.set("prompt", precedingTranscript) }
    val _    = form.set("response_format", "verbose_json")
    val init = js.Dynamic.literal("method" -> "POST", "body" -> form)
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
            Future.failed(new RuntimeException(s"transcription endpoint returned ${str(response.status)}: $detail"))
          }
        else response.json().asInstanceOf[js.Promise[js.Dynamic]].toFuture
      }
      .map { payload =>
        // Whisper hallucinates stock phrases on silence/noise. Apply its own
        // reference heuristic per segment: high no-speech probability combined
        // with low average log-probability means the segment is not speech.
        val segments = payload.segments.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]]
        segments.fold(str(payload.text).trim) { segs =>
          bb.log.info(
            s"transcription segment evidence ($diagnosticContext): " + segs.zipWithIndex.map { (seg, index) =>
              val start    = seg.start.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
              val end      = seg.end.asInstanceOf[js.UndefOr[Double]].getOrElse(start)
              val noSpeech = seg.no_speech_prob.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
              val logProb  = seg.avg_logprob.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
              val chars    = str(seg.text).trim.length
              f"$index%d:${end - start}%.2fs,noSpeech=$noSpeech%.3f,avgLogProb=$logProb%.3f,chars=$chars%d"
            }.mkString("[", "; ", "]")
          )
          segs
            .filterNot { seg =>
              val noSpeech = seg.no_speech_prob.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
              val logProb  = seg.avg_logprob.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
              noSpeech > 0.6 && logProb < -1.0
            }
            .map(seg => str(seg.text).trim)
            .filter(_.nonEmpty)
            .mkString(" ")
            .trim
        }
      }

  def transcribeAudio(
      mimeType: String,
      audioBase64: String,
      retryUnavailable: Boolean,
      precedingTranscript: String,
      diagnosticContext: String
  ): Future[String] =
    val file = audioFile(mimeType, audioBase64)
    settings.get().toFuture.flatMap { s =>
      val language = str(s.sttLanguage)
      str(s.sttProvider) match
        case "groq" =>
          transcribeViaEndpoint(
            "https://api.groq.com/openai/v1/audio/transcriptions",
            str(s.groqModel),
            str(s.groqApiKey),
            language,
            file,
            precedingTranscript,
            diagnosticContext
          )
        case "openai-compatible" =>
          val url = str(s.sttCustomUrl)
          if url.isEmpty then Future.failed(new RuntimeException("sttCustomUrl is not configured"))
          else
            transcribeViaEndpoint(
              url,
              str(s.sttCustomModel),
              str(s.sttCustomApiKey),
              language,
              file,
              precedingTranscript,
              diagnosticContext
            )
        case _ =>
          transcribeViaBb(file).recoverWith {
            case e if retryUnavailable && e.getMessage != null && e.getMessage.contains("503") =>
              bb.log.warn(s"transcription unavailable; retrying once: ${e.getMessage}")
              delayMs(1500).flatMap(_ => transcribeViaBb(file))
          }
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

  def touchPocketUse(): Unit =
    bb.storage.kv.set("pocket.lastUseAt", js.Date.now()).toFuture.failed.foreach(_ => ())

  var pocketStarting: Future[Either[String, String]] = null

  /** Resolve Pocket TTS, always managing its local lifecycle. */
  def ensurePocket(): Future[Either[String, String]] =
    val url = s"http://127.0.0.1:$ManagedPocketPort/tts"
    probeService(url).flatMap {
        case true =>
          touchPocketUse()
          Future.successful(Right(url))
        case false =>
          if pocketStarting != null then pocketStarting
          else
            bb.log.info(s"starting managed Pocket TTS: $ManagedPocketStartCommand")
            state.active.foreach { a =>
              publishSignal("status", a.sessionId, a.threadId, "text" -> "Starting Pocket TTS…")
            }
            val started =
              connectedHostId()
                .flatMap {
                  case None => Future.successful(Left("no connected host to start Pocket TTS on"))
                  case Some(hostId) =>
                    val home = str(js.Dynamic.global.process.env.HOME) match
                      case "" => "/tmp"
                      case h  => h
                    bb.sdk.terminals
                      .create(
                        js.Dynamic.literal(
                          "cols"  -> 120,
                          "rows"  -> 30,
                          "scope" -> js.Dynamic.literal("kind" -> "host_path", "hostId" -> hostId, "cwd" -> home),
                          "title" -> "pocket-tts (converse)",
                          "start" -> js.Dynamic.literal("mode" -> "command", "command" -> ManagedPocketStartCommand)
                        )
                      )
                      .toFuture
                      .flatMap { result =>
                        val terminal =
                          result.terminal.asInstanceOf[js.UndefOr[js.Dynamic]].getOrElse(result)
                        val terminalId = str(terminal.id)
                        bb.storage.kv.set("pocket.terminalId", terminalId).toFuture.map(_ => terminalId)
                      }
                      .flatMap { terminalId =>
                        def awaitReady(attempt: Int): Future[Either[String, String]] =
                          probeService(url).flatMap {
                            case true =>
                              bb.log.info(s"Pocket TTS is up (terminal $terminalId)")
                              touchPocketUse()
                              Future.successful(Right(url))
                            case false if attempt >= 90 =>
                              Future.successful(Left("Pocket TTS did not become ready within 90s"))
                            case false => delayMs(1000).flatMap(_ => awaitReady(attempt + 1))
                          }
                        awaitReady(0)
                      }
                }
                .recover { case e => Left(s"could not start Pocket TTS: ${e.getMessage}") }
            pocketStarting = started
            started.onComplete(_ => pocketStarting = null)
            started
    }

  bb.background.schedule(
    "service-idle",
    "*/5 * * * *",
    () =>
      (for
        terminalId <- bb.storage.kv.get("pocket.terminalId").toFuture
        lastUse    <- bb.storage.kv.get("pocket.lastUseAt").toFuture
        _ <- {
          val idleMinutes = ManagedPocketIdleMinutes
          val tid         = str(terminalId)
          val last        = lastUse.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
          if tid.nonEmpty && last > 0 && js.Date.now() - last > idleMinutes * 60_000.0 then
            bb.log.info(s"stopping managed Pocket TTS after $idleMinutes idle minutes")
            for
              _ <- bb.sdk.terminals.close(js.Dynamic.literal("terminalId" -> tid)).toFuture.recover(_ => ())
              _ <- bb.storage.kv.delete("pocket.terminalId").toFuture
              _ <- bb.storage.kv.delete("pocket.lastUseAt").toFuture
            yield ()
          else Future.unit
        }
      yield ()).toJSPromise
  )

  bb.rpc.register(
    js.Dynamic.literal(
      "startSession" -> js.Dynamic.literal(
        "input"  -> Schema.strings("sessionId" -> 100, "threadId" -> 100),
        "output" -> Schema.passthrough
      ),
      "stopSession" -> js.Dynamic.literal(
        "input"  -> Schema.strings("sessionId" -> 100),
        "output" -> Schema.passthrough
      ),
      "submitUtterance" -> js.Dynamic.literal(
        "input" -> Schema.strings(
          "sessionId"   -> 100,
          "threadId"    -> 100,
          "mimeType"    -> 100,
          "audioBase64" -> MaxAudioBase64Chars
        ),
        "output" -> Schema.passthrough
      ),
      "previewUtterance" -> js.Dynamic.literal(
        "input" -> Schema.strings(
          "sessionId"   -> 100,
          "mimeType"    -> 100,
          "audioBase64" -> MaxAudioBase64Chars
        ),
        "output" -> Schema.passthrough
      ),
      "reportDiscardedCapture" -> js.Dynamic.literal(
        "input"  -> Schema.strings("sessionId" -> 100, "threadId" -> 100),
        "output" -> Schema.passthrough
      ),
      "voiceSettings" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "updateVoiceSettings" -> js.Dynamic.literal(
        "input" -> Schema.passthrough,
        "output" -> Schema.passthrough
      ),
      "threadVoiceSettings" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
      "updateThreadVoiceSettings" -> js.Dynamic.literal(
        "input" -> Schema.passthrough,
        "output" -> Schema.passthrough
      ),
      "threadVoice" -> js.Dynamic.literal("input" -> Schema.passthrough, "output" -> Schema.passthrough),
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
        state.active = Some(ActiveSession(sessionId, threadId))
        state.awaiting.filterInPlace((_, reply) => reply.sessionId == sessionId)
        state.continuationWaiters.expireAll()
        bb.log.info(s"voice session $sessionId started for thread $threadId")
        // Warm the managed TTS service while the user starts talking so the
        // first spoken reply is not delayed by service startup.
        settings.get().toFuture.foreach { s =>
          if str(s.ttsProvider) == "pocket-tts" then
            ensurePocket().foreach {
              case Left(message) => bb.log.warn(s"TTS warm-up: $message")
              case Right(_)      => ()
            }
        }
        js.Dynamic.literal("ok" -> true)
      },
      "stopSession" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        if state.active.exists(_.sessionId == sessionId) then state.active = None
        state.awaiting.filterInPlace((_, reply) => reply.sessionId != sessionId)
        state.continuationWaiters.expireAll()
        js.Dynamic.literal("ok" -> true)
      },
      "submitUtterance" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        val threadId  = str(input.threadId)
        if !state.active.exists(_.sessionId == sessionId) then rejected("voice session is no longer active")
        else
          val diagnosticContext = s"final thread=$threadId request=${state.nextEventId()}"
          val evidence = input.evidence.asInstanceOf[js.UndefOr[js.Dynamic]]
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
          transcribeAudio(
            str(input.mimeType),
            str(input.audioBase64),
            retryUnavailable = true,
            precedingTranscript = "",
            diagnosticContext = diagnosticContext
          )
            .flatMap { text =>
              bb.log.info(s"transcribed ($diagnosticContext) ${text.length} chars")
              if text.isEmpty then Future.successful(js.Dynamic.literal("text" -> "", "sent" -> false))
              else if state.continuationWaiters.deliver(threadId, text) then
                bb.log.info(s"delivered ${text.length} chars to continuation waiter")
                Future.successful(js.Dynamic.literal("text" -> text, "sent" -> true, "continued" -> true))
              else
                def send(mode: String): Future[js.Dynamic] =
                  bb.sdk.threads
                    .send(
                      js.Dynamic.literal(
                        "threadId" -> threadId,
                        "mode"     -> mode,
                        "input" -> js.Array(
                          js.Dynamic.literal(
                            "type"       -> "text",
                            "text"       -> "[heard]\n",
                            "visibility" -> "agent-only"
                          ),
                          js.Dynamic.literal("type" -> "text", "text" -> text),
                          js.Dynamic.literal(
                            "type"       -> "text",
                            "text"       -> "[/heard]",
                            "visibility" -> "agent-only"
                          )
                        )
                      )
                    )
                    .toFuture
                latestAssistantMessage(threadId).flatMap { previousAssistantMessage =>
                  state.awaiting.update(
                    threadId,
                    AwaitingReply(sessionId, previousAssistantMessage.map(_.id))
                  )
                  send("steer-if-active")
                    .recoverWith {
                      // steer-if-active 409s ("Thread is not active") when the
                      // thread is starting or errored; auto resolves those to a
                      // plain start, so the utterance is not dropped.
                      case e if e.getMessage != null && e.getMessage.contains("Thread is not active") =>
                        send("auto")
                    }
                    .map(_ => js.Dynamic.literal("text" -> text, "sent" -> true))
                    .recoverWith { case e =>
                      val _ = state.awaiting.remove(threadId)
                      Future.failed(e)
                    }
                }
            }
            .toJSPromise
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
        if !state.active.exists(_.sessionId == sessionId) then rejected("voice session is no longer active")
        else
          val diagnosticContext = s"preview thread=${state.active.map(_.threadId).getOrElse("unknown")} request=${state.nextEventId()}"
          transcribeAudio(
            str(input.mimeType),
            str(input.audioBase64),
            retryUnavailable = false,
            precedingTranscript = str(input.precedingTranscript),
            diagnosticContext = diagnosticContext
          )
            .map(text => js.Dynamic.literal("text" -> text))
            .toJSPromise
      },
      "voiceSettings" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val requestedProvider = str(input.provider)
        val providerF =
          if requestedProvider.nonEmpty then Future.successful(requestedProvider)
          else settings.get().toFuture.map(s => str(s.ttsProvider))
        providerF.flatMap(voiceSettingsResponse).toJSPromise
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
        if !Set("browser", "pocket-tts", "kokoro", "openai-compatible").contains(provider) then
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
      "threadVoice" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val threadId = str(input.threadId)
        if threadId.isEmpty then rejected("threadId is required")
        else
          settings.get().toFuture.flatMap { currentSettings =>
            val provider = str(currentSettings.ttsProvider)
            voiceSettingsResponse(provider).map { voiceSettings =>
              js.Dynamic.literal(
                "enabled"  -> voiceSettings.enabled,
                "provider" -> provider,
                "voice"    -> storedThreadVoice(provider, threadId).orNull,
                "voices"   -> voiceSettings.voices
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
      state.awaiting.remove(threadId).foreach { awaitedReply =>
        latestAssistantMessage(threadId).onComplete {
          case scala.util.Success(currentAssistantMessage) =>
            AssistantReply.freshSince(
              awaitedReply.previousAssistantMessageId,
              currentAssistantMessage
            ) match
              case Some(reply) => publishSignal("speak", awaitedReply.sessionId, threadId, "text" -> reply.text)
              case None =>
                publishSignal(
                  "agent-failed",
                  awaitedReply.sessionId,
                  threadId,
                  "error" -> "the agent finished without a response"
                )
          case scala.util.Failure(error) =>
            bb.log.error(s"could not read the assistant reply for thread $threadId: ${error.getMessage}")
            publishSignal(
              "agent-failed",
              awaitedReply.sessionId,
              threadId,
              "error" -> "could not read the agent response"
            )
        }
      }
      ()
    }
  )

  bb.events.on(
    "thread.failed",
    { payload =>
      val threadId = str(payload.thread.id)
      state.awaiting.remove(threadId).foreach { awaitedReply =>
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
            val text = str(body.text)
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
                  ensurePocket().map(_.map { url =>
                    touchPocketUse()
                    val form = js.Dynamic.newInstance(js.Dynamic.global.FormData)()
                    val _    = form.set("text", text)
                    if voice.nonEmpty then { val _ = form.set("voice_url", voice) }
                    (url, js.Dynamic.literal("method" -> "POST", "body" -> form))
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

  bb.onDispose { () =>
    state.active = None
    state.awaiting.clear()
    state.continuationWaiters.expireAll()
    ()
  }
