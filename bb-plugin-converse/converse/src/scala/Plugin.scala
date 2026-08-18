package converse

import bbplugin.*

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

private final case class ActiveSession(sessionId: String, threadId: String)

/** Voice-conversation state for the whole server: at most one active browser
  * session, plus the threads whose next idle transition should be spoken.
  */
private final class State:
  var active: Option[ActiveSession]     = None
  val awaiting: mutable.Map[String, String] = mutable.Map.empty // threadId -> sessionId
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

  val settings = bb.settings.define(
    js.Dynamic.literal(
      "ttsProvider" -> js.Dynamic.literal(
        "type"    -> "select",
        "label"   -> "TEXT-TO-SPEECH PROVIDER",
        "options" -> js.Array("browser", "pocket-tts", "kokoro", "openai-compatible"),
        "default" -> "browser"
      ),
      "pocketUrl" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "POCKET TTS: external /tts endpoint (empty = managed locally by the plugin)",
        "default" -> ""
      ),
      "pocketVoice" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "POCKET TTS: voice",
        "default" -> "alba"
      ),
      "pocketPort" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "POCKET TTS: local port (managed mode)",
        "default" -> "8000"
      ),
      "pocketStartCommand" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "POCKET TTS: start command (managed mode; empty = do not manage)",
        "default" -> "uvx pocket-tts serve --host localhost --port 8000"
      ),
      "serviceIdleMinutes" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "SERVICES: stop managed services after this many idle minutes",
        "default" -> "15"
      ),
      "kokoroUrl" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "KOKORO: OpenAI-compatible speech endpoint",
        "default" -> "http://127.0.0.1:8880/v1/audio/speech"
      ),
      "kokoroVoice" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "KOKORO: voice",
        "default" -> "af_heart"
      ),
      "ttsCustomUrl" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "CUSTOM TTS: OpenAI-compatible speech endpoint (also covers Speaches, Piper, OpenAI)",
        "default" -> ""
      ),
      "ttsCustomModel" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "CUSTOM TTS: model",
        "default" -> ""
      ),
      "ttsCustomVoice" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "CUSTOM TTS: voice",
        "default" -> ""
      ),
      "ttsCustomApiKey" -> js.Dynamic.literal(
        "type"   -> "string",
        "label"  -> "CUSTOM TTS: API key (Bearer; empty for local services)",
        "secret" -> true
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
      "vadThreshold" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "MIC: voice detection threshold (normalized RMS; raise if noise triggers it)",
        "default" -> "0.01"
      )
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

  def audioFile(mimeType: String, audioBase64: String): js.Dynamic =
    val bytes = js.Dynamic.global.Buffer.from(audioBase64, "base64")
    // The transcription provider detects the container from the filename
    // extension; a bare Blob (filename "blob") is rejected as unavailable.
    val extension =
      if mimeType.startsWith("audio/webm") then "webm"
      else if mimeType.startsWith("audio/mp4") then "mp4"
      else if mimeType.startsWith("audio/ogg") then "ogg"
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
      file: js.Dynamic
  ): Future[String] =
    val form = js.Dynamic.newInstance(js.Dynamic.global.FormData)()
    val _    = form.set("file", file)
    if model.nonEmpty then { val _ = form.set("model", model) }
    if language.nonEmpty then { val _ = form.set("language", language) }
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

  def transcribeAudio(mimeType: String, audioBase64: String, retryUnavailable: Boolean): Future[String] =
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
            file
          )
        case "openai-compatible" =>
          val url = str(s.sttCustomUrl)
          if url.isEmpty then Future.failed(new RuntimeException("sttCustomUrl is not configured"))
          else transcribeViaEndpoint(url, str(s.sttCustomModel), str(s.sttCustomApiKey), language, file)
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

  /** Resolve the Pocket TTS base URL, starting the managed service into a bb
    * terminal when it is not running. External pocketUrl disables management.
    */
  def ensurePocket(s: js.Dynamic): Future[Either[String, String]] =
    val external = str(s.pocketUrl)
    if external.nonEmpty then Future.successful(Right(external))
    else
      val port = str(s.pocketPort) match
        case "" => "8000"
        case p  => p
      val url = s"http://127.0.0.1:$port/tts"
      probeService(url).flatMap {
        case true =>
          touchPocketUse()
          Future.successful(Right(url))
        case false =>
          val command = str(s.pocketStartCommand)
          if command.isEmpty then
            Future.successful(Left("Pocket TTS is not running and no start command is configured"))
          else if pocketStarting != null then pocketStarting
          else
            bb.log.info(s"starting managed Pocket TTS: $command")
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
                          "start" -> js.Dynamic.literal("mode" -> "command", "command" -> command)
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
        s          <- settings.get().toFuture
        terminalId <- bb.storage.kv.get("pocket.terminalId").toFuture
        lastUse    <- bb.storage.kv.get("pocket.lastUseAt").toFuture
        _ <- {
          val idleMinutes = str(s.serviceIdleMinutes).toIntOption.filter(_ > 0).getOrElse(15)
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
        state.awaiting.filterInPlace((_, sess) => sess == sessionId)
        bb.log.info(s"voice session $sessionId started for thread $threadId")
        // Warm the managed TTS service while the user starts talking so the
        // first spoken reply is not delayed by service startup.
        settings.get().toFuture.foreach { s =>
          if str(s.ttsProvider) == "pocket-tts" then
            ensurePocket(s).foreach {
              case Left(message) => bb.log.warn(s"TTS warm-up: $message")
              case Right(_)      => ()
            }
        }
        js.Dynamic.literal("ok" -> true)
      },
      "stopSession" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        if state.active.exists(_.sessionId == sessionId) then state.active = None
        state.awaiting.filterInPlace((_, sess) => sess != sessionId)
        js.Dynamic.literal("ok" -> true)
      },
      "submitUtterance" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        val threadId  = str(input.threadId)
        if !state.active.exists(_.sessionId == sessionId) then rejected("voice session is no longer active")
        else
          bb.log.info(s"utterance received: ${str(input.audioBase64).length} b64 chars, ${str(input.mimeType)}")
          transcribeAudio(str(input.mimeType), str(input.audioBase64), retryUnavailable = true)
            .flatMap { text =>
              bb.log.info(s"transcribed ${text.length} chars")
              if text.isEmpty then Future.successful(js.Dynamic.literal("text" -> "", "sent" -> false))
              else
                state.awaiting.update(threadId, sessionId)
                bb.sdk.threads
                  .send(
                    js.Dynamic.literal(
                      "threadId" -> threadId,
                      "mode"     -> "steer-if-active",
                      "input" -> js.Array(
                        js.Dynamic.literal("type" -> "text", "text" -> text),
                        js.Dynamic.literal(
                          "type" -> "text",
                          "text" ->
                            ("(The message above is a voice transcription, not typed text: " +
                              "expect filler words, homophone and punctuation errors, and spoken phrasing.)"),
                          "visibility" -> "agent-only"
                        )
                      )
                    )
                  )
                  .toFuture
                  .map(_ => js.Dynamic.literal("text" -> text, "sent" -> true))
                  .recoverWith { case e =>
                    val _ = state.awaiting.remove(threadId)
                    Future.failed(e)
                  }
            }
            .toJSPromise
      },
      "previewUtterance" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
        val sessionId = str(input.sessionId)
        if !state.active.exists(_.sessionId == sessionId) then rejected("voice session is no longer active")
        else
          transcribeAudio(str(input.mimeType), str(input.audioBase64), retryUnavailable = false)
            .map(text => js.Dynamic.literal("text" -> text))
            .toJSPromise
      }
    )
  )

  bb.events.on(
    "thread.idle",
    { payload =>
      val threadId = str(payload.thread.id)
      state.awaiting.remove(threadId).foreach { sessionId =>
        val text = payload.lastAssistantText.asInstanceOf[String | Null]
        publishSignal("speak", sessionId, threadId, "text" -> (if text == null then "" else text))
      }
      ()
    }
  )

  bb.events.on(
    "thread.failed",
    { payload =>
      val threadId = str(payload.thread.id)
      state.awaiting.remove(threadId).foreach { sessionId =>
        val error = str(payload.error)
        publishSignal(
          "agent-failed",
          sessionId,
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
          response <- {
            val text = str(body.text)
            if text.isEmpty then Future.successful(jsonResponse(400, "text is required"))
            else
              // Each provider owns its settings block; unused blocks persist
              // untouched, so switching providers is never destructive.
              val requestF: Future[Either[String, (String, js.Dynamic)]] = str(s.ttsProvider) match
                case "pocket-tts" =>
                  ensurePocket(s).map(_.map { url =>
                    touchPocketUse()
                    val form = js.Dynamic.newInstance(js.Dynamic.global.FormData)()
                    val _    = form.set("text", text)
                    val voice = str(s.pocketVoice)
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
                    val voice = str(s.kokoroVoice)
                    if voice.nonEmpty then payload.updateDynamic("voice")(voice)
                    Right(
                      (
                        str(s.kokoroUrl),
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
                    val url = str(s.ttsCustomUrl)
                    if url.isEmpty then Left("ttsCustomUrl is not configured in the Converse plugin settings")
                    else
                      val headers = js.Dynamic.literal("content-type" -> "application/json")
                      val apiKey  = str(s.ttsCustomApiKey)
                      if apiKey.nonEmpty then headers.updateDynamic("Authorization")(s"Bearer $apiKey")
                      val payload = js.Dynamic.literal("input" -> text, "response_format" -> "mp3")
                      val model   = str(s.ttsCustomModel)
                      if model.nonEmpty then payload.updateDynamic("model")(model)
                      val voice = str(s.ttsCustomVoice)
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
    ()
  }
