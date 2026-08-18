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
      "ttsMode" -> js.Dynamic.literal(
        "type"    -> "select",
        "label"   -> "Speech synthesis",
        "options" -> js.Array("browser", "server"),
        "default" -> "browser"
      ),
      "ttsApi" -> js.Dynamic.literal(
        "type"    -> "select",
        "label"   -> "Server TTS API shape",
        "options" -> js.Array("openai-compatible", "pocket-tts"),
        "default" -> "openai-compatible"
      ),
      "ttsUrl" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "Server TTS endpoint, e.g. http://127.0.0.1:8880/v1/audio/speech (OpenAI-compatible) or http://127.0.0.1:8000/tts (Pocket)",
        "default" -> ""
      ),
      "ttsModel" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "TTS model (e.g. kokoro, gpt-4o-mini-tts)",
        "default" -> ""
      ),
      "ttsVoice" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "TTS voice (e.g. af_heart, alloy)",
        "default" -> ""
      ),
      "ttsApiKey" -> js.Dynamic.literal(
        "type"   -> "string",
        "label"  -> "TTS API key (sent as a Bearer token; leave empty for local services)",
        "secret" -> true
      ),
      "vadThreshold" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "Voice detection threshold (normalized RMS; raise if noise triggers it)",
        "default" -> "0.01"
      ),
      "sttUrl" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "OpenAI-compatible transcription endpoint (empty = use bb's voice transcription), e.g. http://127.0.0.1:2022/v1/audio/transcriptions",
        "default" -> ""
      ),
      "sttModel" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "Transcription model for the custom endpoint (e.g. base.en, whisper-large-v3)",
        "default" -> ""
      ),
      "sttApiKey" -> js.Dynamic.literal(
        "type"   -> "string",
        "label"  -> "Transcription API key (Bearer; empty for local services)",
        "secret" -> true
      ),
      "sttLanguage" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "Transcription language hint (ISO code like en; empty = auto-detect)",
        "default" -> "en"
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
      val sttUrl = str(s.sttUrl)
      if sttUrl.nonEmpty then
        transcribeViaEndpoint(sttUrl, str(s.sttModel), str(s.sttApiKey), str(s.sttLanguage), file)
      else
        transcribeViaBb(file).recoverWith {
          case e if retryUnavailable && e.getMessage != null && e.getMessage.contains("503") =>
            bb.log.warn(s"transcription unavailable; retrying once: ${e.getMessage}")
            delayMs(1500).flatMap(_ => transcribeViaBb(file))
        }
    }

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
            val url  = str(s.ttsUrl)
            if text.isEmpty then Future.successful(jsonResponse(400, "text is required"))
            else if url.isEmpty then
              Future.successful(jsonResponse(400, "ttsUrl is not configured in the Converse plugin settings"))
            else
              val voice = str(s.ttsVoice)
              val init =
                if str(s.ttsApi) == "pocket-tts" then
                  // Pocket TTS: multipart form { text, voice_url }, wav response.
                  val form = js.Dynamic.newInstance(js.Dynamic.global.FormData)()
                  val _    = form.set("text", text)
                  if voice.nonEmpty then { val _ = form.set("voice_url", voice) }
                  js.Dynamic.literal("method" -> "POST", "body" -> form)
                else
                  val headers = js.Dynamic.literal("content-type" -> "application/json")
                  val apiKey  = str(s.ttsApiKey)
                  if apiKey.nonEmpty then headers.updateDynamic("Authorization")(s"Bearer $apiKey")
                  val payload = js.Dynamic.literal("input" -> text, "response_format" -> "mp3")
                  val model   = str(s.ttsModel)
                  if model.nonEmpty then payload.updateDynamic("model")(model)
                  if voice.nonEmpty then payload.updateDynamic("voice")(voice)
                  js.Dynamic.literal("method" -> "POST", "headers" -> headers, "body" -> js.JSON.stringify(payload))
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
        yield response
      handled.toJSPromise
    }
  )

  bb.onDispose { () =>
    state.active = None
    state.awaiting.clear()
    ()
  }
