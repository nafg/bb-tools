package converseroundtrip

import conversecore.Speech

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import java.util.UUID
import scala.io.Source

/** Checks how a reply comes out when spoken. Reads the reply's text from
  * standard input and splits it into the pieces the player speaks, exactly as
  * it sends them. Each piece is synthesized with Pocket TTS and the audio
  * transcribed back with Parakeet. Prints, for each piece, a "sent:" line and
  * a "heard:" line, then a blank line.
  *
  * Speech is synthesized through the converse plugin's own speech route at
  * BB_SERVER_URL, which starts Pocket TTS if it is not running. Parakeet must
  * be running at its usual local port; the plugin starts it for a voice
  * session.
  *
  * Options: `--voice <name>` (a Pocket voice; default alba), `--keep <dir>`
  * (also writes each piece's audio there, as 1.wav, 2.wav, ...).
  */
@main def speechRoundTrip(args: String*): Unit =
  val options = args.grouped(2).collect { case Seq(name, value) => name -> value }.toMap
  val voice   = options.getOrElse("--voice", "alba")
  val keep    = options.get("--keep").map(Path.of(_))
  keep.foreach(Files.createDirectories(_))
  val reply  = Source.stdin.mkString
  val titles = Speech.threadsReferredTo(reply).flatMap(id => threadTitle(id).map(id -> _)).toMap
  for (chunk, p) <- Speech.splitSpeechChunks(reply, titles).zipWithIndex do
    val audio = Services.synthesize(chunk.text, voice)
    keep.foreach(dir => Files.write(dir.resolve(s"${p + 1}.wav"), audio))
    println(s"sent:  ${chunk.text}")
    println(s"heard: ${Services.transcribe(audio)}")
    println()

/** A bb thread's title, as `bb thread get` gives it; None if it cannot be read. */
private def threadTitle(id: String): Option[String] =
  val process = new ProcessBuilder("bb", "thread", "get", id, "--json").redirectErrorStream(false).start()
  val output  = String(process.getInputStream.readAllBytes(), "UTF-8")
  if process.waitFor() != 0 then None
  else
    val thread = ujson.read(output)("thread")
    Seq("title", "titleFallback").flatMap(key => thread.obj.get(key).flatMap(_.strOpt)).find(_.nonEmpty)

private object Services:
  private val SpeechRoute = sys.env.getOrElse("BB_SERVER_URL", "http://127.0.0.1:38886") + "/api/v1/plugins/converse/http/tts"
  private val Parakeet    = "http://127.0.0.1:8790/v1/audio/transcriptions"
  private val client      = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()

  /** A WAV of `text` spoken by Pocket TTS in `voice`. */
  def synthesize(text: String, voice: String): Array[Byte] =
    val body = ujson.write(ujson.Obj("text" -> text, "voice" -> voice, "provider" -> "pocket-tts"))
    send(SpeechRoute, "application/json", body.getBytes("UTF-8"))

  def transcribe(wav: Array[Byte]): String =
    val boundary = UUID.randomUUID().toString
    val body =
      s"--$boundary\r\nContent-Disposition: form-data; name=\"file\"; filename=\"piece.wav\"\r\n\r\n".getBytes("UTF-8") ++
        wav ++ s"\r\n--$boundary--\r\n".getBytes("UTF-8")
    ujson.read(send(Parakeet, s"multipart/form-data; boundary=$boundary", body))("text").str

  private def send(url: String, contentType: String, body: Array[Byte]): Array[Byte] =
    val request = HttpRequest
      .newBuilder(URI.create(url))
      .header("content-type", contentType)
      .POST(HttpRequest.BodyPublishers.ofByteArray(body))
      .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofByteArray())
    if response.statusCode() != 200 then
      throw new RuntimeException(s"$url answered ${response.statusCode()}: ${String(response.body(), "UTF-8").take(300)}")
    response.body()
