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
  * Pocket TTS and Parakeet must be running at their usual local ports; the
  * converse plugin starts them when a voice session needs them.
  *
  * Options: `--voice <name>` (a Pocket voice; default alba), `--keep <dir>`
  * (also writes each piece's audio there, as 1.wav, 2.wav, ...).
  */
@main def speechRoundTrip(args: String*): Unit =
  val options = args.grouped(2).collect { case Seq(name, value) => name -> value }.toMap
  val voice   = options.getOrElse("--voice", "alba")
  val keep    = options.get("--keep").map(Path.of(_))
  keep.foreach(Files.createDirectories(_))
  val reply = Source.stdin.mkString
  for (chunk, p) <- Speech.splitSpeechChunks(reply).zipWithIndex do
    val audio = Services.synthesize(chunk.text, voice)
    keep.foreach(dir => Files.write(dir.resolve(s"${p + 1}.wav"), audio))
    println(s"sent:  ${chunk.text}")
    println(s"heard: ${Services.transcribe(audio)}")
    println()

private object Services:
  private val PocketTts = "http://127.0.0.1:8000/tts"
  private val Parakeet  = "http://127.0.0.1:8790/v1/audio/transcriptions"
  private val client    = HttpClient.newHttpClient()

  /** A WAV of `text` spoken in `voice`. */
  def synthesize(text: String, voice: String): Array[Byte] =
    post(PocketTts, Seq(Part.Field("text", text), Part.Field("voice_url", voice)))

  def transcribe(wav: Array[Byte]): String =
    ujson.read(post(Parakeet, Seq(Part.File("file", "piece.wav", wav))))("text").str

  private enum Part:
    case Field(name: String, value: String)
    case File(name: String, fileName: String, bytes: Array[Byte])

  private def post(url: String, parts: Seq[Part]): Array[Byte] =
    val boundary = UUID.randomUUID().toString
    val body = parts.flatMap {
      case Part.Field(name, value) =>
        s"--$boundary\r\nContent-Disposition: form-data; name=\"$name\"\r\n\r\n$value\r\n".getBytes("UTF-8").toSeq
      case Part.File(name, fileName, bytes) =>
        s"--$boundary\r\nContent-Disposition: form-data; name=\"$name\"; filename=\"$fileName\"\r\n\r\n"
          .getBytes("UTF-8")
          .toSeq ++ bytes ++ "\r\n".getBytes("UTF-8")
    } ++ s"--$boundary--\r\n".getBytes("UTF-8")
    val request = HttpRequest
      .newBuilder(URI.create(url))
      .header("content-type", s"multipart/form-data; boundary=$boundary")
      .POST(HttpRequest.BodyPublishers.ofByteArray(body.toArray))
      .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofByteArray())
    if response.statusCode() != 200 then
      throw new RuntimeException(s"$url answered ${response.statusCode()}: ${String(response.body(), "UTF-8").take(300)}")
    response.body()
