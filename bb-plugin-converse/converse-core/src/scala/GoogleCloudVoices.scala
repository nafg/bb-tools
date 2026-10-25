package conversecore

/** A voice of Google Cloud Text-to-Speech, named as its API names it
  * (`en-US-Wavenet-D`): locale, model, then a letter.
  */
final case class GoogleCloudVoice(name: String, gender: GoogleCloudVoice.Gender):
  /** The BCP-47 language code the voice is requested with, the start of its name. */
  def languageCode: String = name.split("-", 3).take(2).mkString("-")

  /** The voice as the voice picker shows it: "US English WaveNet D (male)". */
  def label: String =
    val parts  = name.split("-")
    val locale = GoogleCloudVoice.LocaleNames.getOrElse(parts.take(2).mkString("-"), parts.take(2).mkString("-"))
    val model  = GoogleCloudVoice.ModelNames.getOrElse(parts(2), parts(2))
    s"$locale $model ${parts.drop(3).mkString("-")} (${gender.toString.toLowerCase})"

object GoogleCloudVoice:
  enum Gender:
    case Male, Female

  private val LocaleNames = Map("en-US" -> "US English", "en-GB" -> "British English", "en-AU" -> "Australian English")
  private val ModelNames  = Map("Wavenet" -> "WaveNet")

/** The WaveNet voices the provider offers. WaveNet is the family that both
  * stresses a word written in `<emphasis>` (Chirp 3 HD takes no SSML, Studio
  * ignores the element) and has the 4 million characters a month free that
  * cover the plugin's use, which Neural2 (1 million) does not.
  */
object GoogleCloudVoices:
  import GoogleCloudVoice.Gender.*

  val English: Vector[GoogleCloudVoice] = Vector(
    GoogleCloudVoice("en-US-Wavenet-A", Male),
    GoogleCloudVoice("en-US-Wavenet-B", Male),
    GoogleCloudVoice("en-US-Wavenet-C", Female),
    GoogleCloudVoice("en-US-Wavenet-D", Male),
    GoogleCloudVoice("en-US-Wavenet-E", Female),
    GoogleCloudVoice("en-US-Wavenet-F", Female),
    GoogleCloudVoice("en-US-Wavenet-G", Female),
    GoogleCloudVoice("en-US-Wavenet-H", Female),
    GoogleCloudVoice("en-US-Wavenet-I", Male),
    GoogleCloudVoice("en-US-Wavenet-J", Male),
    GoogleCloudVoice("en-GB-Wavenet-A", Female),
    GoogleCloudVoice("en-GB-Wavenet-B", Male),
    GoogleCloudVoice("en-GB-Wavenet-C", Female),
    GoogleCloudVoice("en-GB-Wavenet-D", Male),
    GoogleCloudVoice("en-GB-Wavenet-F", Female),
    GoogleCloudVoice("en-GB-Wavenet-N", Female),
    GoogleCloudVoice("en-GB-Wavenet-O", Male),
    GoogleCloudVoice("en-AU-Wavenet-A", Female),
    GoogleCloudVoice("en-AU-Wavenet-B", Male),
    GoogleCloudVoice("en-AU-Wavenet-C", Female),
    GoogleCloudVoice("en-AU-Wavenet-D", Male)
  )

  /** The voice a thread gets when none is chosen: a natural male US voice. */
  val Default: GoogleCloudVoice = English.find(_.name == "en-US-Wavenet-D").get

  def byName(name: String): Option[GoogleCloudVoice] = English.find(_.name == name)
