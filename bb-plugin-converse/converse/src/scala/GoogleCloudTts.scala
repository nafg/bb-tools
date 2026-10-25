package converse

import conversecore.{CharacterTally, GoogleCloudVoice, GoogleCloudVoices, Ssml}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.util.Try

/** Google Cloud Text-to-Speech over its REST API, a hosted speech provider.
  * Each request is one speech chunk as SSML, so a word the agent marked is
  * stressed with `<emphasis>`; the audio comes back whole, as MP3, in a JSON
  * field. WaveNet voices are offered: see [[GoogleCloudVoices]] for why.
  *
  * Authentication is gcloud's Application Default Credentials, the refresh
  * token `gcloud auth application-default login` wrote, exchanged for an
  * access token here and kept until shortly before it expires; the project
  * whose quota and billing the calls use is the one configured, else the one
  * recorded with those credentials. An API key restricted to the API works
  * in their place.
  */
private object GoogleCloudTts:
  val Provider = "google-cloud-tts"

  val SynthesizeUrl = "https://texttospeech.googleapis.com/v1/text:synthesize"
  val TokenUrl      = "https://oauth2.googleapis.com/token"

  /** How an authenticated call to the API is made. */
  enum Auth:
    /** An OAuth access token, with the project to bill (the API refuses user
      * credentials without one).
      */
    case Bearer(accessToken: String, quotaProject: Option[String])
    case ApiKey(key: String)

    def url(base: String): String = this match
      case Bearer(_, _) => base
      case ApiKey(key)  => s"$base?key=${js.URIUtils.encodeURIComponent(key)}"

    def headers: js.Dynamic =
      val h = js.Dynamic.literal("content-type" -> "application/json")
      this match
        case Bearer(token, project) =>
          h.updateDynamic("authorization")(s"Bearer $token")
          project.foreach(p => h.updateDynamic("x-goog-user-project")(p))
        case ApiKey(_) => ()
      h

  /** The request body speaking `ssml` with `voice`. The audio is MP3: a chunk
    * arrives whole anyway, and it is a tenth of WAV's size, which matters when
    * the browser is remote.
    */
  def synthesizeRequest(ssml: String, voice: GoogleCloudVoice): js.Dynamic =
    js.Dynamic.literal(
      "input"       -> js.Dynamic.literal("ssml" -> ssml),
      "voice"       -> js.Dynamic.literal("languageCode" -> voice.languageCode, "name" -> voice.name),
      "audioConfig" -> js.Dynamic.literal("audioEncoding" -> "MP3")
    )

  /** The characters Google bills for `ssml`: all of it but `<mark>` tags,
    * which the plugin does not use.
    */
  def billedCharacters(ssml: String): Long = ssml.length.toLong

  /** Where gcloud keeps Application Default Credentials, unless the standard
    * environment variable points elsewhere.
    */
  def credentialsPath(env: js.Dynamic, home: String): String =
    val path = js.Dynamic.global.process.getBuiltinModule("node:path")
    Option(str(env.GOOGLE_APPLICATION_CREDENTIALS)).filter(_.nonEmpty).getOrElse {
      val configDir =
        Option(str(env.CLOUDSDK_CONFIG)).filter(_.nonEmpty).getOrElse(path.join(home, ".config", "gcloud").asInstanceOf[String])
      path.join(configDir, "application_default_credentials.json").asInstanceOf[String]
    }

  /** A user's Application Default Credentials. Only a user's are read: a
    * service account's key would need a signed JWT instead, which nothing
    * here needs.
    */
  final case class UserCredentials(clientId: String, clientSecret: String, refreshToken: String, quotaProject: Option[String])

  def readCredentials(path: String): Either[String, UserCredentials] =
    val fs = js.Dynamic.global.process.getBuiltinModule("node:fs")
    Try(fs.readFileSync(path, "utf8").asInstanceOf[String]).toEither.left
      .map(_ =>
        s"no Google credentials at $path: run `gcloud auth application-default login`, or set an API key in the settings"
      )
      .flatMap { json =>
        Try(js.JSON.parse(json)).toEither.left.map(_ => s"$path is not JSON").flatMap { parsed =>
          str(parsed.`type`) match
            case "authorized_user" =>
              Right(
                UserCredentials(
                  str(parsed.client_id),
                  str(parsed.client_secret),
                  str(parsed.refresh_token),
                  Option(str(parsed.quota_project_id)).filter(_.nonEmpty)
                )
              )
            case other => Left(s"$path holds credentials of type '$other'; only a user's (authorized_user) are supported")
        }
      }

  /** Exchanges a refresh token for an access token: the token and the seconds
    * it is good for.
    */
  def fetchAccessToken(credentials: UserCredentials)(using ExecutionContext): Future[Either[String, (String, Double)]] =
    val form = js.Dynamic.newInstance(js.Dynamic.global.URLSearchParams)()
    val _ = form.set("grant_type", "refresh_token")
    val _ = form.set("client_id", credentials.clientId)
    val _ = form.set("client_secret", credentials.clientSecret)
    val _ = form.set("refresh_token", credentials.refreshToken)
    js.Dynamic.global
      .fetch(TokenUrl, js.Dynamic.literal("method" -> "POST", "body" -> form))
      .asInstanceOf[js.Promise[js.Dynamic]]
      .toFuture
      .flatMap { response =>
        response.json().asInstanceOf[js.Promise[js.Dynamic]].toFuture.map { body =>
          if response.ok.asInstanceOf[Boolean] then
            Right((str(body.access_token), body.expires_in.asInstanceOf[js.UndefOr[Double]].getOrElse(3600.0)))
          else
            Left(
              s"Google refused to refresh the access token (${str(response.status)} ${str(body.error)}: " +
                s"${str(body.error_description)}); run `gcloud auth application-default login` again"
            )
        }
      }

/** The access token for Google's APIs, refreshed from the user's Application
  * Default Credentials when there is none or it is about to expire. One
  * refresh at a time: callers arriving during one share it.
  */
private final class GoogleAccessTokens(credentialsPath: () => String)(using ExecutionContext):
  private var current: Option[(String, Double)]              = None // token, expiry as Date.now()
  private var refreshing: Option[Future[Either[String, GoogleCloudTts.Auth]]] = None

  /** Reusing a token until its last minute leaves a request time to complete. */
  private val RenewBeforeExpiryMs = 60_000.0

  def auth(configuredProject: Option[String]): Future[Either[String, GoogleCloudTts.Auth]] =
    GoogleCloudTts.readCredentials(credentialsPath()) match
      case Left(message) => Future.successful(Left(message))
      case Right(credentials) =>
        val project = configuredProject.orElse(credentials.quotaProject)
        current match
          case Some((token, expiresAt)) if js.Date.now() < expiresAt - RenewBeforeExpiryMs =>
            Future.successful(Right(GoogleCloudTts.Auth.Bearer(token, project)))
          case _ =>
            refreshing.getOrElse {
              val started = GoogleCloudTts.fetchAccessToken(credentials).map(_.map { (token, seconds) =>
                current = Some((token, js.Date.now() + seconds * 1000))
                GoogleCloudTts.Auth.Bearer(token, project)
              })
              refreshing = Some(started)
              started.onComplete(_ => refreshing = None)
              started
            }
