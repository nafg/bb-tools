package billing

import bbplugin.Dyn.{number, str}
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.URIUtils.encodeURIComponent

class ZohoException(message: String) extends Exception(message)

final case class ZohoCredentials(clientId: String, clientSecret: String, organizationId: String, refreshToken: String)

final case class ZohoOrganization(name: String, timeZone: String)
final case class ZohoProject(projectId: String, projectName: String, customerId: String, customerName: String)
final case class ZohoTask(taskId: String, taskName: String)

/** Zoho Books REST (https://www.zoho.com/books/api/v3/). Access tokens come from the refresh token and are kept until
  * shortly before they expire. Every request carries the organization id.
  */
class ZohoClient(credentials: ZohoCredentials, log: String => Unit)(using ExecutionContext):
  private val accountsUrl = "https://accounts.zoho.com/oauth/v2/token"
  private val apiUrl      = "https://www.zohoapis.com/books/v3"

  private var accessToken: Option[(String, Double)] = None

  private def fetch(url: String, init: js.Dynamic): Future[js.Dynamic] =
    js.Dynamic.global.fetch(url, init).asInstanceOf[js.Promise[js.Dynamic]].toFuture

  private def readJson(response: js.Dynamic, what: String): Future[js.Dynamic] =
    response.text().asInstanceOf[js.Promise[String]].toFuture.flatMap { text =>
      val status  = number(response.status).fold(0)(_.toInt)
      val parsed  = scala.util.Try(js.JSON.parse(text)).toOption
      val message = parsed.map(_.message).map(str).filter(_.nonEmpty).getOrElse(text.take(300))
      if status >= 400 then Future.failed(ZohoException(s"Zoho $what failed ($status): $message"))
      else
        parsed match
          case Some(json) => Future.successful(json)
          case None       => Future.failed(ZohoException(s"Zoho $what returned something other than JSON"))
    }

  private def token: Future[String] =
    accessToken match
      case Some((token, expiresAt)) if js.Date.now() < expiresAt => Future.successful(token)
      case _                                                     =>
        val form = js.Dynamic.newInstance(js.Dynamic.global.URLSearchParams)()
        val _    = form.set("grant_type", "refresh_token")
        val _    = form.set("client_id", credentials.clientId)
        val _    = form.set("client_secret", credentials.clientSecret)
        val _    = form.set("refresh_token", credentials.refreshToken)
        fetch(accountsUrl, js.Dynamic.literal("method" -> "POST", "body" -> form))
          .flatMap(readJson(_, "token refresh"))
          .flatMap { json =>
            val token = str(json.access_token)
            val error = str(json.error)
            if token.isEmpty then
              Future.failed(ZohoException(
                s"Zoho token refresh failed: ${if error.isEmpty then "no access token in the response" else error}"
              ))
            else
              val expiresIn = json.expires_in.asInstanceOf[js.UndefOr[Double]].getOrElse(3600.0)
              accessToken = Some((token, js.Date.now() + (expiresIn - 60) * 1000))
              log("Zoho access token refreshed")
              Future.successful(token)
          }

  private def request(
    method: String,
    path: String,
    query: Seq[(String, String)],
    body: Option[js.Any]): Future[js.Dynamic] =
    token.flatMap { token =>
      val params  = (("organization_id" -> credentials.organizationId) +: query)
        .map((k, v) => s"${encodeURIComponent(k)}=${encodeURIComponent(v)}")
        .mkString("&")
      val headers = js.Dynamic.literal("Authorization" -> s"Zoho-oauthtoken $token")
      val init    = js.Dynamic.literal("method" -> method, "headers" -> headers)
      body.foreach { b =>
        headers.updateDynamic("Content-Type")("application/json")
        init.updateDynamic("body")(js.JSON.stringify(b.asInstanceOf[js.Object]))
      }
      fetch(s"$apiUrl$path?$params", init).flatMap(readJson(_, s"$method $path"))
    }

  def organization(): Future[ZohoOrganization] =
    request("GET", s"/organizations/${credentials.organizationId}", Nil, None).map { json =>
      val org = json.organization
      ZohoOrganization(str(org.name), str(org.time_zone))
    }

  /** The user the credentials belong to; time entries are logged under this id. */
  def currentUserId(): Future[String] =
    request("GET", "/users/me", Nil, None).map { json =>
      val id = str(json.user.user_id)
      if id.isEmpty then throw ZohoException("Zoho GET /users/me returned no user_id") else id
    }

  private def pages(path: String, query: Seq[(String, String)], key: String): Future[Vector[js.Dynamic]] =
    def page(n: Int, acc: Vector[js.Dynamic]): Future[Vector[js.Dynamic]] =
      request("GET", path, query ++ Seq("page" -> n.toString, "per_page" -> "200"), None).flatMap { json =>
        val items =
          json.selectDynamic(key).asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].fold(Vector.empty)(_.toVector)
        val more  = json.page_context.asInstanceOf[js.UndefOr[js.Dynamic]].toOption
          .exists(_.has_more_page.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false))
        if more && items.nonEmpty then page(n + 1, acc ++ items) else Future.successful(acc ++ items)
      }
    page(1, Vector.empty)

  def activeProjects(): Future[Vector[ZohoProject]] =
    pages("/projects", Seq("filter_by" -> "Status.Active"), "projects").map(_.map { p =>
      ZohoProject(str(p.project_id), str(p.project_name), str(p.customer_id), str(p.customer_name))
    })

  def tasks(projectId: String): Future[Vector[ZohoTask]] =
    request("GET", s"/projects/$projectId/tasks", Nil, None).map { json =>
      // The documented key is `task`; accept `tasks` too.
      val items = List("task", "tasks").iterator
        .map(k => json.selectDynamic(k).asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]])
        .collectFirst { case arr if arr.isDefined => arr.get.toVector }
        .getOrElse(Vector.empty)
      items.map(t => ZohoTask(str(t.task_id), str(t.task_name)))
    }

  /** Time entries logged in the date range for the project, whoever created them. */
  def timeEntries(fromDate: String, toDate: String, projectId: String): Future[Vector[ZohoExistingEntry]] =
    pages(
      "/projects/timeentries",
      Seq("from_date" -> fromDate, "to_date" -> toDate, "project_id" -> projectId),
      "time_entries"
    ).map(_.map { e =>
      ZohoExistingEntry(
        timeEntryId = str(e.time_entry_id),
        date = str(e.log_date),
        logTime = str(e.log_time),
        taskName = str(e.task_name),
        userName = str(e.user_name),
        notes = str(e.notes),
        billedStatus = str(e.billed_status)
      )
    })

  private def entryId(what: String)(json: js.Dynamic): Future[String] =
    val id = str(json.time_entry.time_entry_id)
    if id.isEmpty then Future.failed(ZohoException(s"Zoho $what returned no time_entry_id")) else Future.successful(id)

  /** Creates a time entry dated `logDate` (yyyy-MM-dd) with `logTime` (HH:mm); resolves its `time_entry_id`. */
  def createTimeEntry(entry: ZohoTimeEntryBody): Future[String] =
    request("POST", "/projects/timeentries", Nil, Some(entry.json)).flatMap(entryId("POST /projects/timeentries"))

  def deleteTimeEntry(timeEntryId: String): Future[Unit] =
    request("DELETE", s"/projects/timeentries/$timeEntryId", Nil, None).map(_ => ())

/** A time entry as Zoho lists it: a date and `HH:mm` of logged time, no clock times. */
final case class ZohoExistingEntry(
  timeEntryId: String,
  date: String,
  logTime: String,
  taskName: String,
  userName: String,
  notes: String,
  billedStatus: String):
  def minutes: Long =
    logTime.split(':') match
      case Array(h, m) => h.toLongOption.getOrElse(0L) * 60 + m.toLongOption.getOrElse(0L)
      case _           => 0L

final case class ZohoTimeEntryBody(
  projectId: String,
  taskId: String,
  userId: String,
  logDate: String,
  logTime: String,
  notes: String):
  def json: js.Dynamic =
    js.Dynamic.literal(
      "project_id"  -> projectId,
      "task_id"     -> taskId,
      "user_id"     -> userId,
      "log_date"    -> logDate,
      "log_time"    -> logTime,
      "is_billable" -> true,
      "notes"       -> notes
    )
