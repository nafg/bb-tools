package billing

import bbplugin.Dyn.str
import bbplugin.{BbApi, BbSettingsHandle}
import billingcore.ZohoTarget

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

final case class CatalogProject(project: ZohoProject, tasks: Vector[ZohoTask])

/** Projects and their tasks as last fetched from Zoho, kept in the plugin's key-value store. */
final case class Catalog(projects: Vector[CatalogProject], fetchedAt: Double):
  def target(projectId: String, taskId: String): Option[ZohoTarget] =
    for
      p <- projects.find(_.project.projectId == projectId)
      t <- p.tasks.find(_.taskId == taskId)
    yield ZohoTarget(
      p.project.projectId,
      p.project.projectName,
      t.taskId,
      t.taskName,
      p.project.customerId,
      p.project.customerName
    )

  /** A project by id or exact name, else by unique case-insensitive name. */
  def findProject(text: String): Either[String, CatalogProject] =
    val wanted = text.trim
    projects.find(p => p.project.projectId == wanted || p.project.projectName == wanted) match
      case Some(p) => Right(p)
      case None    =>
        projects.filter(_.project.projectName.equalsIgnoreCase(wanted)) match
          case Vector(p) => Right(p)
          case Vector()  => Left(s"no active Zoho project named \"$wanted\"")
          case many      =>
            Left(s"several Zoho projects are named \"$wanted\": ${many.map(_.project.projectId).mkString(", ")}")

  def findTask(project: CatalogProject, text: String): Either[String, ZohoTask] =
    val wanted = text.trim
    project.tasks.find(t => t.taskId == wanted || t.taskName == wanted) match
      case Some(t) => Right(t)
      case None    =>
        project.tasks.filter(_.taskName.equalsIgnoreCase(wanted)) match
          case Vector(t) => Right(t)
          case Vector()  => Left(s"project ${project.project.projectName} has no task named \"$wanted\"")
          case many      => Left(
              s"several tasks in ${project.project.projectName} are named \"$wanted\": ${many.map(_.taskId).mkString(", ")}"
            )

  def customers: Vector[(String, String)] =
    projects.map(p => p.project.customerId -> p.project.customerName).distinct.sortBy(_._2)

  def json: js.Dynamic =
    js.Dynamic.literal(
      "fetchedAt" -> fetchedAt,
      "projects"  -> projects.map { p =>
        js.Dynamic.literal(
          "projectId"    -> p.project.projectId,
          "projectName"  -> p.project.projectName,
          "customerId"   -> p.project.customerId,
          "customerName" -> p.project.customerName,
          "tasks" -> p.tasks.map(t => js.Dynamic.literal("taskId" -> t.taskId, "taskName" -> t.taskName)).toJSArray
        )
      }.toJSArray
    )

object Catalog:
  def fromJson(value: js.Dynamic): Option[Catalog] =
    if value == null || js.typeOf(value) != "object" then None
    else
      val projects =
        value.projects.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].fold(Vector.empty[js.Dynamic])(_.toVector)
      Some(
        Catalog(
          projects.map { p =>
            CatalogProject(
              ZohoProject(str(p.projectId), str(p.projectName), str(p.customerId), str(p.customerName)),
              p.tasks.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].fold(Vector.empty[js.Dynamic])(_.toVector)
                .map(t => ZohoTask(str(t.taskId), str(t.taskName)))
            )
          },
          str(value.fetchedAt).toDoubleOption.getOrElse(0.0)
        )
      )

/** Where the review's time zone came from. */
enum ZoneSource:
  /** The Zoho organization's zone. */
  case Zoho

  /** The bb server's own zone, used while Zoho is not configured. */
  case Server

/** The configured Zoho connection: credentials from settings, the organization's zone and the catalog cached in kv. The
  * zone and user id caches are keyed by the organization and client they were fetched for, so changed credentials never
  * log under the old user or split days in the old zone.
  */
final class Zoho(bb: BbApi, settings: BbSettingsHandle)(using ExecutionContext):
  private val CatalogKey = "zoho.catalog"

  private var client: Option[(ZohoCredentials, ZohoClient)] = None

  private def credentials(): Future[ZohoCredentials] =
    settings.get().toFuture.map { s =>
      ZohoCredentials(
        str(s.clientId).trim,
        str(s.clientSecret).trim,
        str(s.organizationId).trim,
        str(s.refreshToken).trim
      )
    }

  /** The settings still empty; the connection is usable only when this is empty. */
  def missingSettings(): Future[List[String]] =
    settings.get().toFuture.map { s =>
      List("clientId", "clientSecret", "organizationId", "refreshToken").filter(k =>
        str(s.selectDynamic(k)).trim.isEmpty
      )
    }

  def clientOrFail(): Future[ZohoClient] =
    credentials().flatMap { creds =>
      if creds.clientId.isEmpty || creds.clientSecret.isEmpty || creds.organizationId.isEmpty || creds.refreshToken.isEmpty
      then
        Future.failed(ZohoException(
          "Zoho is not configured: set the client id, client secret, organization id, and refresh token in the Billing plugin settings"
        ))
      else
        client match
          case Some((c, existing)) if c == creds => Future.successful(existing)
          case _                                 =>
            val fresh = ZohoClient(creds, bb.log.info)
            client = Some((creds, fresh))
            Future.successful(fresh)
    }

  /** The organization's time zone and where it came from: the kv cache or Zoho; without Zoho configured, the server's
    * own zone.
    */
  def timeZone(): Future[(String, ZoneSource)] =
    credentials().flatMap { creds =>
      if creds.organizationId.isEmpty then Future.successful((IntlCalendar.serverZone, ZoneSource.Server))
      else
        val key = s"zoho.organization:${creds.organizationId}"
        bb.storage.kv.get(key).toFuture.flatMap { cached =>
          val zone = cached.asInstanceOf[js.UndefOr[js.Dynamic]].toOption.filter(_ != null).map(o =>
            str(o.timeZone)
          ).filter(_.nonEmpty)
          zone match
            case Some(z) => Future.successful((z, ZoneSource.Zoho))
            case None    =>
              missingSettings().flatMap { missing =>
                if missing.nonEmpty then Future.successful((IntlCalendar.serverZone, ZoneSource.Server))
                else
                  clientOrFail().flatMap(_.organization()).flatMap { org =>
                    bb.storage.kv
                      .set(key, js.Dynamic.literal("name" -> org.name, "timeZone" -> org.timeZone))
                      .toFuture
                      .map(_ => (org.timeZone, ZoneSource.Zoho))
                  }
              }
        }
    }

  /** The id Zoho logs entries under for these credentials. */
  def userId(): Future[String] =
    credentials().flatMap { creds =>
      val key = s"zoho.userId:${creds.clientId}:${creds.organizationId}"
      bb.storage.kv.get(key).toFuture.flatMap { cached =>
        Option(str(cached)).filter(_.nonEmpty) match
          case Some(id) => Future.successful(id)
          case None     =>
            clientOrFail().flatMap(_.currentUserId()).flatMap(id => bb.storage.kv.set(key, id).toFuture.map(_ => id))
      }
    }

  def cachedCatalog(): Future[Option[Catalog]] = bb.storage.kv.get(CatalogKey).toFuture.map(v => Catalog.fromJson(v))

  def refreshCatalog(): Future[Catalog] =
    clientOrFail().flatMap { c =>
      c.activeProjects().flatMap { projects =>
        Future
          .traverse(projects)(p => c.tasks(p.projectId).map(tasks => CatalogProject(p, tasks)))
          .map(ps => Catalog(ps.sortBy(p => (p.project.customerName, p.project.projectName)), js.Date.now()))
      }
    }.flatMap(catalog => bb.storage.kv.set(CatalogKey, catalog.json).toFuture.map(_ => catalog))

  /** The cached catalog, fetched once when there is none. */
  def catalog(refresh: Boolean): Future[Catalog] =
    if refresh then refreshCatalog()
    else cachedCatalog().flatMap(_.fold(refreshCatalog())(Future.successful))
