package billing

import bbplugin.BbApi
import bbplugin.Dyn.str
import billingcore.{ProjectMapping, ProjectZoho, ZohoTarget}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** Which Zoho projects each bb project's threads may bill to, kept in the plugin's key-value store as `{ <bbProjectId>:
  * [{ projectId, taskId }] }`. A thread chooses among them; a bb project with exactly one is that project's default for
  * threads that choose nothing.
  */
final class ProjectMappings(bb: BbApi)(using ExecutionContext):
  private val Key = "projectMappings"

  def all(): Future[Map[String, Vector[ProjectZoho]]] =
    bb.storage.kv.get(Key).toFuture.map { stored =>
      stored.asInstanceOf[js.UndefOr[js.Dynamic]].toOption.filter(v => v != null && js.typeOf(v) == "object").fold(
        Map.empty
      ) { obj =>
        js.Object.keys(obj.asInstanceOf[js.Object]).toVector.map { bbProjectId =>
          val items = obj.selectDynamic(bbProjectId).asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].fold(
            Vector.empty[js.Dynamic]
          )(_.toVector)
          bbProjectId -> items.flatMap { item =>
            val p = str(item.projectId)
            val t = str(item.taskId)
            Option.when(p.nonEmpty && t.nonEmpty)(ProjectZoho(p, t))
          }
        }.toMap
      }
    }

  def set(bbProjectId: String, zoho: Vector[ProjectZoho]): Future[Map[String, Vector[ProjectZoho]]] =
    all().flatMap { current =>
      val next = if zoho.isEmpty then current - bbProjectId else current.updated(bbProjectId, zoho)
      val obj  = js.Dynamic.literal()
      next.foreach { (id, items) =>
        obj.updateDynamic(id)(items.map(i =>
          js.Dynamic.literal("projectId" -> i.projectId, "taskId" -> i.taskId)
        ).toJSArray)
      }
      bb.storage.kv.set(Key, obj).toFuture.map(_ => next)
    }

  /** Each bb project's default target, given the catalog to name it. */
  def defaults(catalog: Option[Catalog]): Future[Map[String, ZohoTarget]] =
    all().map(_.flatMap { (bbProjectId, zoho) =>
      ProjectMapping.default(zoho, (p, t) => catalog.flatMap(_.target(p, t))).map(bbProjectId -> _)
    })
