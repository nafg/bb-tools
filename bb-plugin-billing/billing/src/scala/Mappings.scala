package billing

import bbplugin.Dyn.str
import bbplugin.BbApi
import billingcore.{Billability, Mapping, MappingEntry, ProjectRule, Resolved, ZohoTarget}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** The `billing` metadata namespace on threads: `{ zoho: { projectId, projectName, taskId, taskName, customerId,
  * customerName } }`, `{ zoho: null }` for an explicitly unbillable thread, or no `zoho` key to inherit.
  */
final class Mappings(bb: BbApi)(using ExecutionContext):
  private val Digits = """\d+""".r

  /** A namespace's `zoho` entry. A malformed value counts as absent and is reported, never silently trusted. */
  def entryOf(metadata: js.Dynamic): Either[String, MappingEntry] =
    if metadata == null || js.typeOf(metadata) != "object" then Right(MappingEntry.Absent)
    else
      val zoho = metadata.zoho.asInstanceOf[js.UndefOr[js.Dynamic]]
      if zoho.isEmpty then Right(MappingEntry.Absent)
      else if zoho.get == null then Right(MappingEntry.Unbillable)
      else if js.typeOf(zoho.get) != "object" then Left("zoho is neither an object nor null")
      else
        val z = zoho.get
        (str(z.projectId), str(z.taskId)) match
          case (Digits(), Digits()) =>
            Right(
              MappingEntry.Assigned(
                ZohoTarget(
                  projectId = str(z.projectId),
                  projectName = str(z.projectName),
                  taskId = str(z.taskId),
                  taskName = str(z.taskName),
                  customerId = str(z.customerId),
                  customerName = str(z.customerName)
                )
              )
            )
          case _                    => Left("zoho.projectId and zoho.taskId must be Zoho ids")

  private def chunked[A](ids: Seq[String])(fetch: Seq[String] => Future[Seq[A]]): Future[Vector[A]] =
    ids.distinct.grouped(200).foldLeft(Future.successful(Vector.empty[A])) { (acc, chunk) =>
      acc.flatMap(done => fetch(chunk).map(done ++ _))
    }

  /** Each thread's ancestors, parent first, archived and deleted ancestors included (bb's ancestors route has no
    * archived filter, unlike its descendants route). Threads bb does not know are absent from the result.
    */
  def ancestors(threadIds: Seq[String]): Future[Map[String, Vector[String]]] =
    if threadIds.isEmpty then Future.successful(Map.empty)
    else
      chunked(threadIds) { chunk =>
        bb.sdk.threads
          .experimental_listAncestors(js.Dynamic.literal("threadIds" -> chunk.toJSArray))
          .toFuture
          .map(_.threads.asInstanceOf[js.Array[js.Dynamic]].toSeq.map { t =>
            str(t.threadId) -> t.ancestorIds.asInstanceOf[js.Array[String]].toVector
          })
      }.map(_.toMap)

  /** Each thread's `billing` namespace; threads without one are absent. */
  def namespaces(threadIds: Seq[String]): Future[Map[String, js.Dynamic]] =
    if threadIds.isEmpty then Future.successful(Map.empty)
    else
      chunked(threadIds) { chunk =>
        bb.sdk.threads
          .experimental_listPluginMetadata(js.Dynamic.literal("threadIds" -> chunk.toJSArray))
          .toFuture
          .map(_.threads.asInstanceOf[js.Array[js.Dynamic]].toSeq.map(t => str(t.threadId) -> t.metadata))
      }.map(_.toMap)

  /** Where every listed thread's time belongs, following each one's ancestry and then its bb project's default
    * (`projectRule` answers for a thread id).
    */
  def resolveAll(threadIds: Seq[String], projectRule: String => Option[ProjectRule]): Future[Resolution] =
    ancestors(threadIds).flatMap { ancestry =>
      val everyThread = (threadIds ++ ancestry.values.flatten).distinct
      namespaces(everyThread).map { spaces =>
        val entries                        = everyThread.map { t =>
          t -> spaces.get(t).map(entryOf).getOrElse(Right(MappingEntry.Absent))
        }.toMap
        val warnings                       = entries.collect { case (t, Left(problem)) =>
          s"thread $t has an invalid billing mapping: $problem"
        }
        val lookup: String => MappingEntry = t => entries.get(t).flatMap(_.toOption).getOrElse(MappingEntry.Absent)
        Resolution(
          everyThread.map(t =>
            t -> Mapping.resolve(t, ancestry.getOrElse(t, Vector.empty), lookup, projectRule(t))
          ).toMap,
          ancestry,
          warnings.toVector.sorted
        )
      }
    }

  def resolve(threadId: String, projectRule: String => Option[ProjectRule]): Future[Resolved] =
    resolveAll(Seq(threadId), projectRule).map(_.resolved.getOrElse(
      threadId,
      Resolved(Billability.NotBillable, None)
    ))

  def assign(threadId: String, target: ZohoTarget): Future[Unit] =
    bb.sdk.threads
      .updatePluginMetadata(js.Dynamic.literal(
        "threadId" -> threadId,
        "set"      -> js.Dynamic.literal("zoho" -> Json.targetJs(target))
      ))
      .toFuture
      .map(_ => ())

  def markUnbillable(threadId: String): Future[Unit] =
    bb.sdk.threads
      .updatePluginMetadata(js.Dynamic.literal("threadId" -> threadId, "set" -> js.Dynamic.literal("zoho" -> null)))
      .toFuture
      .map(_ => ())

  def clear(threadId: String): Future[Unit] =
    bb.sdk.threads
      .updatePluginMetadata(js.Dynamic.literal("threadId" -> threadId, "remove" -> js.Array("zoho")))
      .toFuture
      .map(_ => ())

/** `ancestry` holds each requested thread's ancestors, parent first. */
final case class Resolution(
  resolved: Map[String, Resolved],
  ancestry: Map[String, Vector[String]],
  warnings: Vector[String])
