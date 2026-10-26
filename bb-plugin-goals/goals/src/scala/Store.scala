package goals

import activitycore.{ActivityJson, Interval}
import bbplugin.{BbApi, Dyn}
import goalscore.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** Everything the plugin reads from bb and from the activity recorder, returned as core-model values.
  */
final class Store(bb: BbApi)(using ExecutionContext):
  private val PageSize  = 200
  private val BatchSize = 200

  /** Every visible thread, archived ones included: a finished thread may still owe billing or labeling. Pages until a page comes back
    * empty, however large the server lets a page be.
    */
  def allThreads(): Future[List[ThreadInfo]] =
    def page(archived: Boolean, offset: Int, acc: List[ThreadInfo]): Future[List[ThreadInfo]] =
      bb.sdk.threads
        .list(js.Dynamic.literal("archived" -> archived, "limit" -> PageSize, "offset" -> offset))
        .toFuture
        .flatMap { rows =>
          if rows.isEmpty then Future.successful(acc)
          else page(archived, offset + rows.length, acc ++ rows.toList.flatMap(threadInfo))
        }
    for
      open     <- page(archived = false, 0, Nil)
      archived <- page(archived = true, 0, Nil)
    yield open ++ archived

  private def threadInfo(row: js.Dynamic): Option[ThreadInfo] =
    for
      id        <- Js.optStr(row.id)
      projectId <- Js.optStr(row.projectId)
    yield ThreadInfo(
      id = id,
      parentId = Js.optStr(row.parentThreadId),
      projectId = projectId,
      title = Js.optStr(row.title).orElse(Js.optStr(row.titleFallback)).getOrElse(id),
      activeAt = Dyn.number(row.latestAttentionAt).orElse(Dyn.number(row.updatedAt)).map(_.toLong).getOrElse(0L),
      archived = Dyn.number(row.archivedAt).isDefined
    )

  def projectNames(): Future[Map[String, String]] =
    bb.sdk.projects
      .list(js.Dynamic.literal("includePersonal" -> true))
      .toFuture
      .map(_.toList.flatMap(p => for id <- Js.optStr(p.id); name <- Js.optStr(p.name) yield id -> name).toMap)

  /** Each thread's ancestors from its parent upward. */
  def ancestors(threadIds: List[String]): Future[Map[String, List[String]]] =
    batched(threadIds) { batch =>
      bb.sdk.threads
        .experimental_listAncestors(js.Dynamic.literal("threadIds" -> batch.toJSArray))
        .toFuture
        .map { response =>
          Js.optArr(response.threads).getOrElse(Nil).flatMap { row =>
            Js.optStr(row.threadId).map(_ -> Js.optArr(row.ancestorIds).getOrElse(Nil).flatMap(Js.optStr))
          }
        }
    }.map(_.toMap)

  /** The goals namespace of each thread that has one. */
  def labels(threadIds: List[String]): Future[Map[String, ThreadLabels]] =
    batched(threadIds) { batch =>
      bb.sdk.threads
        .experimental_listPluginMetadata(js.Dynamic.literal("threadIds" -> batch.toJSArray))
        .toFuture
        .map { response =>
          Js.optArr(response.threads).getOrElse(Nil).flatMap { row =>
            Js.optStr(row.threadId).map(_ -> Js.labelsOf(row.metadata))
          }
        }
    }.map(_.toMap)

  /** `labels` for `threadIds`, then for every thread those wait on, and so on, so lending can follow a chain through closed threads. */
  private def labelsWithBlockers(threadIds: List[String]): Future[Map[String, ThreadLabels]] =
    def closure(loaded: Map[String, ThreadLabels], asked: Set[String]): Future[Map[String, ThreadLabels]] =
      val next = loaded.values.flatMap(_.blockedBy).toList.distinct.filterNot(asked.contains)
      if next.isEmpty then Future.successful(loaded)
      else labels(next).flatMap(more => closure(loaded ++ more, asked ++ next))
    labels(threadIds).flatMap(closure(_, threadIds.toSet))

  private def batched[A](ids: List[String])(load: List[String] => Future[List[A]]): Future[List[A]] =
    ids.distinct.grouped(BatchSize).foldLeft(Future.successful(List.empty[A])) { (acc, batch) =>
      acc.flatMap(done => load(batch).map(done ++ _))
    }

  def ownLabels(threadId: String): Future[ThreadLabels] =
    bb.sdk.threads.getPluginMetadata(js.Dynamic.literal("threadId" -> threadId)).toFuture.map(Js.labelsOf)

  /** Writes the given fields; `None` inside a `Some` clears that field. Returns the stored namespace. */
  def write(
      threadId: String,
      goal: Option[Option[Goal]],
      value: Option[Option[Int]],
      blockedBy: Option[List[String]],
      myMove: Option[Option[MyMove]] = None
  ): Future[ThreadLabels] =
    val set    = js.Dynamic.literal()
    val remove = js.Array[String]()
    goal.foreach(_.fold(remove.push("goal"))(g => set.updateDynamic("goal")(g.letter)))
    value.foreach(_.fold(remove.push("value"))(v => set.updateDynamic("value")(v)))
    blockedBy.foreach(ids => if ids.isEmpty then remove.push("blockedBy") else set.updateDynamic("blockedBy")(ids.toJSArray))
    myMove.foreach {
      case None    => remove.push("myMove", "myMoveAt")
      case Some(m) =>
        set.updateDynamic("myMove")(m.name)
        set.updateDynamic("myMoveAt")(js.Date.now())
    }
    bb.sdk.threads
      .updatePluginMetadata(js.Dynamic.literal("threadId" -> threadId, "set" -> set, "remove" -> remove))
      .toFuture
      .map(Js.labelsOf)

  /** Every thread with the labels inheritance and lending read: theirs, their ancestors', and those of every thread reachable through
    * `blockedBy`. `within` keeps the threads active in a window (see `Overview.active`), given each one's attention seconds there.
    */
  final case class Snapshot(threads: List[ThreadInfo], own: Map[String, ThreadLabels], ancestors: Map[String, List[String]]):
    def resolved: Map[String, Resolved]                             = Overview.resolve(threads, own, ancestors)
    def within(since: Long, seconds: Map[String, Double]): Snapshot =
      copy(threads = threads.filter(t => Overview.active(t, own.get(t.id), seconds.getOrElse(t.id, 0.0), since)))

  def snapshot(): Future[Snapshot] =
    for
      threads <- allThreads()
      ids = threads.map(_.id)
      anc <- ancestors(ids)
      own <- labelsWithBlockers(ids ++ anc.values.flatten)
    yield Snapshot(threads, own, anc)

  /** The label a single thread is read as having, without loading every thread. */
  def effectiveOf(threadId: String): Future[(ThreadLabels, Option[Effective])] =
    for
      own <- ownLabels(threadId)
      anc <- ancestors(List(threadId))
      ids = anc.getOrElse(threadId, Nil)
      more <- if own.goal.isDefined || ids.isEmpty then Future.successful(Map.empty[String, ThreadLabels]) else labels(ids)
    yield own -> Inheritance.effective(threadId, more + (threadId -> own), anc)

  /** When the user last wrote to the thread: the newest turn request the user initiated, not an agent, bb, or an automation (those arrive
    * tagged as the user, so their "[bb " prefix is the only sign of them). None when there is no such message.
    */
  def latestUserMessageAt(threadId: String): Future[Option[Long]] =
    bb.sdk.threads.events
      .list(js.Dynamic.literal("threadId" -> threadId, "types" -> js.Array("client/turn/requested"), "order" -> "desc", "limit" -> "20"))
      .toFuture
      .map { rows =>
        rows.toList.iterator
          .filter(row => isUserMessage(row.data))
          .flatMap(row => Dyn.number(row.createdAt).map(_.toLong))
          .nextOption()
      }

  private def isUserMessage(data: js.Dynamic): Boolean =
    Js.optStr(data.initiator).contains("user") && Js.optStr(data.senderThreadId).forall(_.isEmpty) && !messageText(data).startsWith("[bb ")

  private def messageText(data: js.Dynamic): String =
    Js.optArr(data.input).getOrElse(Nil).filter(b => Js.optStr(b.`type`).contains("text")).flatMap(b => Js.optStr(b.text)).mkString

  // ---- activity recorder ----

  private val RecorderId = "activity-recorder"

  /** Attention seconds per thread in [from, to), each interval counted in full: `Right(None)` when the recorder plugin is not installed,
    * `Left(message)` when it is but could not be read or answered in a shape the shared parser rejects. Discovery decides "not installed"
    * because `callRpc`'s error codes have no unknown-plugin case, so a failed call alone cannot tell missing from broken.
    */
  def attentionSeconds(from: Double, to: Double): Future[Either[String, Option[Map[String, Double]]]] =
    bb.sdk.plugins
      .experimental_discoverRpc(js.Dynamic.literal("pluginId" -> RecorderId, "method" -> "intervals"))
      .toFuture
      .flatMap { methods =>
        if methods.isEmpty then Future.successful(Right(None))
        else
          bb.sdk.plugins
            .callRpc(
              js.Dynamic.literal(
                "pluginId" -> RecorderId,
                "method"   -> "intervals",
                "input"    -> js.Dynamic.literal("from" -> from, "to" -> to),
                // callRpc only calls `parse` on this; the handler reads the result itself.
                "outputSchema" -> js.Dynamic.literal("parse" -> js.Any.fromFunction1((result: js.Any) => result))
              )
            )
            .toFuture
            .map(response => ActivityJson.intervals(response).map(intervals => Some(secondsByThread(intervals))))
      }
      .recover { case e => Left(e.getMessage) }

  /** Each interval counted in full for its thread, so overlapping threads each get all of their time. */
  private def secondsByThread(intervals: Vector[Interval]): Map[String, Double] =
    intervals.groupMapReduce(_.threadId)(i => math.max(i.end - i.start, 0L).toDouble / 1000)(_ + _)
