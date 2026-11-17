package goals

import activitycore.{ActivityJson, Interval}
import bbplugin.{BbApi, Dyn}
import goalscore.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** One thread's own labels, the threads it waits on, and the label it is read as having. */
final case class Shown(own: ThreadLabels, blockedBy: List[String], effective: Option[Effective])

/** Everything the plugin reads from bb, the activity recorder, and the subscriptions plugin, returned as core-model values.
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

  /** The raw goals namespace of each thread that has one. */
  private def metadata(threadIds: List[String]): Future[Map[String, js.Dynamic]] =
    batched(threadIds) { batch =>
      bb.sdk.threads
        .experimental_listPluginMetadata(js.Dynamic.literal("threadIds" -> batch.toJSArray))
        .toFuture
        .map(response => Js.optArr(response.threads).getOrElse(Nil).flatMap(row => Js.optStr(row.threadId).map(_ -> row.metadata)))
    }.map(_.toMap)

  /** The goals namespace of each thread that has one. */
  def labels(threadIds: List[String]): Future[Map[String, ThreadLabels]] =
    metadata(threadIds).map(_.view.mapValues(Js.labelsOf).toMap)

  /** The `blockedBy` lists goals stored before waits moved to the subscriptions plugin, for each thread that still has one. */
  def legacyBlockedBy(threadIds: List[String]): Future[Map[String, List[String]]] =
    metadata(threadIds).map(_.flatMap { (id, meta) =>
      Js.optArr(meta.blockedBy).map(ids => id -> ThreadLabels.legacyBlockedBy(ids.map(_.asInstanceOf[Any])))
    })

  def removeLegacyBlockedBy(threadId: String): Future[Unit] =
    bb.sdk.threads
      .updatePluginMetadata(js.Dynamic.literal("threadId" -> threadId, "set" -> js.Dynamic.literal(), "remove" -> js.Array("blockedBy")))
      .toFuture
      .map(_ => ())

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
      myMove: Option[Option[MyMove]] = None
  ): Future[ThreadLabels] =
    val set    = js.Dynamic.literal()
    val remove = js.Array[String]()
    goal.foreach(_.fold(remove.push("goal"))(g => set.updateDynamic("goal")(g.letter)))
    value.foreach(_.fold(remove.push("value"))(v => set.updateDynamic("value")(v)))
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

  /** Every thread with the labels inheritance and lending read: theirs, their ancestors', and those of every thread in a wait, and which
    * threads each waits on (none when the subscriptions plugin is not installed). `within` keeps the threads active in a window (see
    * `Overview.active`), given each one's attention seconds there.
    */
  final case class Snapshot(
      threads: List[ThreadInfo],
      own: Map[String, ThreadLabels],
      ancestors: Map[String, List[String]],
      blockedBy: Map[String, List[String]]
  ):
    def resolved: Map[String, Resolved]                             = Overview.resolve(threads, own, ancestors, blockedBy)
    def within(since: Long, seconds: Map[String, Double]): Snapshot =
      copy(threads = threads.filter(t => Overview.active(t, own.get(t.id), seconds.getOrElse(t.id, 0.0), since)))

  def snapshot(): Future[Snapshot] =
    val waits = threadWaits().map(_.getOrElse(Map.empty))
    for
      threads <- allThreads()
      ids = threads.map(_.id)
      anc   <- ancestors(ids)
      waits <- waits
      own   <- labels(ids ++ anc.values.flatten ++ waits.keys ++ waits.values.flatten)
    yield Snapshot(threads, own, anc, waits)

  /** A single thread's labels, waits, and the label it is read as having, without loading every thread. */
  def shownOf(threadId: String): Future[Shown] =
    val waits = threadWaits()
    for
      own <- ownLabels(threadId)
      anc <- ancestors(List(threadId))
      ids = anc.getOrElse(threadId, Nil)
      more  <- if own.goal.isDefined || ids.isEmpty then Future.successful(Map.empty[String, ThreadLabels]) else labels(ids)
      waits <- waits
    yield Shown(own, waits.flatMap(_.get(threadId)).getOrElse(Nil), Inheritance.effective(threadId, more + (threadId -> own), anc))

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

  /** The same rule as the activity recorder's `isUserMessage` (initiated by the user, no sender thread, no "[bb " prefix); keep the two in
    * step, since a message one counts as the user's and the other does not would clear a my-move mark the recorder still shows as
    * attention.
    */
  private def isUserMessage(data: js.Dynamic): Boolean =
    Js.optStr(data.initiator).contains("user") && Js.optStr(data.senderThreadId).forall(_.isEmpty) && !messageText(data).startsWith("[bb ")

  private def messageText(data: js.Dynamic): String =
    Js.optArr(data.input).getOrElse(Nil).filter(b => Js.optStr(b.`type`).contains("text")).flatMap(b => Js.optStr(b.text)).mkString

  // ---- other plugins ----

  /** Whether `pluginId` publishes `method`. Discovery decides "not installed" because `callRpc`'s error codes have no unknown-plugin case,
    * so a failed call alone cannot tell missing from broken.
    */
  private def publishes(pluginId: String, method: String): Future[Boolean] =
    bb.sdk.plugins
      .experimental_discoverRpc(js.Dynamic.literal("pluginId" -> pluginId, "method" -> method))
      .toFuture
      .map(_.nonEmpty)

  private def callRpc(pluginId: String, method: String, input: js.Any): Future[js.Dynamic] =
    bb.sdk.plugins
      .callRpc(
        js.Dynamic.literal(
          "pluginId" -> pluginId,
          "method"   -> method,
          "input"    -> input,
          // callRpc only calls `parse` on this; the caller reads the result itself.
          "outputSchema" -> js.Dynamic.literal("parse" -> js.Any.fromFunction1((result: js.Any) => result))
        )
      )
      .toFuture
      .map(_.asInstanceOf[js.Dynamic])

  // ---- subscriptions: the one record of which thread waits on which ----

  private val SubscriptionsId = "subscriptions"

  /** The threads each thread waits on: its subscriptions to their being archived. None when the subscriptions plugin is not installed. */
  def threadWaits(): Future[Option[Map[String, List[String]]]] =
    publishes(SubscriptionsId, "threadWaits").flatMap { installed =>
      if !installed then Future.successful(None)
      else
        callRpc(SubscriptionsId, "threadWaits", null).map { response =>
          val edges = Js.optArr(response.edges).getOrElse(Nil).flatMap { e =>
            for from <- Js.optStr(e.subscriberThreadId); to <- Js.optStr(e.targetThreadId) yield from -> to
          }
          Some(edges.groupMap(_._1)(_._2).view.mapValues(_.distinct).toMap)
        }
    }

  /** Subscribes `threadId` to `blocker` being archived; returns the subscriptions plugin's message. */
  def waitOn(threadId: String, blocker: String): Future[String] = changeWait("waitOnThread", threadId, blocker)

  /** Cancels `threadId`'s subscription to `blocker` being archived; returns the subscriptions plugin's message. */
  def stopWaitingOn(threadId: String, blocker: String): Future[String] = changeWait("stopWaitingOnThread", threadId, blocker)

  private def changeWait(method: String, threadId: String, blocker: String): Future[String] =
    callRpc(SubscriptionsId, method, js.Dynamic.literal("subscriberThreadId" -> threadId, "targetThreadId" -> blocker))
      .map(response => Js.optStr(response.message).getOrElse(""))

  // ---- activity recorder ----

  private val RecorderId = "activity-recorder"

  /** Attention seconds per thread in [from, to), each interval counted in full: `Right(None)` when the recorder plugin is not installed,
    * `Left(message)` when it is but could not be read or answered in a shape the shared parser rejects.
    */
  def attentionSeconds(from: Double, to: Double): Future[Either[String, Option[Map[String, Double]]]] =
    publishes(RecorderId, "intervals")
      .flatMap { installed =>
        if !installed then Future.successful(Right(None))
        else
          callRpc(RecorderId, "intervals", js.Dynamic.literal("from" -> from, "to" -> to))
            .map(response => ActivityJson.intervals(response).map(intervals => Some(secondsByThread(intervals))))
      }
      .recover { case e => Left(e.getMessage) }

  /** Each interval counted in full for its thread, so overlapping threads each get all of their time. */
  private def secondsByThread(intervals: Vector[Interval]): Map[String, Double] =
    intervals.groupMapReduce(_.threadId)(i => math.max(i.end - i.start, 0L).toDouble / 1000)(_ + _)
