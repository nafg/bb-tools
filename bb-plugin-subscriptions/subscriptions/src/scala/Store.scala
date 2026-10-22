package subscriptions

import bbplugin.*
import subscriptionscore.*

import scala.scalajs.js

/** One thread's subscription to a pull or merge request, with what the plugin last told it. */
final case class Subscription(
    id: Int,
    threadId: String,
    changeRequest: ChangeRequest,
    lastSeen: Snapshot,
    /** Set while checking fails; cleared when a check succeeds again. */
    checkError: Option[String]
)

final class Store(db: SqliteDb):
  /** Left when the row can't be read back, with the row id and the reason. */
  private def toSubscription(row: js.Dynamic): Either[String, Subscription] =
    val id = row.id.asInstanceOf[Double].toInt
    scala.util.Try {
      Subscription(
        id = id,
        threadId = row.thread_id.asInstanceOf[String],
        changeRequest = ChangeRequest.parse(row.url.asInstanceOf[String]).fold(e => throw IllegalStateException(e), identity),
        lastSeen = Snapshot(
          Lifecycle.valueOf(row.lifecycle.asInstanceOf[String]),
          CiResult.valueOf(row.ci.asInstanceOf[String]),
          row.head_sha.asInstanceOf[String]
        ),
        checkError = Option(row.check_error.asInstanceOf[String | Null]).map(_.nn)
      )
    }.toEither.left.map(e => s"subscription #$id is unreadable: ${e.getMessage}")

  /** Every active subscription, plus a description of each row that couldn't be read. */
  def activeWithProblems(): (Seq[Subscription], Seq[String]) =
    val rows = db.prepare("SELECT * FROM subscriptions WHERE ended_at IS NULL ORDER BY id").all().toSeq
    val read = rows.map(toSubscription)
    (read.collect { case Right(s) => s }, read.collect { case Left(e) => e })

  def active(): Seq[Subscription] = activeWithProblems()._1

  def activeFor(threadId: String): Seq[Subscription] =
    db.prepare("SELECT * FROM subscriptions WHERE ended_at IS NULL AND thread_id = ? ORDER BY id")
      .all(threadId).toSeq.map(toSubscription).collect { case Right(s) => s }

  def find(threadId: String, cr: ChangeRequest): Option[Subscription] =
    activeFor(threadId).find(_.changeRequest == cr)

  def get(id: Int): Option[Subscription] =
    db.prepare("SELECT * FROM subscriptions WHERE id = ? AND ended_at IS NULL").get(id).toOption
      .flatMap(toSubscription(_).toOption)

  def add(threadId: String, cr: ChangeRequest, seen: Snapshot): Int =
    val result = db
      .prepare("INSERT INTO subscriptions (thread_id, url, lifecycle, ci, head_sha) VALUES (?, ?, ?, ?, ?)")
      .run(threadId, cr.url, seen.lifecycle.toString, seen.ci.toString, seen.headSha)
    result.lastInsertRowid.asInstanceOf[Double].toInt

  def recordSeen(id: Int, seen: Snapshot, ended: Boolean): Unit =
    val _ = db
      .prepare(
        """UPDATE subscriptions SET lifecycle = ?, ci = ?, head_sha = ?, check_error = NULL,
          |  ended_at = CASE WHEN ? THEN unixepoch() ELSE NULL END WHERE id = ?""".stripMargin
      )
      .run(seen.lifecycle.toString, seen.ci.toString, seen.headSha, if ended then 1 else 0, id)

  def recordCheckError(id: Int, error: String): Unit =
    val _ = db.prepare("UPDATE subscriptions SET check_error = ? WHERE id = ?").run(error, id)

  def end(id: Int): Unit =
    val _ = db.prepare("UPDATE subscriptions SET ended_at = unixepoch() WHERE id = ?").run(id)

  def endAllFor(threadId: String): Unit =
    val _ = db.prepare("UPDATE subscriptions SET ended_at = unixepoch() WHERE thread_id = ? AND ended_at IS NULL")
      .run(threadId)

object Store:
  val migrations: js.Array[String] = js.Array(
    """CREATE TABLE IF NOT EXISTS subscriptions (
      |  id INTEGER PRIMARY KEY,
      |  thread_id TEXT NOT NULL,
      |  url TEXT NOT NULL,
      |  lifecycle TEXT NOT NULL,
      |  ci TEXT NOT NULL,
      |  head_sha TEXT NOT NULL,
      |  check_error TEXT,
      |  created_at INTEGER NOT NULL DEFAULT (unixepoch()),
      |  ended_at INTEGER
      |)""".stripMargin
  )
