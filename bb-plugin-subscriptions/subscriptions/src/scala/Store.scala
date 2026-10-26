package subscriptions

import bbplugin.*
import subscriptionscore.*

import scala.scalajs.js

/** One thread's subscription to a target, with what the plugin last told it. */
final case class Subscription(
    id: Int,
    threadId: String,
    target: Target,
    lastSeen: WatchState,
    /** Set while checking fails; cleared when a check succeeds again. */
    checkError: Option[String]
)

/** A thread that subscribed to a target; `since` is when it last subscribed, in Unix seconds. */
final case class Subscriber(threadId: String, since: Long, active: Boolean)

final class Store(db: SqliteDb):
  /** Left when the row can't be read back, with the row id and the reason. */
  private def toSubscription(row: js.Dynamic): Either[String, Subscription] =
    val id = row.id.asInstanceOf[Double].toInt
    (for
      target <- Target.parse(row.url.asInstanceOf[String])
      state  <- WatchState.decode(row.state.asInstanceOf[String])
    yield Subscription(
      id = id,
      threadId = row.thread_id.asInstanceOf[String],
      target = target,
      lastSeen = state,
      checkError = Option(row.check_error.asInstanceOf[String | Null]).map(_.nn)
    )).left.map(e => s"subscription #$id is unreadable: $e")

  /** Every active subscription, plus a description of each row that couldn't be read. */
  def activeWithProblems(): (Seq[Subscription], Seq[String]) =
    val rows = db.prepare("SELECT * FROM subscriptions WHERE ended_at IS NULL ORDER BY id").all().toSeq
    val read = rows.map(toSubscription)
    (read.collect { case Right(s) => s }, read.collect { case Left(e) => e })

  def active(): Seq[Subscription] = activeWithProblems()._1

  def activeFor(threadId: String): Seq[Subscription] =
    db.prepare("SELECT * FROM subscriptions WHERE ended_at IS NULL AND thread_id = ? ORDER BY id")
      .all(threadId).toSeq.map(toSubscription).collect { case Right(s) => s }

  def find(threadId: String, target: Target): Option[Subscription] =
    activeFor(threadId).find(_.target == target)

  def get(id: Int): Option[Subscription] =
    db.prepare("SELECT * FROM subscriptions WHERE id = ? AND ended_at IS NULL").get(id).toOption
      .flatMap(toSubscription(_).toOption)

  /** Every thread that has subscribed to `target`, current subscriptions first, newest first within each. */
  def subscribersOf(target: Target): Seq[Subscriber] =
    db.prepare(
      """SELECT thread_id, max(created_at) AS since, max(ended_at IS NULL) AS active FROM subscriptions
        |WHERE url = ? GROUP BY thread_id ORDER BY active DESC, since DESC""".stripMargin
    ).all(target.url).toSeq.map { row =>
      Subscriber(row.thread_id.asInstanceOf[String], row.since.asInstanceOf[Double].toLong, row.active.asInstanceOf[Double] == 1)
    }

  def add(threadId: String, target: Target, seen: WatchState): Int =
    val result = db
      .prepare("INSERT INTO subscriptions (thread_id, url, state) VALUES (?, ?, ?)")
      .run(threadId, target.url, seen.encode)
    result.lastInsertRowid.asInstanceOf[Double].toInt

  def recordSeen(id: Int, seen: WatchState, ended: Boolean): Unit =
    val _ = db
      .prepare(
        """UPDATE subscriptions SET state = ?, check_error = NULL,
          |  ended_at = CASE WHEN ? THEN unixepoch() ELSE NULL END WHERE id = ?""".stripMargin
      )
      .run(seen.encode, if ended then 1 else 0, id)

  def recordCheckError(id: Int, error: String): Unit =
    val _ = db.prepare("UPDATE subscriptions SET check_error = ? WHERE id = ?").run(error, id)

  def end(id: Int): Unit =
    val _ = db.prepare("UPDATE subscriptions SET ended_at = unixepoch() WHERE id = ?").run(id)

  def endAllFor(threadId: String): Unit =
    val _ = db.prepare("UPDATE subscriptions SET ended_at = unixepoch() WHERE thread_id = ? AND ended_at IS NULL")
      .run(threadId)

object Store:
  /** Append-only: bb records each statement by position and rejects edits to applied ones. */
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
      |)""".stripMargin,
    // The per-kind columns became one encoded `state` (see WatchState.encode).
    "ALTER TABLE subscriptions ADD COLUMN state TEXT",
    "UPDATE subscriptions SET state = 'change ' || lifecycle || ' ' || ci || ' ' || head_sha",
    "ALTER TABLE subscriptions DROP COLUMN lifecycle",
    "ALTER TABLE subscriptions DROP COLUMN ci",
    "ALTER TABLE subscriptions DROP COLUMN head_sha",
    // Thread targets name their event, so watching other things about a thread stays possible.
    "UPDATE subscriptions SET url = url || '/archived' WHERE url LIKE '@thread:%' AND url NOT LIKE '%/%'"
  )
