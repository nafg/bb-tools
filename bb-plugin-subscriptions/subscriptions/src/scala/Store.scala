package subscriptions

import bbplugin.*
import subscriptionscore.ThreadArchived
import subscriptionsource.CheckResult

import scala.scalajs.js

/** One thread's subscription to a target, with what the plugin last told it.
  *
  * `source` is the plugin that checks the target (this plugin for threads), `url` the target's canonical form and
  * `state` that source's record of what it saw last, opaque here. `label` and `summary` come from the last check;
  * before the first check after they were added, `label` falls back to the URL.
  */
final case class Subscription(
    id: Int,
    threadId: String,
    source: String,
    url: String,
    label: String,
    summary: Option[String],
    state: String,
    /** Set while checking fails; cleared when a check succeeds again. */
    checkError: Option[String]
)

/** A thread that subscribed to a target; `since` is when it last subscribed, in Unix seconds. */
final case class Subscriber(threadId: String, since: Long, active: Boolean)

final class Store(db: SqliteDb):
  private def opt(v: js.Dynamic): Option[String] = Option(v.asInstanceOf[String | Null]).map(_.nn)

  private def toSubscription(row: js.Dynamic): Subscription =
    val url = row.url.asInstanceOf[String]
    Subscription(
      id = row.id.asInstanceOf[Double].toInt,
      threadId = row.thread_id.asInstanceOf[String],
      source = opt(row.source).getOrElse(""),
      url = url,
      label = opt(row.label).getOrElse(url),
      summary = opt(row.summary),
      state = opt(row.state).getOrElse(""),
      checkError = opt(row.check_error)
    )

  def active(): Seq[Subscription] =
    db.prepare("SELECT * FROM subscriptions WHERE ended_at IS NULL ORDER BY id").all().toSeq.map(toSubscription)

  def activeFor(threadId: String): Seq[Subscription] =
    db.prepare("SELECT * FROM subscriptions WHERE ended_at IS NULL AND thread_id = ? ORDER BY id")
      .all(threadId).toSeq.map(toSubscription)

  /** Every active subscription to another thread being archived, as (subscribing thread, watched thread). */
  def threadWaits(): Seq[(String, String)] =
    active().flatMap(s => ThreadArchived.parse(s.url).map(t => s.threadId -> t.threadId))

  def find(threadId: String, url: String): Option[Subscription] = activeFor(threadId).find(_.url == url)

  def get(id: Int): Option[Subscription] =
    db.prepare("SELECT * FROM subscriptions WHERE id = ? AND ended_at IS NULL").get(id).toOption.map(toSubscription)

  /** Every thread that has subscribed to `url`, current subscriptions first, newest first within each. */
  def subscribersOf(url: String): Seq[Subscriber] =
    db.prepare(
      """SELECT thread_id, max(created_at) AS since, max(ended_at IS NULL) AS active FROM subscriptions
        |WHERE url = ? GROUP BY thread_id ORDER BY active DESC, since DESC""".stripMargin
    ).all(url).toSeq.map { row =>
      Subscriber(row.thread_id.asInstanceOf[String], row.since.asInstanceOf[Double].toLong, row.active.asInstanceOf[Double] == 1)
    }

  def add(threadId: String, source: String, url: String, first: CheckResult): Int =
    val result = db
      .prepare("INSERT INTO subscriptions (thread_id, source, url, label, summary, state) VALUES (?, ?, ?, ?, ?, ?)")
      .run(threadId, source, url, first.label, first.summary, first.state)
    result.lastInsertRowid.asInstanceOf[Double].toInt

  def recordCheck(id: Int, result: CheckResult): Unit =
    val _ = db
      .prepare(
        """UPDATE subscriptions SET state = ?, label = ?, summary = ?, check_error = NULL,
          |  ended_at = CASE WHEN ? THEN unixepoch() ELSE NULL END WHERE id = ?""".stripMargin
      )
      .run(result.state, result.label, result.summary, if result.finished then 1 else 0, id)

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
    "UPDATE subscriptions SET url = url || '/archived' WHERE url LIKE '@thread:%' AND url NOT LIKE '%/%'",
    "UPDATE subscriptions SET url = replace(url, '@thread:', 'bb:thread/') WHERE url LIKE '@thread:%'",
    // Each target is now checked by a source plugin; state is that source's, opaque here.
    "ALTER TABLE subscriptions ADD COLUMN source TEXT",
    "ALTER TABLE subscriptions ADD COLUMN label TEXT",
    "ALTER TABLE subscriptions ADD COLUMN summary TEXT",
    """UPDATE subscriptions SET source = CASE
      |  WHEN url LIKE 'bb:thread/%' THEN 'subscriptions'
      |  WHEN url LIKE 'https://central.sonatype.com/%' THEN 'maven-central'
      |  WHEN url LIKE '%/-/%' THEN 'forge-gitlab'
      |  ELSE 'forge-github' END""".stripMargin,
    // Source plugins are named subscriptions-<source>.
    """UPDATE subscriptions SET source = CASE source
      |  WHEN 'forge-github' THEN 'subscriptions-github'
      |  WHEN 'forge-gitlab' THEN 'subscriptions-gitlab'
      |  WHEN 'maven-central' THEN 'subscriptions-maven-central'
      |  WHEN 'local-git' THEN 'subscriptions-local-git'
      |  ELSE source END""".stripMargin
  )
