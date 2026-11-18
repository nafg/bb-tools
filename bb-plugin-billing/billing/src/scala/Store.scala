package billing

import bbplugin.Dyn.{number, str}
import bbplugin.SqliteDb
import billingcore.{Allocation, BlockKey, Coverage, Span, SubmissionEntry, WireJs, ZohoTarget}

import scala.scalajs.js

/** A submission as stored: the stretches of one thread (and of the subthreads it covered) sent to Zoho, or deliberately
  * not billed, in one go. The spans are the state: a stretch is billed when a submission's span covers it.
  */
final case class Submission(
  id: String,
  rootThreadId: String,
  spans: Vector[Span],
  billed: Boolean,
  target: ZohoTarget,
  createdAt: Double)

/** The plugin's own tables: submissions with their spans and entries, shared-time decisions, excluded windows, cached
  * window summaries, and the notes the user edited before sending.
  */
final class Store(db: SqliteDb):
  def decisions(): Map[BlockKey, Allocation] =
    db.prepare("SELECT block_key, allocation FROM decisions").all().iterator.flatMap { row =>
      for key <- BlockKey.parse(str(row.block_key)); alloc <- AllocationJson.parse(str(row.allocation))
      yield key -> alloc
    }.toMap

  def saveDecision(key: BlockKey, allocation: Allocation): Unit =
    val _ = db
      .prepare(
        """INSERT INTO decisions (block_key, day, allocation, decided_at) VALUES (?, ?, ?, ?)
          |ON CONFLICT (block_key) DO UPDATE SET allocation = excluded.allocation, decided_at = excluded.decided_at""".stripMargin
      )
      .run(key.text, key.day, AllocationJson.write(allocation), js.Date.now())

  def deleteDecision(key: BlockKey): Unit =
    val _ = db.prepare("DELETE FROM decisions WHERE block_key = ?").run(key.text)

  /** Everything every submission covers, per thread: the whole of what is billed or settled. */
  def coverage(): Coverage =
    Coverage.of(
      db.prepare("SELECT thread_id, start, end FROM submission_spans").all().iterator.map(row =>
        Span(
          str(row.thread_id),
          str(row.start).toDoubleOption.fold(0L)(_.toLong),
          str(row.end).toDoubleOption.fold(0L)(_.toLong)
        )
      ).toVector
    )

  def excludedWindows(): Set[String] =
    db.prepare("SELECT window_key FROM excluded_windows").all().iterator.map(row => str(row.window_key)).toSet

  def setExcluded(windowKey: String, threadId: String, excluded: Boolean): Unit =
    if excluded then
      val _ =
        db.prepare("INSERT OR IGNORE INTO excluded_windows (window_key, thread_id, excluded_at) VALUES (?, ?, ?)").run(
          windowKey,
          threadId,
          js.Date.now()
        )
    else
      val _ = db.prepare("DELETE FROM excluded_windows WHERE window_key = ?").run(windowKey)

  def windowSummaries(keys: Seq[String]): Map[String, String] =
    keys.iterator.flatMap { key =>
      db.prepare("SELECT summary FROM window_summaries WHERE window_key = ?").get(key).toOption.map(row =>
        key -> str(row.summary)
      )
    }.toMap

  def saveWindowSummary(key: String, summary: String): Unit =
    val _ = db
      .prepare(
        "INSERT INTO window_summaries (window_key, summary, created_at) VALUES (?, ?, ?) ON CONFLICT (window_key) DO UPDATE SET summary = excluded.summary"
      )
      .run(key, summary, js.Date.now())

  /** A thread's conversation as [[Conversation]] last read it, in its own form, when it was read. */
  def conversationRows(threadId: String): Option[String] =
    db.prepare("SELECT rows FROM conversation_rows WHERE thread_id = ?").get(threadId).toOption.map(row => str(row.rows))

  def saveConversationRows(threadId: String, rows: String): Unit =
    val _ = db
      .prepare(
        "INSERT INTO conversation_rows (thread_id, rows, saved_at) VALUES (?, ?, ?) ON CONFLICT (thread_id) DO UPDATE SET rows = excluded.rows, saved_at = excluded.saved_at"
      )
      .run(threadId, rows, js.Date.now())

  /** The last read of a Zoho project's time entries: the days it covered, when it was made, and the entries in
    * `ZohoExistingEntry.encode`'s form.
    */
  def zohoEntryRead(projectId: String): Option[(String, String, Double, String)] =
    db.prepare("SELECT from_day, to_day, read_at, entries FROM zoho_entry_reads WHERE project_id = ?").get(projectId)
      .toOption.map(row => (str(row.from_day), str(row.to_day), number(row.read_at).getOrElse(0.0), str(row.entries)))

  def saveZohoEntryRead(projectId: String, from: String, to: String, at: Double, entries: String): Unit =
    val _ = db
      .prepare(
        "INSERT INTO zoho_entry_reads (project_id, from_day, to_day, read_at, entries) VALUES (?, ?, ?, ?, ?) ON CONFLICT (project_id) DO UPDATE SET from_day = excluded.from_day, to_day = excluded.to_day, read_at = excluded.read_at, entries = excluded.entries"
      )
      .run(projectId, from, to, at, entries)

  /** A day's model-written notes, keyed by the day and the exact set of stretch lines it summarized. */
  def dayNotes(key: String): Option[String] =
    db.prepare("SELECT notes FROM day_notes WHERE notes_key = ?").get(key).toOption.map(row => str(row.notes))

  def saveDayNotes(key: String, notes: String): Unit =
    val _ = db
      .prepare(
        "INSERT INTO day_notes (notes_key, notes, created_at) VALUES (?, ?, ?) ON CONFLICT (notes_key) DO UPDATE SET notes = excluded.notes"
      )
      .run(key, notes, js.Date.now())

  def notesOverrides(rootThreadId: String): Map[String, String] =
    db.prepare("SELECT day, notes FROM notes_overrides WHERE root_thread_id = ?").all(rootThreadId).iterator.map(row =>
      str(row.day) -> str(row.notes)
    ).toMap

  def setNotesOverride(rootThreadId: String, day: String, notes: Option[String]): Unit =
    notes match
      case Some(text) =>
        val _ = db
          .prepare(
            "INSERT INTO notes_overrides (root_thread_id, day, notes) VALUES (?, ?, ?) ON CONFLICT (root_thread_id, day) DO UPDATE SET notes = excluded.notes"
          )
          .run(rootThreadId, day, text)
      case None       =>
        val _ = db.prepare("DELETE FROM notes_overrides WHERE root_thread_id = ? AND day = ?").run(rootThreadId, day)

  def clearNotesOverrides(rootThreadId: String): Unit =
    val _ = db.prepare("DELETE FROM notes_overrides WHERE root_thread_id = ?").run(rootThreadId)

  def recordSubmission(s: Submission): Unit =
    s.spans.foreach(sp =>
      val _ = db
        .prepare("INSERT INTO submission_spans (submission_id, thread_id, start, end) VALUES (?, ?, ?, ?)")
        .run(s.id, sp.threadId, sp.start.toDouble, sp.end.toDouble)
    )
    val _ = db
      .prepare(
        """INSERT INTO submissions
          |  (id, root_thread_id, threads, billed, project_id, project_name, task_id, task_name, customer_id, customer_name, created_at)
          |VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""".stripMargin
      )
      .run(
        s.id,
        s.rootThreadId,
        "[]",
        if s.billed then 1 else 0,
        s.target.projectId,
        s.target.projectName,
        s.target.taskId,
        s.target.taskName,
        s.target.customerId,
        s.target.customerName,
        s.createdAt
      )

  def recordEntry(submissionId: String, e: SubmissionEntry): Unit =
    val _ = db
      .prepare(
        "INSERT INTO submission_entries (submission_id, day, time_entry_id, minutes, notes) VALUES (?, ?, ?, ?, ?) ON CONFLICT (submission_id, day) DO UPDATE SET time_entry_id = excluded.time_entry_id, minutes = excluded.minutes, notes = excluded.notes"
      )
      .run(submissionId, e.day, e.timeEntryId, e.minutes.toDouble, e.notes)

  def deleteSubmission(id: String): Unit =
    val _ = db.prepare("DELETE FROM submission_spans WHERE submission_id = ?").run(id)
    val _ = db.prepare("DELETE FROM submission_entries WHERE submission_id = ?").run(id)
    val _ = db.prepare("DELETE FROM submissions WHERE id = ?").run(id)

  def submission(id: String): Option[Submission] =
    db.prepare("SELECT * FROM submissions WHERE id = ?").get(id).toOption.map(submissionRow)

  def submissionsOf(threadId: Option[String]): Vector[Submission] =
    threadId
      .fold(db.prepare("SELECT * FROM submissions ORDER BY created_at DESC").all())(t =>
        db.prepare(
          """SELECT * FROM submissions WHERE root_thread_id = ?
            |OR id IN (SELECT submission_id FROM submission_spans WHERE thread_id = ?) ORDER BY created_at DESC""".stripMargin
        ).all(t, t)
      )
      .toVector
      .map(submissionRow)

  def entriesOf(submissionId: String): Vector[SubmissionEntry] =
    db.prepare("SELECT * FROM submission_entries WHERE submission_id = ? ORDER BY day").all(submissionId).toVector.map(
      entryRow
    )

  /** Zoho entry ids created by submissions under this design. Entries from the earlier day-based sends are left out, so
    * they show as already in Zoho: no submission span records them.
    */
  def allEntryIds(): Set[String] =
    db.prepare("SELECT time_entry_id FROM submission_entries WHERE submission_id NOT LIKE 'day-%'").all().iterator.map(
      row => str(row.time_entry_id)
    ).toSet

  /** Entries sent for a customer in a day range, with the submission each belongs to. */
  def entriesBetween(customerId: String, fromDay: String, toDay: String): Vector[(Submission, SubmissionEntry)] =
    db.prepare(
      """SELECT s.*, e.day AS e_day, e.time_entry_id AS e_id, e.minutes AS e_minutes, e.notes AS e_notes
        |FROM submission_entries e JOIN submissions s ON s.id = e.submission_id
        |WHERE s.customer_id = ? AND e.day >= ? AND e.day <= ? ORDER BY e.day""".stripMargin
    ).all(customerId, fromDay, toDay).toVector.map { row =>
      submissionRow(row) -> SubmissionEntry(
        str(row.e_day),
        str(row.e_id),
        str(row.e_minutes).toDoubleOption.fold(0L)(_.toLong),
        str(row.e_notes)
      )
    }

  private def submissionRow(row: js.Dynamic): Submission =
    val id    = str(row.id)
    val spans =
      db.prepare("SELECT thread_id, start, end FROM submission_spans WHERE submission_id = ?").all(id).toVector
        .map(sp =>
          Span(
            str(sp.thread_id),
            str(sp.start).toDoubleOption.fold(0L)(_.toLong),
            str(sp.end).toDoubleOption.fold(0L)(_.toLong)
          )
        )
    Submission(
      id = id,
      rootThreadId = str(row.root_thread_id),
      spans = spans,
      billed = str(row.billed) == "1",
      target = ZohoTarget(
        str(row.project_id),
        str(row.project_name),
        str(row.task_id),
        str(row.task_name),
        str(row.customer_id),
        str(row.customer_name)
      ),
      createdAt = str(row.created_at).toDoubleOption.getOrElse(0.0)
    )

  private def entryRow(row: js.Dynamic): SubmissionEntry =
    SubmissionEntry(
      str(row.day),
      str(row.time_entry_id),
      str(row.minutes).toDoubleOption.fold(0L)(_.toLong),
      str(row.notes)
    )

object Store:
  /** Append-only: statement index is the migration id. The first four shipped with the day-based design; the rest move
    * it to submissions and carry the entries already sent over.
    */
  val migrations: js.Array[String] = js.Array(
    """CREATE TABLE IF NOT EXISTS decisions (
      |  block_key TEXT PRIMARY KEY,
      |  day TEXT NOT NULL,
      |  allocation TEXT NOT NULL,
      |  decided_at REAL NOT NULL
      |)""".stripMargin,
    "CREATE INDEX IF NOT EXISTS decisions_day ON decisions (day)",
    """CREATE TABLE IF NOT EXISTS pushed_entries (
      |  thread_id TEXT NOT NULL,
      |  day TEXT NOT NULL,
      |  time_entry_id TEXT NOT NULL,
      |  project_id TEXT NOT NULL,
      |  project_name TEXT NOT NULL,
      |  task_id TEXT NOT NULL,
      |  task_name TEXT NOT NULL,
      |  customer_id TEXT NOT NULL,
      |  customer_name TEXT NOT NULL,
      |  minutes REAL NOT NULL,
      |  notes TEXT NOT NULL,
      |  pushed_at REAL NOT NULL,
      |  PRIMARY KEY (thread_id, day)
      |)""".stripMargin,
    "CREATE INDEX IF NOT EXISTS pushed_entries_customer_day ON pushed_entries (customer_id, day)",
    """CREATE TABLE IF NOT EXISTS watermarks (
      |  thread_id TEXT PRIMARY KEY,
      |  billed_until REAL NOT NULL,
      |  updated_at REAL NOT NULL
      |)""".stripMargin,
    """CREATE TABLE IF NOT EXISTS submissions (
      |  id TEXT PRIMARY KEY,
      |  root_thread_id TEXT NOT NULL,
      |  threads TEXT NOT NULL,
      |  until REAL NOT NULL,
      |  billed INTEGER NOT NULL,
      |  project_id TEXT NOT NULL,
      |  project_name TEXT NOT NULL,
      |  task_id TEXT NOT NULL,
      |  task_name TEXT NOT NULL,
      |  customer_id TEXT NOT NULL,
      |  customer_name TEXT NOT NULL,
      |  created_at REAL NOT NULL
      |)""".stripMargin,
    "CREATE INDEX IF NOT EXISTS submissions_customer ON submissions (customer_id)",
    """CREATE TABLE IF NOT EXISTS submission_entries (
      |  submission_id TEXT NOT NULL,
      |  day TEXT NOT NULL,
      |  time_entry_id TEXT NOT NULL,
      |  minutes REAL NOT NULL,
      |  notes TEXT NOT NULL,
      |  PRIMARY KEY (submission_id, day)
      |)""".stripMargin,
    "CREATE TABLE IF NOT EXISTS excluded_windows (window_key TEXT PRIMARY KEY, thread_id TEXT NOT NULL, excluded_at REAL NOT NULL)",
    "CREATE TABLE IF NOT EXISTS window_summaries (window_key TEXT PRIMARY KEY, summary TEXT NOT NULL, created_at REAL NOT NULL)",
    "CREATE TABLE IF NOT EXISTS notes_overrides (root_thread_id TEXT NOT NULL, day TEXT NOT NULL, notes TEXT NOT NULL, PRIMARY KEY (root_thread_id, day))",
    // Entries sent under the day-based design become one submission each, covering no stretch.
    """INSERT OR IGNORE INTO submissions (id, root_thread_id, threads, until, billed, project_id, project_name, task_id, task_name, customer_id, customer_name, created_at)
      |SELECT 'day-' || thread_id || '-' || day, thread_id, '[]', 0, 1, project_id, project_name, task_id, task_name, customer_id, customer_name, pushed_at FROM pushed_entries""".stripMargin,
    """INSERT OR IGNORE INTO submission_entries (submission_id, day, time_entry_id, minutes, notes)
      |SELECT 'day-' || thread_id || '-' || day, day, time_entry_id, minutes, notes FROM pushed_entries""".stripMargin,
    "DROP TABLE IF EXISTS pushed_entries",
    // Lines written from the user's messages alone, before the summarizer saw the whole conversation: write them anew.
    "DELETE FROM window_summaries",
    // Submissions record the spans they cover; billed is derived from them. The watermark cache goes: it could not be
    // kept right once a submission other than the latest was undone.
    """CREATE TABLE IF NOT EXISTS submission_spans (
      |  submission_id TEXT NOT NULL,
      |  thread_id TEXT NOT NULL,
      |  start REAL NOT NULL,
      |  end REAL NOT NULL
      |)""".stripMargin,
    "CREATE INDEX IF NOT EXISTS submission_spans_thread ON submission_spans (thread_id)",
    // Earlier submissions covered each thread from where the previous one ended up to the chosen moment: one span each.
    """INSERT INTO submission_spans (submission_id, thread_id, start, end)
      |SELECT s.id, json_extract(c.value, '$.threadId'), json_extract(c.value, '$.from'), s.until
      |FROM submissions s, json_each(s.threads) c WHERE s.until > 0""".stripMargin,
    "DROP TABLE IF EXISTS watermarks",
    // A day's entry notes as the model wrote them from that day's stretch lines; the key names the lines, so a change
    // in which stretches are in (a strike-out, a new line) asks for new notes.
    "CREATE TABLE IF NOT EXISTS day_notes (notes_key TEXT PRIMARY KEY, notes TEXT NOT NULL, created_at REAL NOT NULL)",
    // Lines were one short sentence; they are now a few sentences so the day notes can be accurate. Write them anew.
    "DELETE FROM window_summaries",
    // A submission is its spans; the moment the user once chose is not state.
    "ALTER TABLE submissions DROP COLUMN until",
    "CREATE TABLE IF NOT EXISTS conversation_rows (thread_id TEXT PRIMARY KEY, rows TEXT NOT NULL, saved_at REAL NOT NULL)",
    "CREATE TABLE IF NOT EXISTS zoho_entry_reads (project_id TEXT PRIMARY KEY, from_day TEXT NOT NULL, to_day TEXT NOT NULL, read_at REAL NOT NULL, entries TEXT NOT NULL)"
  )

/** Allocations as stored: the wire form as a JSON string. */
object AllocationJson:
  def write(allocation: Allocation): String = js.JSON.stringify(WireJs.toJs(allocation.encode))

  def parse(text: String): Option[Allocation] =
    scala.util.Try(js.JSON.parse(text)).toOption.flatMap(v => Allocation.decode(WireJs.fromJs(v)))
