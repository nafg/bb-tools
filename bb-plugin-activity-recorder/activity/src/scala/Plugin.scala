package activity

import bbplugin.*
import bbplugin.Dyn.{isDefined, number, str}
import activitycore.*

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

/** Thrown by an RPC handler for input the schema let through but the handler cannot use. */
private final class InvalidInput(message: String) extends Exception(message)

/** JSON Schema pieces of the published RPC contract. */
private object Schema:
  val Null: js.Dynamic = js.Dynamic.literal("type" -> "null")

  def number(description: String): js.Dynamic  = js.Dynamic.literal("type" -> "number", "description" -> description)
  def string(description: String): js.Dynamic  = js.Dynamic.literal("type" -> "string", "description" -> description)
  def boolean(description: String): js.Dynamic = js.Dynamic.literal("type" -> "boolean", "description" -> description)
  def nullable(schema: js.Dynamic): js.Dynamic = js.Dynamic.literal("anyOf" -> js.Array(schema, Null))
  def array(items: js.Dynamic, description: String): js.Dynamic =
    js.Dynamic.literal("type" -> "array", "items" -> items, "description" -> description)
  def obj(required: Seq[String], properties: (String, js.Dynamic)*): js.Dynamic =
    js.Dynamic.literal(
      "type"                 -> "object",
      "properties"           -> js.Dynamic.literal(properties*),
      "required"             -> required.toJSArray,
      "additionalProperties" -> false
    )

  val kind: js.Dynamic = js.Dynamic.literal(
    "type"        -> "string",
    "enum"        -> SignalKind.values.map(_.name).toJSArray,
    "description" -> "A signal kind"
  )
  val kinds: js.Dynamic     = array(kind, "Signal kinds to include")
  val threadIds: js.Dynamic = array(string("A thread id"), "Only these threads; every thread when absent")
  val from: js.Dynamic      = number("Window start, epoch milliseconds (inclusive)")
  val to: js.Dynamic        = number("Window end, epoch milliseconds (exclusive)")

  val signal: js.Dynamic = obj(
    Seq("id", "threadId", "kind", "start", "end", "source"),
    "id"       -> string("Upstream id of the signal, unique within its source"),
    "threadId" -> string("The thread"),
    "kind"     -> kind,
    "start"    -> number("Epoch milliseconds"),
    "end"      -> number("Epoch milliseconds; equal to start for a point signal"),
    "source"   -> string("What produced it: converse, converse-replay, converse-backfill, bb, or app")
  )
  val interval: js.Dynamic = obj(
    Seq("threadId", "start", "end", "kinds"),
    "threadId" -> string("The thread"),
    "start"    -> number("Epoch milliseconds"),
    "end"      -> number("Epoch milliseconds"),
    "kinds"    -> array(kind, "The kinds of signal the interval is made of")
  )

/** What the converse plugin feeds the recorder. */
private object Converse:
  val PluginId  = "converse"
  val Method    = "activitySpans"
  val PollMs    = 60_000
  val PageLimit = 500
  val CursorSeq = "converse:seq"
  val CursorAt  = "converse:at"

/** The bb thread events the recorder reads, and the cursor it keeps per thread. */
private object ThreadEvents:
  val Types     = js.Array("client/turn/requested", "turn/started", "turn/completed")
  val PageLimit = 100 // the most bb serves per request
  def cursor(threadId: String): String = s"thread:$threadId"

private val DefaultGapMs        = 300_000L
private val DefaultStaleAfterMs = 86_400_000L

final class Store(storage: BbStorageArea):
  private val db = storage.database()
  storage.migrate(
    db,
    js.Array(
      """CREATE TABLE IF NOT EXISTS signals (
        |  id TEXT PRIMARY KEY,
        |  thread_id TEXT NOT NULL,
        |  kind TEXT NOT NULL,
        |  start_at INTEGER NOT NULL,
        |  end_at INTEGER NOT NULL,
        |  source TEXT NOT NULL,
        |  recorded_at INTEGER NOT NULL
        |)""".stripMargin,
      "CREATE INDEX IF NOT EXISTS signals_by_thread ON signals (thread_id, start_at)",
      "CREATE INDEX IF NOT EXISTS signals_by_end ON signals (end_at)",
      """CREATE TABLE IF NOT EXISTS cursors (
        |  name TEXT PRIMARY KEY,
        |  value TEXT NOT NULL
        |)""".stripMargin,
      """CREATE TABLE IF NOT EXISTS open_turns (
        |  thread_id TEXT PRIMARY KEY,
        |  signal_id TEXT NOT NULL
        |)""".stripMargin,
      // Ids are unique only within their source (bb event ids, converse span
      // ids, frontend UUIDs), so the key is the pair.
      """CREATE TABLE signals_by_source (
        |  source TEXT NOT NULL,
        |  id TEXT NOT NULL,
        |  thread_id TEXT NOT NULL,
        |  kind TEXT NOT NULL,
        |  start_at INTEGER NOT NULL,
        |  end_at INTEGER NOT NULL,
        |  recorded_at INTEGER NOT NULL,
        |  PRIMARY KEY (source, id)
        |)""".stripMargin,
      """INSERT INTO signals_by_source (source, id, thread_id, kind, start_at, end_at, recorded_at)
        |SELECT source, id, thread_id, kind, start_at, end_at, recorded_at FROM signals""".stripMargin,
      "DROP TABLE signals",
      "ALTER TABLE signals_by_source RENAME TO signals",
      "CREATE INDEX IF NOT EXISTS signals_by_thread ON signals (thread_id, start_at)",
      "CREATE INDEX IF NOT EXISTS signals_by_end ON signals (end_at)"
    )
  )

  /** Inserts the signal, or moves the existing one's end to the given end. */
  def upsert(signal: Signal): Unit =
    val _ = db
      .prepare(
        "INSERT INTO signals (source, id, thread_id, kind, start_at, end_at, recorded_at) VALUES (?, ?, ?, ?, ?, ?, ?) " +
          "ON CONFLICT(source, id) DO UPDATE SET end_at = excluded.end_at"
      )
      .run(signal.source, signal.id, signal.threadId, signal.kind.name, signal.start.toDouble, signal.end.toDouble, js.Date.now())

  def setEnd(source: String, id: String, end: Long): Unit =
    val _ = db.prepare("UPDATE signals SET end_at = ? WHERE source = ? AND id = ?").run(end.toDouble, source, id)

  /** Signals of the given kinds reaching into `[from, to)`: starting before
    * `to`, and ending after `from` or starting at or after it. By thread, then start.
    */
  def signals(from: Long, to: Long, threadIds: Option[Seq[String]], kinds: Set[SignalKind]): Vector[Signal] =
    if kinds.isEmpty || threadIds.exists(_.isEmpty) then Vector.empty
    else
      val kindMarks    = kinds.toSeq.map(_ => "?").mkString(", ")
      val threadClause = threadIds.fold("")(ids => s" AND thread_id IN (${ids.map(_ => "?").mkString(", ")})")
      val params: Seq[js.Any] =
        Seq[js.Any](to.toDouble, from.toDouble, from.toDouble) ++ kinds.toSeq.map(_.name: js.Any) ++
          threadIds.getOrElse(Nil).map(id => id: js.Any)
      db.prepare(
        "SELECT source, id, thread_id, kind, start_at, end_at FROM signals " +
          s"WHERE start_at < ? AND (end_at > ? OR start_at >= ?) AND kind IN ($kindMarks)$threadClause " +
          "ORDER BY thread_id, start_at"
      ).all(params*)
        .toVector
        .flatMap { row =>
          SignalKind.parse(str(row.kind)).map { kind =>
            Signal(
              str(row.id),
              str(row.thread_id),
              kind,
              row.start_at.asInstanceOf[Double].toLong,
              row.end_at.asInstanceOf[Double].toLong,
              str(row.source)
            )
          }
        }

  def cursor(name: String): Option[String] =
    db.prepare("SELECT value FROM cursors WHERE name = ?").get(name).toOption.map(row => str(row.value))

  def setCursor(name: String, value: String): Unit =
    val _ = db
      .prepare("INSERT INTO cursors (name, value) VALUES (?, ?) ON CONFLICT(name) DO UPDATE SET value = excluded.value")
      .run(name, value)

  def openTurn(threadId: String): Option[String] =
    db.prepare("SELECT signal_id FROM open_turns WHERE thread_id = ?").get(threadId).toOption.map(row => str(row.signal_id))

  def setOpenTurn(threadId: String, signalId: String): Unit =
    val _ = db
      .prepare("INSERT INTO open_turns (thread_id, signal_id) VALUES (?, ?) ON CONFLICT(thread_id) DO UPDATE SET signal_id = excluded.signal_id")
      .run(threadId, signalId)

  def clearOpenTurn(threadId: String): Unit =
    val _ = db.prepare("DELETE FROM open_turns WHERE thread_id = ?").run(threadId)

/** Turns a thread's bb events into `message` and `turn` signals, reading each
  * thread's events once in order past a stored cursor.
  */
final class ThreadIngest(bb: BbApi, store: Store):
  private val Source = "bb"

  private val reads = OneInFlight[String](
    readPages,
    (threadId, e) => bb.log.warn(s"could not read events of thread $threadId: ${e.getMessage}")
  )

  /** Reads the thread's events past its cursor. One read per thread at a
    * time; a request during a read queues exactly one more.
    */
  def sync(threadId: String): Future[Unit] = reads.request(threadId)

  private def readPages(threadId: String): Future[Unit] =
    val request = js.Dynamic.literal(
      "threadId" -> threadId,
      "types"    -> ThreadEvents.Types,
      "order"    -> "asc",
      "limit"    -> ThreadEvents.PageLimit.toString
    )
    store.cursor(ThreadEvents.cursor(threadId)).foreach(seq => request.updateDynamic("afterSeq")(seq))
    bb.sdk.threads.events.list(request).toFuture.flatMap { rows =>
      rows.foreach(record)
      rows.lastOption.foreach(row => store.setCursor(ThreadEvents.cursor(threadId), str(row.seq)))
      if rows.length >= ThreadEvents.PageLimit then readPages(threadId) else Future.unit
    }

  private def record(row: js.Dynamic): Unit =
    val threadId = str(row.threadId)
    val at       = number(row.createdAt).map(_.toLong)
    (str(row.`type`), at) match
      case ("client/turn/requested", Some(at)) if isUserMessage(row.data) =>
        store.upsert(Signal(str(row.id), threadId, SignalKind.Message, at, at, Source))
      case ("turn/started", Some(at)) =>
        store.upsert(Signal(str(row.id), threadId, SignalKind.Turn, at, at, Source))
        store.setOpenTurn(threadId, str(row.id))
      case ("turn/completed", Some(at)) =>
        store.openTurn(threadId).foreach { signalId =>
          store.setEnd(Source, signalId, at)
          store.clearOpenTurn(threadId)
        }
      case _ => ()

  /** A message the user wrote: not one an agent, bb, or an automation sent.
    * Automations arrive tagged as the user, so their "[bb " prefix is the
    * only sign of them.
    */
  private def isUserMessage(data: js.Dynamic): Boolean =
    str(data.initiator) == "user" && str(data.senderThreadId).isEmpty && !text(data).startsWith("[bb ")

  private def text(data: js.Dynamic): String =
    data.input.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].fold("") { blocks =>
      blocks.iterator.filter(block => str(block.`type`) == "text").map(block => str(block.text)).mkString
    }

  /** Reads every thread, hidden and archived ones included, so events from
    * before the plugin was loaded are recorded too. Threads are read one at a
    * time.
    */
  def reconcileAll(): Future[Unit] =
    def page(archived: Boolean, offset: Int): Future[Unit] =
      bb.sdk.threads
        .list(js.Dynamic.literal("archived" -> archived, "includeHidden" -> true, "limit" -> 200, "offset" -> offset))
        .toFuture
        .flatMap { threads =>
          threads.foldLeft(Future.unit)((prior, thread) => prior.flatMap(_ => sync(str(thread.id)))).flatMap { _ =>
            if threads.length >= 200 then page(archived, offset + 200) else Future.unit
          }
        }
    page(archived = false, 0).flatMap(_ => page(archived = true, 0))

/** Copies converse's speech and playback spans into the store, by converse's
  * append-only sequence. Converse may be absent or not yet serving the
  * method; then `available` is false and `error` says why.
  */
final class ConverseSync(bb: BbApi, store: Store):
  var available: Boolean         = false
  var error: Option[String]      = None
  var lastSyncAt: Option[Double] = None
  /** Sequences converse expired before they were pulled, from the last sync that said. */
  var lost: Option[(Long, Long)] = None
  private var lostWarned: Option[(Long, Long)] = None

  def cursorSeq: Option[Long] = store.cursor(Converse.CursorSeq).flatMap(_.toLongOption)
  def cursorAt: Option[Long]  = store.cursor(Converse.CursorAt).flatMap(_.toLongOption)

  def syncOnce(): Future[Unit] =
    Future
      .delegate(readPage())
      .map { _ =>
        available = true
        error = None
        lastSyncAt = Some(js.Date.now())
      }
      .recover { case e =>
        available = false
        error = Some(Option(e.getMessage).getOrElse(e.toString))
      }

  private def readPage(): Future[Unit] =
    val input = js.Dynamic.literal("afterSeq" -> cursorSeq.getOrElse(0L).toDouble, "limit" -> Converse.PageLimit)
    bb.sdk.plugins
      .callRpc(
        js.Dynamic.literal(
          "pluginId"     -> Converse.PluginId,
          "method"       -> Converse.Method,
          "input"        -> input,
          "outputSchema" -> js.Dynamic.literal("parse" -> js.Any.fromFunction1((value: js.Any) => value))
        )
      )
      .toFuture
      .flatMap { output =>
        val items = output.items.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].fold(Vector.empty[js.Dynamic])(_.toVector)
        noteLost(ConverseSpans.lost(cursorSeq, number(output.oldestSeq).map(_.toLong)))
        val page  = ConverseSpans.fold(items.flatMap(decode))
        page.signals.foreach(store.upsert)
        page.lastSeq.foreach(seq => store.setCursor(Converse.CursorSeq, seq.toString))
        page.newestEnd.filter(end => cursorAt.forall(_ < end)).foreach(end => store.setCursor(Converse.CursorAt, end.toString))
        // A full page without a usable sequence cannot advance, so stop rather than loop.
        if items.length >= Converse.PageLimit && page.lastSeq.isDefined then readPage() else Future.unit
      }

  /** Converse may not say what it still holds; then the last answer stands. */
  private def noteLost(range: Option[(Long, Long)]): Unit =
    range.foreach { (from, to) =>
      lost = Some((from, to))
      if !lostWarned.contains((from, to)) then
        lostWarned = lost
        bb.log.warn(s"converse expired spans $from to $to before they were pulled; that speech and playback time is lost")
    }

  private def decode(item: js.Dynamic): Option[ConverseSpan] =
    for
      seq   <- number(item.seq)
      start <- number(item.start)
      end   <- number(item.end)
    yield ConverseSpan(seq.toLong, str(item.id), str(item.threadId), str(item.kind), str(item.source), start.toLong, end.toLong)

/** Resolves after `ms`, or as soon as `signal` aborts. */
private def sleep(ms: Int, signal: js.Dynamic): Future[Unit] =
  val done = Promise[Unit]()
  if signal.aborted.asInstanceOf[Boolean] then done.success(())
  else
    val handle = js.timers.setTimeout(ms.toDouble)(done.trySuccess(()): Unit)
    val _ = signal.addEventListener("abort", js.Any.fromFunction0 { () =>
      js.timers.clearTimeout(handle)
      done.trySuccess(()): Unit
    })
  done.future

private def untilAborted(signal: js.Dynamic): Future[Unit] =
  val done = Promise[Unit]()
  if signal.aborted.asInstanceOf[Boolean] then done.success(()): Unit
  else
    val _ = signal.addEventListener("abort", js.Any.fromFunction0(() => done.trySuccess(()): Unit))
  done.future

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit =
  val store    = Store(bb.storage)
  val threads  = ThreadIngest(bb, store)
  val converse = ConverseSync(bb, store)

  val settings = bb.settings.define(
    js.Dynamic.literal(
      "gapMs" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "Gap: signals on a thread no further apart than this join into one interval (milliseconds)",
        "default" -> DefaultGapMs.toString
      ),
      "staleAfterMs" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "Converse: report the sync as stale when its newest span is older than this (milliseconds)",
        "default" -> DefaultStaleAfterMs.toString
      )
    )
  )

  final case class Settings(gapMs: Long, staleAfterMs: Long)
  def currentSettings: Future[Settings] =
    settings.get().toFuture.map { s =>
      Settings(
        str(s.gapMs).toLongOption.filter(_ >= 0).getOrElse(DefaultGapMs),
        str(s.staleAfterMs).toLongOption.filter(_ > 0).getOrElse(DefaultStaleAfterMs)
      )
    }

  /** Stale means the newest span synced is older than the setting. Before
    * the first successful sync, or when converse has no spans, nothing is
    * stale yet.
    */
  def converseStale(staleAfterMs: Long): Boolean =
    converse.lastSyncAt.isDefined && converse.cursorAt.exists(at => js.Date.now() - at > staleAfterMs)

  // ------------------------------------------------------------ ingestion

  val unsubscribe = bb.sdk.subscribe(
    js.Dynamic.literal(
      "event" -> "thread:changed",
      "callback" -> js.Any.fromFunction1 { (change: js.Dynamic) =>
        val threadId = str(change.id)
        val changes  = change.changes.asInstanceOf[js.UndefOr[js.Array[String]]].fold(Set.empty[String])(_.toSet)
        val eventTypes = change.metadata
          .asInstanceOf[js.UndefOr[js.Dynamic]]
          .flatMap(metadata => metadata.eventTypes.asInstanceOf[js.UndefOr[js.Array[String]]])
          .map(_.toSet)
        val relevant = eventTypes.fold(changes.contains("events-appended"))(_.exists(ThreadEvents.Types.contains))
        if threadId.nonEmpty && relevant then threads.sync(threadId): Unit
      }
    )
  )
  bb.onDispose(() => unsubscribe())

  bb.background.service(
    "thread-events",
    js.Dynamic.literal(
      "start" -> js.Any.fromFunction1 { (signal: js.Dynamic) =>
        threads
          .reconcileAll()
          .recover { case e => bb.log.warn(s"reconciling thread events failed: ${e.getMessage}") }
          .flatMap(_ => untilAborted(signal))
          .toJSPromise
      }
    )
  )

  bb.background.service(
    "converse-sync",
    js.Dynamic.literal(
      "start" -> js.Any.fromFunction1 { (signal: js.Dynamic) =>
        var staleWarned = false
        def loop(): Future[Unit] =
          if signal.aborted.asInstanceOf[Boolean] then Future.unit
          else
            converse
              .syncOnce()
              .flatMap { _ =>
                currentSettings.map { s =>
                  val stale = converse.available && converseStale(s.staleAfterMs)
                  if stale && !staleWarned then
                    bb.log.warn(s"converse sync is stale: newest span synced ended at ${converse.cursorAt.getOrElse(0L)}")
                  staleWarned = stale
                }
              }
              .recover { case _ => () }
              .flatMap(_ => sleep(Converse.PollMs, signal))
              .flatMap(_ => loop())
        loop().toJSPromise
      }
    )
  )

  // ------------------------------------------------------------------ RPC

  def window(input: js.Dynamic): (Long, Long) =
    val from = number(input.from).getOrElse(throw InvalidInput("from must be a number"))
    val to   = number(input.to).getOrElse(throw InvalidInput("to must be a number"))
    if to < from then throw InvalidInput("to must not be before from")
    (from.toLong, to.toLong)

  def threadIdsOf(input: js.Dynamic): Option[Seq[String]] =
    if !isDefined(input.threadIds) then None
    else if !js.Array.isArray(input.threadIds) then throw InvalidInput("threadIds must be an array of strings")
    else Some(input.threadIds.asInstanceOf[js.Array[Any]].toSeq.map(_.toString))

  def kindsOf(input: js.Dynamic, default: Set[SignalKind]): Set[SignalKind] =
    if !isDefined(input.kinds) then default
    else if !js.Array.isArray(input.kinds) then throw InvalidInput("kinds must be an array of signal kind names")
    else
      input.kinds.asInstanceOf[js.Array[Any]].toSeq.map { name =>
        SignalKind.parse(name.toString).getOrElse(throw InvalidInput(s"unknown signal kind: $name"))
      }.toSet

  def gapMsOf(input: js.Dynamic): Option[Long] =
    if !isDefined(input.gapMs) then None
    else
      number(input.gapMs).filter(_ >= 0).map(_.toLong).orElse(throw InvalidInput("gapMs must be a non-negative number"))

  def objectInput(input: js.Dynamic): js.Dynamic =
    if input == null || js.typeOf(input) != "object" then throw InvalidInput("expected an object") else input

  /** Signals one more than the gap outside the window can still join one
    * inside it, so the query reaches that far.
    */
  def intervalsWithin(from: Long, to: Long, threadIds: Option[Seq[String]], kinds: Set[SignalKind], gap: Long): Vector[Interval] =
    Intervals.derive(store.signals(from - gap - 1, to + gap + 1, threadIds, kinds), gap, from, to)

  def intervals(rawInput: js.Dynamic): Future[js.Any] =
    val input      = objectInput(rawInput)
    val (from, to) = window(input)
    val threadIds  = threadIdsOf(input)
    val kinds      = kindsOf(input, SignalKind.Attention)
    val gapMs      = gapMsOf(input)
    currentSettings.map { s =>
      val derived = intervalsWithin(from, to, threadIds, kinds, gapMs.getOrElse(s.gapMs))
      js.Dynamic.literal("intervals" -> derived.map(ActivityJson.interval).toJSArray)
    }

  def signals(rawInput: js.Dynamic): js.Any =
    val input      = objectInput(rawInput)
    val (from, to) = window(input)
    val rows       = store.signals(from, to, threadIdsOf(input), kindsOf(input, SignalKind.values.toSet))
    js.Dynamic.literal("signals" -> rows.map(ActivityJson.signal).toJSArray)

  def status(): Future[js.Any] =
    currentSettings.map { s =>
      js.Dynamic.literal(
        "gapMs"        -> s.gapMs.toDouble,
        "staleAfterMs" -> s.staleAfterMs.toDouble,
        "sources" -> js.Dynamic.literal(
          "converse" -> js.Dynamic.literal(
            "available"  -> converse.available,
            "cursorSeq"  -> converse.cursorSeq.map(_.toDouble).map(js.Any.fromDouble).orNull,
            "cursorAt"   -> converse.cursorAt.map(_.toDouble).map(js.Any.fromDouble).orNull,
            "lastSyncAt" -> converse.lastSyncAt.map(js.Any.fromDouble).orNull,
            "stale"      -> converseStale(s.staleAfterMs),
            "lost"       -> converse.lost.isDefined,
            "lostSeqs" -> converse.lost.fold[js.Any](null) { (from, to) =>
              js.Dynamic.literal("from" -> from.toDouble, "to" -> to.toDouble)
            },
            "error"      -> converse.error.orNull
          )
        )
      )
    }

  /** The frontend's `view` span: reported when it opens, every so often while
    * it stays open, and when it closes. Each report sets the span's end.
    */
  def reportView(rawInput: js.Dynamic): js.Any =
    val input    = objectInput(rawInput)
    val id       = str(input.id)
    val threadId = str(input.threadId)
    if id.isEmpty || id.length > 100 then throw InvalidInput("id must be a string of at most 100 characters")
    if threadId.isEmpty || threadId.length > 100 then throw InvalidInput("threadId must be a non-empty string")
    val (start, end) = (number(input.start), number(input.end)) match
      case (Some(start), Some(end)) if end >= start => (start.toLong, end.toLong)
      case _                                        => throw InvalidInput("start and end must be numbers with end >= start")
    store.upsert(Signal(id, threadId, SignalKind.View, start, end, "app"))
    js.Dynamic.literal("recorded" -> true)

  def method(description: String, input: js.Dynamic, output: js.Dynamic): js.Dynamic =
    js.Dynamic.literal(
      "input"                    -> StandardSchema.describedBy(input),
      "output"                   -> StandardSchema.describedBy(output),
      "experimental_description" -> description
    )

  bb.rpc.register(
    js.Dynamic.literal(
      ActivityRecorderRpc.Intervals -> method(
        "Per thread, signals of the requested kinds (default: speech, playback, message, view) merged by the gap rule " +
          "and clipped to [from, to). Intervals of different threads overlap freely.",
        Schema.obj(
          Seq("from", "to"),
          "from"      -> Schema.from,
          "to"        -> Schema.to,
          "threadIds" -> Schema.threadIds,
          "kinds"     -> Schema.kinds,
          "gapMs"     -> Schema.number("Gap threshold in milliseconds; the plugin setting when absent")
        ),
        Schema.obj(Seq("intervals"), "intervals" -> Schema.array(Schema.interval, "By thread id, then start"))
      ),
      ActivityRecorderRpc.Signals -> method(
        "The raw signals of the requested kinds (default: every kind) reaching into [from, to).",
        Schema.obj(
          Seq("from", "to"),
          "from"      -> Schema.from,
          "to"        -> Schema.to,
          "threadIds" -> Schema.threadIds,
          "kinds"     -> Schema.kinds
        ),
        Schema.obj(Seq("signals"), "signals" -> Schema.array(Schema.signal, "By thread id, then start"))
      ),
      ActivityRecorderRpc.Status -> method(
        "The gap setting and the state of each signal source.",
        Schema.Null,
        Schema.obj(
          Seq("gapMs", "staleAfterMs", "sources"),
          "gapMs"        -> Schema.number("Gap threshold in milliseconds"),
          "staleAfterMs" -> Schema.number("Age of the newest converse span past which the sync is stale"),
          "sources" -> Schema.obj(
            Seq("converse"),
            "converse" -> Schema.obj(
              Seq("available", "cursorSeq", "cursorAt", "lastSyncAt", "stale", "lost", "lostSeqs", "error"),
              "available"  -> Schema.boolean("The last pull from converse succeeded"),
              "cursorSeq"  -> Schema.nullable(Schema.number("Converse's sequence of the newest span synced")),
              "cursorAt"   -> Schema.nullable(Schema.number("End time of the newest span synced, epoch milliseconds")),
              "lastSyncAt" -> Schema.nullable(Schema.number("When the last successful pull finished, epoch milliseconds")),
              "stale"      -> Schema.boolean("A sync has succeeded and cursorAt is older than staleAfterMs"),
              "lost"       -> Schema.boolean("Converse expired spans the recorder never pulled; lostSeqs says which"),
              "lostSeqs" -> Schema.nullable(
                Schema.obj(
                  Seq("from", "to"),
                  "from" -> Schema.number("First converse sequence lost"),
                  "to"   -> Schema.number("Last converse sequence lost")
                )
              ),
              "error"      -> Schema.nullable(Schema.string("Why the last pull failed"))
            )
          )
        )
      ),
      "reportView" -> method(
        "The frontend's view span: each report sets the span's end.",
        Schema.obj(
          Seq("id", "threadId", "start", "end"),
          "id"       -> Schema.string("Client-generated id of the pane session"),
          "threadId" -> Schema.string("The thread"),
          "start"    -> Schema.number("Epoch milliseconds"),
          "end"      -> Schema.number("Epoch milliseconds")
        ),
        Schema.obj(Seq("recorded"), "recorded" -> Schema.boolean("Always true"))
      )
    ),
    js.Dynamic.literal(
      ActivityRecorderRpc.Intervals -> js.Any.fromFunction1((input: js.Dynamic) => intervals(input).toJSPromise),
      ActivityRecorderRpc.Signals   -> js.Any.fromFunction1((input: js.Dynamic) => signals(input)),
      ActivityRecorderRpc.Status    -> js.Any.fromFunction1((_: js.Dynamic) => status().toJSPromise),
      "reportView"                  -> js.Any.fromFunction1((input: js.Dynamic) => reportView(input))
    ),
    js.Dynamic.literal(
      "experimental_discoverable" -> true,
      "experimental_description"  -> "Per-thread activity intervals and the raw signals behind them; see ActivityRecorderRpc."
    )
  )
