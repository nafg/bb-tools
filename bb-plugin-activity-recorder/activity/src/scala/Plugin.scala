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
  val agentSpan: js.Dynamic = obj(
    Seq("threadId", "start", "end", "startedBy"),
    "threadId" -> string("The thread whose agent ran the turn"),
    "start"    -> number("Epoch milliseconds"),
    "end"      -> number("Epoch milliseconds; the time of the call for a turn still running"),
    "startedBy" -> js.Dynamic.literal(
      "type"        -> "object",
      "description" -> "{ kind: \"user\" }, or { kind: \"threads\", ids } when other threads started the turn"
    )
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
  val Types     = js.Array("client/turn/requested", "turn/input/accepted", "turn/started", "turn/completed")
  val PageLimit = 100 // the most bb serves per request
  // Versioned: reading turn requests and their acceptance (who started each turn) meant reading every thread again.
  def cursor(threadId: String): String = s"thread-v2:$threadId"

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
      "CREATE INDEX IF NOT EXISTS signals_by_end ON signals (end_at)",
      // A turn signal remembers its turn and who started it (see `Starter.encode`); null until the turn's input is accepted.
      "ALTER TABLE signals ADD COLUMN turn_id TEXT",
      "ALTER TABLE signals ADD COLUMN starter TEXT",
      "CREATE INDEX IF NOT EXISTS signals_by_turn ON signals (thread_id, turn_id)",
      """CREATE TABLE IF NOT EXISTS turn_requests (
        |  thread_id TEXT NOT NULL,
        |  request_id TEXT NOT NULL,
        |  starter TEXT NOT NULL,
        |  PRIMARY KEY (thread_id, request_id)
        |)""".stripMargin
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

  /** Inserts the turn's signal, or moves the existing one's end; the turn's starter is kept if already known. */
  def upsertTurn(signal: Signal, turnId: String): Unit =
    val _ = db
      .prepare(
        "INSERT INTO signals (source, id, thread_id, kind, start_at, end_at, recorded_at, turn_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?) " +
          "ON CONFLICT(source, id) DO UPDATE SET end_at = excluded.end_at, turn_id = excluded.turn_id"
      )
      .run(signal.source, signal.id, signal.threadId, signal.kind.name, signal.start.toDouble, signal.end.toDouble, js.Date.now(), turnId)

  def setRequestStarter(threadId: String, requestId: String, starter: Option[Starter]): Unit =
    val _ = db
      .prepare("INSERT INTO turn_requests (thread_id, request_id, starter) VALUES (?, ?, ?) ON CONFLICT(thread_id, request_id) DO UPDATE SET starter = excluded.starter")
      .run(threadId, requestId, Starter.encode(starter))

  def requestStarter(threadId: String, requestId: String): Option[String] =
    db.prepare("SELECT starter FROM turn_requests WHERE thread_id = ? AND request_id = ?").get(threadId, requestId).toOption.map(row => str(row.starter))

  /** Sets who started the turn, unless an earlier accepted input already did. */
  def setTurnStarter(threadId: String, turnId: String, starter: String): Unit =
    val _ = db
      .prepare("UPDATE signals SET starter = ? WHERE source = 'bb' AND kind = 'turn' AND thread_id = ? AND turn_id = ? AND starter IS NULL")
      .run(starter, threadId, turnId)

  /** The turns the user set going that reach into `[from, to)`, by thread then start. A turn still running ends at
    * `now`. A turn with no accepted input is bb's agent resuming on its own, after a background task finished, so it
    * takes the starter of the thread's nearest earlier turn that has one.
    */
  def agentSpans(from: Long, to: Long, threadIds: Option[Seq[String]], now: Long): Vector[AgentSpan] =
    if threadIds.exists(_.isEmpty) then Vector.empty
    else
      val threadClause = threadIds.fold("")(ids => s" AND s.thread_id IN (${ids.map(_ => "?").mkString(", ")})")
      val params: Seq[js.Any] = Seq[js.Any](to.toDouble) ++ threadIds.getOrElse(Nil).map(id => id: js.Any) :+ (from.toDouble: js.Any)
      db.prepare(
        "SELECT thread_id, start_at, end_at, starter, open FROM (" +
          "SELECT s.thread_id, s.start_at, s.end_at, (o.signal_id IS NOT NULL) AS open, COALESCE(s.starter, " +
          "(SELECT p.starter FROM signals p WHERE p.source = 'bb' AND p.kind = 'turn' AND p.thread_id = s.thread_id " +
          "AND p.starter IS NOT NULL AND p.start_at < s.start_at ORDER BY p.start_at DESC LIMIT 1)) AS starter " +
          "FROM signals s LEFT JOIN open_turns o ON o.thread_id = s.thread_id AND o.signal_id = s.id " +
          s"WHERE s.source = 'bb' AND s.kind = 'turn' AND s.start_at < ?$threadClause AND (o.signal_id IS NOT NULL OR s.end_at > ?)" +
          ") WHERE starter IS NOT NULL AND starter != '' ORDER BY thread_id, start_at"
      ).all(params*)
        .toVector
        .flatMap { row =>
          Starter.decode(str(row.starter)).map { starter =>
            val start = row.start_at.asInstanceOf[Double].toLong
            val end   = if row.open.asInstanceOf[Double] != 0 then math.max(now, start) else row.end_at.asInstanceOf[Double].toLong
            AgentSpan(str(row.thread_id), start, end, starter)
          }
        }

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

  /** What the recorder needs to know about a thread to tell who started its turns. */
  private final case class ThreadInfo(hidden: Boolean, parentThreadId: Option[String])

  private val infos = scala.collection.mutable.Map.empty[String, ThreadInfo]

  private def infoOf(row: js.Dynamic): ThreadInfo =
    ThreadInfo(str(row.visibility) == "hidden", Option(str(row.parentThreadId)).filter(_.nonEmpty))

  /** A thread's visibility and parent never change, so each is read once. */
  private def info(threadId: String): Future[ThreadInfo] =
    infos.get(threadId) match
      case Some(known) => Future.successful(known)
      case None        =>
        bb.sdk.threads.get(js.Dynamic.literal("threadId" -> threadId)).toFuture.map { thread =>
          val known = infoOf(if isDefined(thread.thread) then thread.thread else thread)
          infos.update(threadId, known)
          known
        }

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
    info(threadId).zip(bb.sdk.threads.events.list(request).toFuture).flatMap { (info, rows) =>
      rows.foreach(record(info, _))
      rows.lastOption.foreach(row => store.setCursor(ThreadEvents.cursor(threadId), str(row.seq)))
      if rows.length >= ThreadEvents.PageLimit then readPages(threadId) else Future.unit
    }

  private def record(info: ThreadInfo, row: js.Dynamic): Unit =
    val threadId = str(row.threadId)
    val at       = number(row.createdAt).map(_.toLong)
    (str(row.`type`), at) match
      case ("client/turn/requested", Some(at)) =>
        if isUserMessage(row.data) then store.upsert(Signal(str(row.id), threadId, SignalKind.Message, at, at, Source))
        store.setRequestStarter(threadId, str(row.data.requestId), starterOf(info, row.data))
      case ("turn/input/accepted", Some(_)) =>
        store.requestStarter(threadId, str(row.data.clientRequestId)).foreach(starter =>
          store.setTurnStarter(threadId, str(row.scope.turnId), starter)
        )
      case ("turn/started", Some(at)) =>
        store.upsertTurn(Signal(str(row.id), threadId, SignalKind.Turn, at, at, Source), str(row.scope.turnId))
        store.setOpenTurn(threadId, str(row.id))
      case ("turn/completed", Some(at)) =>
        store.openTurn(threadId).foreach { signalId =>
          store.setEnd(Source, signalId, at)
          store.clearOpenTurn(threadId)
        }
      case _ => ()

  /** Who started the turn this request begins, when it is work the user set going. A hidden thread is one of bb's
    * own workers (recap, summaries), which never counts.
    */
  private def starterOf(info: ThreadInfo, data: js.Dynamic): Option[Starter] =
    if info.hidden then None
    else
      val subject = data.systemMessageSubject
      val subjectIds =
        if !isDefined(subject) || subject == null then Set.empty[String]
        else
          val own      = Option(str(subject.threadId)).filter(_.nonEmpty).toSet
          val outcomes = subject.outcomes.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].fold(Set.empty[String])(_.iterator.map(o => str(o.threadId)).filter(_.nonEmpty).toSet)
          own ++ outcomes
      Starter.of(
        TurnRequest(
          initiator = str(data.initiator),
          senderThreadId = Option(str(data.senderThreadId)).filter(_.nonEmpty),
          systemMessageKind = str(data.systemMessageKind),
          subjectThreadIds = subjectIds,
          text = text(data),
          startsThread = isDefined(data.target) && str(data.target.kind) == "thread-start",
          parentThreadId = info.parentThreadId
        )
      )

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
          threads.foreach(thread => infos.update(str(thread.id), infoOf(thread)))
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

/** `bb activity-recorder summary` arguments. Dates are `YYYY-MM-DD` in the
  * bb server's local time; a range runs from the first day's start to the
  * day after the last.
  */
private final case class CliOptions(range: (Long, Long), json: Boolean, touched: Boolean)

private enum WeekStart(val name: String, val dayOfWeek: Int):
  case Monday extends WeekStart("monday", 1)
  case Sunday extends WeekStart("sunday", 0)

private object CliArgs:
  val Usage =
    "bb activity-recorder summary [--date YYYY-MM-DD | --week [YYYY-MM-DD] [--week-start monday|sunday] | " +
      "--from YYYY-MM-DD [--to YYYY-MM-DD]] [--touched] [--json]"

  def failure(message: String): js.Dynamic = js.Dynamic.literal("exitCode" -> 1, "stderr" -> message)

  def parse(args: List[String]): Either[String, CliOptions] =
    def loop(rest: List[String], date: Option[String], week: Option[Option[String]], weekStart: Option[WeekStart], from: Option[String], to: Option[String], json: Boolean, touched: Boolean): Either[String, CliOptions] =
      rest match
        case Nil =>
          val today = startOfDay(new js.Date())
          val range = (date, week, from, to) match
            case (Some(d), None, None, None) => parseDay(d).map(day => (day, addDays(day, 1)))
            case (None, Some(d), None, None) =>
              d.fold[Either[String, Long]](Right(today))(parseDay).map { day =>
                val first = addDays(day, -((dayOfWeek(day) - weekStart.getOrElse(WeekStart.Monday).dayOfWeek + 7) % 7))
                (first, addDays(first, 7))
              }
            case _ if weekStart.isDefined => Left("--week-start only applies with --week")
            case (None, None, Some(f), t) =>
              for
                start <- parseDay(f)
                last  <- t.fold[Either[String, Long]](Right(today))(parseDay)
                _     <- Either.cond(last >= start, (), "--to must not be before --from")
              yield (start, addDays(last, 1))
            case (None, None, None, Some(_)) => Left("--to needs --from")
            case (None, None, None, None)    => Right((today, addDays(today, 1)))
            case _                           => Left("use only one of --date, --week, and --from/--to")
          range.map(CliOptions(_, json, touched))
        case "--date" :: value :: tail => loop(tail, Some(value), week, weekStart, from, to, json, touched)
        case "--week" :: value :: tail if !value.startsWith("--") => loop(tail, date, Some(Some(value)), weekStart, from, to, json, touched)
        case "--week" :: tail          => loop(tail, date, Some(None), weekStart, from, to, json, touched)
        case "--week-start" :: value :: tail =>
          WeekStart.values.find(_.name == value) match
            case Some(start) => loop(tail, date, week, Some(start), from, to, json, touched)
            case None        => Left(s"--week-start must be monday or sunday, got '$value'")
        case "--from" :: value :: tail => loop(tail, date, week, weekStart, Some(value), to, json, touched)
        case "--to" :: value :: tail   => loop(tail, date, week, weekStart, from, Some(value), json, touched)
        case "--json" :: tail          => loop(tail, date, week, weekStart, from, to, true, touched)
        case "--touched" :: tail       => loop(tail, date, week, weekStart, from, to, json, true)
        case other :: _                => Left(s"unknown argument '$other'\nusage: $Usage")
    loop(args, None, None, None, None, None, false, false)

  private val DayPattern = """(\d{4})-(\d{2})-(\d{2})""".r

  private def parseDay(text: String): Either[String, Long] = text match
    case DayPattern(y, m, d) =>
      val date = new js.Date(y.toInt, m.toInt - 1, d.toInt)
      if date.getFullYear() == y.toInt && date.getMonth() == m.toInt - 1 && date.getDate() == d.toInt then Right(date.getTime().toLong)
      else Left(s"not a calendar date: $text")
    case _ => Left(s"expected YYYY-MM-DD, got '$text'")

  private def startOfDay(date: js.Date): Long =
    new js.Date(date.getFullYear().toInt, date.getMonth().toInt, date.getDate().toInt).getTime().toLong

  private def addDays(dayStart: Long, days: Int): Long =
    val date = new js.Date(dayStart.toDouble)
    new js.Date(date.getFullYear().toInt, date.getMonth().toInt, date.getDate().toInt + days).getTime().toLong

  /** 0 for Sunday through 6 for Saturday. */
  private def dayOfWeek(dayStart: Long): Int = new js.Date(dayStart.toDouble).getDay().toInt

  def formatInstant(ms: Long): String =
    val date = new js.Date(ms.toDouble)
    f"${date.getFullYear().toInt}%04d-${date.getMonth().toInt + 1}%02d-${date.getDate().toInt}%02d ${date.getHours().toInt}%02d:${date.getMinutes().toInt}%02d"

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

  def agentSpans(rawInput: js.Dynamic): js.Any =
    val input      = objectInput(rawInput)
    val (from, to) = window(input)
    val spans      = store.agentSpans(from, to, threadIdsOf(input), js.Date.now().toLong)
    js.Dynamic.literal("agentSpans" -> spans.map(ActivityJson.agentSpan).toJSArray)

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
      ActivityRecorderRpc.AgentSpans -> method(
        "The agent turns the user set going that reach into [from, to): started by the user, or by a thread working " +
          "for them. Turns of automations, subscriptions and bb's own workers are left out.",
        Schema.obj(
          Seq("from", "to"),
          "from"      -> Schema.from,
          "to"        -> Schema.to,
          "threadIds" -> Schema.threadIds
        ),
        Schema.obj(Seq("agentSpans"), "agentSpans" -> Schema.array(Schema.agentSpan, "By thread id, then start"))
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
      ActivityRecorderRpc.AgentSpans -> js.Any.fromFunction1((input: js.Dynamic) => agentSpans(input)),
      ActivityRecorderRpc.Status    -> js.Any.fromFunction1((_: js.Dynamic) => status().toJSPromise),
      "reportView"                  -> js.Any.fromFunction1((input: js.Dynamic) => reportView(input))
    ),
    js.Dynamic.literal(
      "experimental_discoverable" -> true,
      "experimental_description"  -> "Per-thread activity intervals and the raw signals behind them; see ActivityRecorderRpc."
    )
  )

  // ------------------------------------------------------------------ CLI

  /** The thread's title for a listing, or its id when it cannot be read. */
  def threadTitle(threadId: String): Future[String] =
    bb.sdk.threads
      .get(js.Dynamic.literal("threadId" -> threadId))
      .toFuture
      .map { thread =>
        val row = if isDefined(thread.thread) then thread.thread else thread
        Seq(str(row.title), str(row.titleFallback)).find(_.nonEmpty).getOrElse(threadId)
      }
      .recover { case _ => threadId }

  def summary(args: List[String]): Future[js.Dynamic] =
    val options = CliArgs.parse(args) match
      case Right(options) => options
      case Left(message)  => return Future.successful(CliArgs.failure(message))
    val (from, to) = options.range
    currentSettings.flatMap { s =>
      val totals = Summary.totals(intervalsWithin(from, to, None, SignalKind.Attention, s.gapMs))
      totals
        .foldLeft(Future.successful(Vector.empty[(ThreadTotal, String)])) { (prior, total) =>
          prior.flatMap(titled => threadTitle(total.threadId).map(title => titled :+ (total, title)))
        }
        .map { titled =>
          val (timed, touched) = titled.partition((total, _) => !total.touchedOnly)
          val totalMs          = timed.map((total, _) => total.ms).sum
          val stdout =
            if options.json then
              def entry(total: ThreadTotal, title: String) = js.Dynamic.literal(
                "threadId"  -> total.threadId,
                "title"     -> title,
                "ms"        -> total.ms.toDouble,
                "intervals" -> total.intervals
              )
              js.JSON.stringify(
                js.Dynamic.literal(
                  "from"    -> from.toDouble,
                  "to"      -> to.toDouble,
                  "gapMs"   -> s.gapMs.toDouble,
                  "threads" -> timed.map(entry).toJSArray,
                  "touched" -> touched.map(entry).toJSArray,
                  "totalMs" -> totalMs.toDouble
                ),
                null,
                2
              )
            else
              val lines = Vector.newBuilder[String]
              lines += s"Attention from ${CliArgs.formatInstant(from)} to ${CliArgs.formatInstant(to)} (gap ${Summary.formatDuration(s.gapMs)})"
              if timed.isEmpty then lines += "  (no measured time)"
              timed.foreach((total, title) => lines += f"  ${Summary.formatDuration(total.ms)}%8s  ${total.threadId}  $title")
              lines += f"  ${Summary.formatDuration(totalMs)}%8s  total, with parallel threads counted in full"
              if options.touched && touched.nonEmpty then
                lines += ""
                lines += "Touched only (a message, but no measured time):"
                touched.foreach((total, title) => lines += s"            ${total.threadId}  $title")
              else if touched.nonEmpty then
                lines += s"  (${touched.size} more thread(s) touched without measured time; --touched lists them)"
              lines.result().mkString("\n")
          js.Dynamic.literal("exitCode" -> 0, "stdout" -> stdout)
        }
    }

  bb.cli.register(
    js.Dynamic.literal(
      "name"    -> "activity-recorder",
      "summary" -> "Per-thread attention totals from the activity recorder",
      "commands" -> js.Array(
        js.Dynamic.literal(
          "name"    -> "summary",
          "summary" -> "Attention per thread for today, a date, a week, or a range, most time first; parallel threads count in full",
          "usage"   -> CliArgs.Usage
        )
      ),
      "run" -> js.Any.fromFunction2 { (argv: js.Array[String], _: js.Dynamic) =>
        val result = argv.toList match
          case "summary" :: rest => summary(rest)
          case _                 => Future.successful(CliArgs.failure(s"usage: ${CliArgs.Usage}"))
        result.toJSPromise
      }
    )
  )
