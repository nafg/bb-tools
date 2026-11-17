package goals

import bbplugin.*
import goalscore.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

/** Realtime channel: `{ threadId }` after a write, or `{}` when the tree changed. */
private val LabelsChanged = "labels-changed"

private def describe(shown: Shown): String =
  val Shown(own, blockedBy, effective) = shown
  def valueText(v: Option[Int]) = v.fold("unvalued")(n => s"value $n")
  val ownText                   = own.goal.fold("no own goal")(g => s"own goal ${g.letter} (${g.title}), ${valueText(own.value)}")
  val effText                   = effective match
    case None                                             => "unlabeled"
    case Some(Effective(g, v, LabelSource.Own))           => s"${g.letter}, ${valueText(v)}"
    case Some(Effective(g, v, LabelSource.Inherited(id))) => s"${g.letter}, ${valueText(v)}, inherited from $id"
  val blocked = if blockedBy.isEmpty then "" else s"; waiting on ${blockedBy.mkString(", ")}"
  val move    = own.myMove.fold("")(m => s"; my move: ${m.name}")
  s"$ownText; effective: $effText$blocked$move"

private def failed(message: String): Future[Nothing] = Future.failed(js.JavaScriptException(js.Error(message)))

final class Service(bb: BbApi, store: Store):
  /** The one path every write takes. Goal, value, and my move are stored in the goals namespace; waits are the subscriptions plugin's
    * subscriptions to the blockers being archived, so a blocked thread is always told when its blocker finishes. Returns the thread as it
    * now reads, and why each requested wait that was not made was not made (the blocker had already finished, say).
    */
  def edit(threadId: String, edit: LabelEdit): Future[(Shown, List[String])] =
    val stored  = edit.goal.isDefined || edit.value.isDefined || edit.myMove.isDefined
    val written = if stored then store.write(threadId, edit.goal, edit.value, edit.myMove) else store.ownLabels(threadId)
    for
      labels <- written
      _ = moves.note(threadId, labels)
      added <- edit.blockedBy.fold(Future.successful(Map.empty[String, String]))(setWaits(threadId, _))
      _ = if !edit.isEmpty then bb.realtime.publish(LabelsChanged, js.Dynamic.literal("threadId" -> threadId))
      shown <- store.shownOf(threadId)
    yield shown -> added.toList.collect { case (blocker, message) if !shown.blockedBy.contains(blocker) => message }

  /** Makes `wanted` the threads `threadId` waits on; returns the subscriptions plugin's answer to each wait it was asked to add. */
  private def setWaits(threadId: String, wanted: List[String]): Future[Map[String, String]] =
    store.threadWaits().flatMap {
      case None        => failed("the subscriptions plugin is not installed; waits are its subscriptions, so none can be set")
      case Some(waits) =>
        val current = waits.getOrElse(threadId, Nil)
        val added   = wanted.filterNot(current.contains).foldLeft(Future.successful(Map.empty[String, String])) { (acc, blocker) =>
          acc.flatMap(done => store.waitOn(threadId, blocker).map(message => done + (blocker -> message)))
        }
        added.flatMap { answers =>
          current
            .filterNot(wanted.contains)
            .foldLeft(Future.unit)((acc, blocker) => acc.flatMap(_ => store.stopWaitingOn(threadId, blocker).map(_ => ())))
            .map(_ => answers)
        }
    }

  /** Moves each `blockedBy` list goals stored itself, before waits were subscriptions, into the subscriptions plugin, then removes it.
    * An archived thread's list is dropped: it lends nothing and has nothing left to be woken for. A wait on a blocker that has already
    * finished is dropped too, as the subscriptions plugin makes no subscription for it. Leaves everything in place when the
    * subscriptions plugin is not installed.
    */
  def migrateLegacyWaits(): Future[Unit] =
    for
      threads <- store.allThreads()
      legacy  <- store.legacyBlockedBy(threads.map(_.id))
      installed <- if legacy.isEmpty then Future.successful(false) else store.threadWaits().map(_.isDefined)
      _ <-
        if legacy.isEmpty then Future.unit
        else if !installed then
          bb.log.warn(s"${legacy.size} thread(s) still store blockedBy; install the subscriptions plugin to move them into it")
          Future.unit
        else
          val archived = threads.filter(_.archived).map(_.id).toSet
          legacy.toList.foldLeft(Future.unit) { case (acc, (threadId, blockers)) =>
            acc.flatMap { _ =>
              val moved =
                if archived.contains(threadId) then
                  bb.log.info(s"dropped stored blockedBy of archived $threadId: ${blockers.mkString(", ")}")
                  Future.unit
                else
                  blockers.foldLeft(Future.unit) { (acc, blocker) =>
                    acc.flatMap(_ => store.waitOn(threadId, blocker).map(m => bb.log.info(s"stored wait $threadId -> $blocker: $m")))
                  }
              moved.flatMap(_ => store.removeLegacyBlockedBy(threadId))
            }
          }.map(_ => bb.realtime.publish(LabelsChanged, js.Dynamic.literal()))
    yield ()

  /** The threads whose "my move" is set, so a user message to one of them can clear it without reading every thread's metadata. */
  object moves:
    private val pending                                    = scala.collection.mutable.Set.empty[String]
    def note(threadId: String, labels: ThreadLabels): Unit = if labels.myMove.isDefined then pending += threadId else pending -= threadId
    def seed(): Future[Unit] = store.snapshot().map(snap => snap.own.foreach { case (id, labels) => note(id, labels) })

    /** Clears the move when the user has written to the thread since setting it. */
    def reconcile(threadId: String): Future[Unit] =
      if !pending.contains(threadId) then Future.unit
      else
        for
          own <- store.ownLabels(threadId)
          at  <- if own.myMove.isDefined then store.latestUserMessageAt(threadId) else Future.successful(None)
          _   <- at match
            case Some(messageAt) if ThreadLabels.clearedBy(own.myMoveAt, messageAt) =>
              store.write(threadId, None, None, Some(None)).map { cleared =>
                note(threadId, cleared)
                bb.realtime.publish(LabelsChanged, js.Dynamic.literal("threadId" -> threadId))
              }
            case _ =>
              note(threadId, own)
              Future.unit
        yield ()

  /** How far back `rows` and `bb goals list` look when no window is given. */
  val DefaultLookbackMs = 30 * 86_400_000L

  def rows(): Future[js.Dynamic] =
    store.snapshot().map(_.within(js.Date.now().toLong - DefaultLookbackMs, Map.empty)).map { snap =>
      js.Dynamic.literal(
        "rows" -> snap.resolved.toList.map { case (id, r) =>
          js.Dynamic.literal(
            "threadId"       -> id,
            "own"            -> Out.own(snap.own.getOrElse(id, ThreadLabels.Empty), snap.blockedBy.getOrElse(id, Nil)),
            "effective"      -> Out.effective(r.label),
            "lent"           -> Out.lent(r.lent),
            "importance"     -> r.importance.map(v => v: js.Any).orNull,
            "closedBlockers" -> r.closedBlockers.toJSArray
          ): js.Any
        }.toJSArray
      )
    }

  def overview(from: Double, to: Double): Future[js.Dynamic] =
    // The three reads are independent; start them together.
    val snapshot  = store.snapshot()
    val names     = store.projectNames()
    val attention = store.attentionSeconds(from, to)
    for
      snap    <- snapshot
      names   <- names
      seconds <- attention
    yield
      val attention   = seconds.toOption.flatten.getOrElse(Map.empty)
      val shown       = snap.within(from.toLong, attention)
      val model       = Overview.build(shown.threads, shown.own, shown.ancestors, shown.blockedBy, attention)
      val recorderOut = seconds match
        case Right(Some(_)) => js.Dynamic.literal("state" -> "ok")
        case Right(None)    => js.Dynamic.literal("state" -> "missing")
        case Left(message)  => js.Dynamic.literal("state" -> "error", "message" -> message)
      js.Dynamic.literal(
        "from"     -> from,
        "to"       -> to,
        "recorder" -> recorderOut,
        "projects" -> js.Dictionary(names.toSeq*),
        "groups"   -> Out.groups(model)
      )

  /** Imports a grades spreadsheet; returns what was written and what was not. */
  def importCsv(text: String): Future[(List[GradeRow], List[Skipped], List[(GradeRow, String)])] =
    val (rows, skipped) = GradesCsv.parse(text)
    rows
      .foldLeft(Future.successful(List.empty[(GradeRow, Either[String, Unit])])) { (acc, row) =>
        acc.flatMap { done =>
          store
            .write(row.threadId, Some(Some(row.goal)), Some(row.value))
            .map(_ => row -> Right(()))
            .recover { case e => row -> Left(e.getMessage) }
            .map(done :+ _)
        }
      }
      .map { results =>
        if rows.nonEmpty then bb.realtime.publish(LabelsChanged, js.Dynamic.literal())
        (results.collect { case (r, Right(())) => r }, skipped, results.collect { case (r, Left(m)) => r -> m })
      }

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit =
  val store   = Store(bb)
  val service = Service(bb, store)

  /** A goal or value as the caller typed it, whether as text or as a number. */
  def typed(value: js.Dynamic): Option[String] =
    Js.optStr(value).orElse(Dyn.number(value).map(n => if n == n.toInt then n.toInt.toString else n.toString))
  def typedList(value: js.Dynamic): Option[List[String]] = Js.optArr(value).map(_.flatMap(Js.optStr))
  def result(threadId: String, shown: Shown, notes: List[String]): js.Dynamic =
    js.Dynamic.literal(
      "threadId"  -> threadId,
      "own"       -> Out.own(shown.own, shown.blockedBy),
      "effective" -> Out.effective(shown.effective),
      "notes"     -> notes.toJSArray,
      "text"      -> (describe(shown) :: notes).mkString("\n")
    )

  def editFrom(input: js.Dynamic): Future[js.Dynamic] =
    Js.optStr(input.threadId) match
      case None           => failed("threadId is required")
      case Some(threadId) =>
        LabelEdit.parse(threadId, typed(input.goal), typed(input.value), typedList(input.blockedBy), typed(input.myMove)) match
          case Left(message) => failed(message)
          case Right(edit)   => service.edit(threadId, edit).map((shown, notes) => result(threadId, shown, notes))

  bb.rpc.register(
    js.Dynamic.literal(
      "rows"       -> js.Dynamic.literal("input" -> StandardSchema.passthrough, "output" -> StandardSchema.passthrough),
      "labels_get" -> js.Dynamic.literal("input" -> StandardSchema.passthrough, "output" -> StandardSchema.passthrough),
      "labels_set" -> js.Dynamic.literal("input" -> StandardSchema.passthrough, "output" -> StandardSchema.passthrough),
      "overview"   -> js.Dynamic.literal("input" -> StandardSchema.passthrough, "output" -> StandardSchema.passthrough)
    ),
    js.Dynamic.literal(
      "rows"       -> js.Any.fromFunction1((_: js.Any) => service.rows().toJSPromise),
      "labels_get" -> js.Any.fromFunction1 { (raw: js.Any) =>
        val input = raw.asInstanceOf[js.Dynamic]
        Js.optStr(input.threadId) match
          case None           => failed("threadId is required").toJSPromise
          case Some(threadId) => store.shownOf(threadId).map(result(threadId, _, Nil)).toJSPromise
      },
      "labels_set" -> js.Any.fromFunction1((raw: js.Any) => editFrom(raw.asInstanceOf[js.Dynamic]).toJSPromise),
      "overview"   -> js.Any.fromFunction1 { (raw: js.Any) =>
        val input = raw.asInstanceOf[js.Dynamic]
        val to    = Dyn.number(input.to).getOrElse(js.Date.now())
        val from  = Dyn.number(input.from).getOrElse(to - 7 * 86_400_000.0)
        service.overview(from, to).toJSPromise
      }
    )
  )

  // A reparented or new thread may now read a different ancestor's label.
  bb.events.on("experimental_thread.parentChanged", _ => bb.realtime.publish(LabelsChanged, js.Dynamic.literal()))
  bb.events.on("thread.created", _ => bb.realtime.publish(LabelsChanged, js.Dynamic.literal()))

  // A user message to a thread with a pending "my move" clears it. The debounced per-thread event covers typed messages and steers alike.
  val _ = service.moves.seed().recover { case e => bb.log.warn(s"could not read pending moves: ${e.getMessage}") }
  val _ = service.migrateLegacyWaits().recover { case e => bb.log.warn(s"could not move stored blockedBy into subscriptions: ${e.getMessage}") }
  bb.events.on(
    "experimental_thread.events",
    payload =>
      Js.optStr(payload.thread.id).foreach { id =>
        service.moves.reconcile(id).recover { case e => bb.log.warn(s"could not reconcile my move for $id: ${e.getMessage}") }
      }
  )

  bb.agents.registerTool(
    js.Dynamic.literal(
      "name"        -> "goals_set_label",
      "description" ->
        ("Set the goal a bb thread serves, how big a lever it is for that goal, which threads it is waiting on, or what the user still owes it. " +
          "Goals: B billable client work, C client work not billable, P product or future income, S sanity, " +
          "M maintainer obligations, F fun. Value 1 is the biggest lever within its goal, 3 the smallest. " +
          "Omitted fields are left as they are; \"none\" clears a field. Defaults to the current thread."),
      "instructions" ->
        ("When the user says what a thread is for or how important it is (\"this is billable\", \"make this a B2\", " +
          "\"this is sanity work\", \"this one is waiting on thread X\"), call goals_set_label rather than only " +
          "acknowledging. A thread without its own goal shows its nearest labeled ancestor's goal."),
      "presentation" -> js.Dynamic.literal(
        "label" -> js.Dynamic.literal("pending" -> "Labeling the thread's goal", "completed" -> "Labeled the thread's goal")
      ),
      "parameters" -> js.Dynamic.literal(
        "type"       -> "object",
        "properties" -> js.Dynamic.literal(
          "threadId" -> js.Dynamic.literal("type" -> "string", "description" -> "Thread to label; omit for the current thread."),
          "goal"     -> js.Dynamic.literal(
            "type"        -> "string",
            "enum"        -> js.Array("B", "C", "P", "S", "M", "F", "none"),
            "description" -> "The goal this thread serves, or none to clear it."
          ),
          "value" -> js.Dynamic.literal(
            "type"        -> "string",
            "enum"        -> js.Array("1", "2", "3", "none"),
            "description" -> "How big a lever within its goal: 1 biggest, 3 smallest; none to clear."
          ),
          "blockedBy" -> js.Dynamic.literal(
            "type"        -> "array",
            "items"       -> js.Dynamic.literal("type" -> "string"),
            "description" -> ("The complete list of thread ids this thread waits on; an empty list clears it. Each wait is a subscription " +
              "to the blocker being archived, so this thread is told when it finishes; a blocker that has already finished is not waited on.")
          ),
          "myMove" -> js.Dynamic.literal(
            "type"        -> "string",
            "enum"        -> js.Array("quick", "focus", "later", "none"),
            "description" -> ("What the user still owes this thread: quick (a short answer or decision), focus (real focused time), " +
              "later (nothing now, more to prompt later), or none. Clears by itself when the user next writes to the thread.")
          )
        ),
        "additionalProperties" -> false
      ),
      "execute" -> js.Any.fromFunction2 { (params: js.Dynamic, context: js.Dynamic) =>
        val input = js.Dynamic.literal(
          "threadId"  -> Js.optStr(params.threadId).orElse(Js.optStr(context.threadId)).orNull,
          "goal"      -> params.goal,
          "value"     -> params.value,
          "blockedBy" -> params.blockedBy,
          "myMove"    -> params.myMove
        )
        editFrom(input)
          .map(out => Js.optStr(out.text).getOrElse(""))
          .recover { case e => s"Could not set the label: ${e.getMessage}" }
          .toJSPromise
      }
    )
  )

  bb.cli.register(
    js.Dynamic.literal(
      "name"     -> "goals",
      "summary"  -> "Label threads with the goal they serve and see every thread's label",
      "commands" -> js.Array(
        js.Dynamic
          .literal("name" -> "show", "summary" -> "Print a thread's own and effective label", "usage" -> "bb goals show [thread-id]"),
        js.Dynamic.literal(
          "name" -> "set",
          "summary" -> "Set a thread's goal (B C P S M F or none), value (1 2 3 or none), blockers (comma-separated thread ids, or none), or my move (quick, focus, later, or none)",
          "usage" -> "bb goals set [thread-id] [--goal X] [--value N] [--blocked-by ids] [--my-move quick|focus|later|none]"
        ),
        js.Dynamic
          .literal(
            "name"    -> "list",
            "summary" -> "Print the effective label of every thread active in the last 30 days, archived included, one per line",
            "usage"   -> "bb goals list"
          ),
        js.Dynamic.literal(
          "name"    -> "import",
          "summary" -> "Write goals from a CSV with thread_id, goal, and value columns; rows without a goal are skipped",
          "usage"   -> "bb goals import <file.csv>"
        )
      ),
      "run" -> js.Any.fromFunction2 { (argv: js.Array[String], context: js.Dynamic) =>
        Cli.run(bb, store, service, argv.toList, Js.optStr(context.threadId), Js.optStr(context.cwd)).toJSPromise
      }
    )
  )

private object Cli:
  private val Usage =
    "Usage: bb goals show [thread-id] | set [thread-id] [--goal X] [--value N] [--blocked-by ids] [--my-move quick|focus|later|none] | list | import <file.csv>"
  private def ok(stdout: String): js.Dynamic   = js.Dynamic.literal("exitCode" -> 0, "stdout" -> (stdout + "\n"))
  private def err(message: String): js.Dynamic = js.Dynamic.literal("exitCode" -> 1, "stderr" -> (message + "\n"))

  def run(
      bb: BbApi,
      store: Store,
      service: Service,
      argv: List[String],
      currentThread: Option[String],
      cwd: Option[String]
  ): Future[js.Dynamic] =
    val result = argv match
      case "show" :: rest =>
        rest.headOption.orElse(currentThread) match
          case None     => Future.successful(err("No thread given and no current thread."))
          case Some(id) =>
            store.shownOf(id).map(shown => ok(s"$id: ${describe(shown)}"))
      case "set" :: rest =>
        parseSet(rest, currentThread) match
          case Left(message)     => Future.successful(err(message))
          case Right((id, edit)) =>
            service.edit(id, edit).map((shown, notes) => ok((s"$id: ${describe(shown)}" :: notes).mkString("\n")))
      case "list" :: Nil =>
        store.snapshot().map(_.within(js.Date.now().toLong - service.DefaultLookbackMs, Map.empty)).map { snap =>
          val resolved = snap.resolved
          val lines    = snap.threads.sortBy(_.title).map { t =>
            val label = resolved(t.id).label
              .fold("--")(e =>
                e.goal.letter + e.value.fold("")(_.toString) + (e.source match
                  case LabelSource.Own          => ""
                  case LabelSource.Inherited(_) => "*")
              )
            f"$label%-4s ${t.id}  ${t.title}"
          }
          ok((lines :+ "(* inherited from an ancestor)").mkString("\n"))
        }
      case "import" :: file :: Nil =>
        val path = if file.startsWith("/") then file else cwd.fold(file)(c => s"${c.stripSuffix("/")}/$file")
        bb.sdk.files.read(js.Dynamic.literal("path" -> path)).toFuture.flatMap { raw =>
          val text = Js.optStr(raw.contentEncoding) match
            case Some("base64") =>
              js.Dynamic.global.Buffer.from(raw.content, "base64").applyDynamic("toString")("utf8").asInstanceOf[String]
            case _ => Js.optStr(raw.content).getOrElse("")
          service.importCsv(text).map { case (written, skipped, failed) =>
            val lines =
              List(s"Labeled ${written.size} thread(s).") ++
                written.map(r => s"  ${r.threadId}: ${r.goal.letter}${r.value.fold("")(_.toString)}") ++
                (if skipped.isEmpty then Nil
                 else s"Skipped ${skipped.size} row(s):" :: skipped.map(s => s"  line ${s.line} ${s.threadId}: ${s.reason}")) ++
                (if failed.isEmpty then Nil else s"Failed ${failed.size} write(s):" :: failed.map { case (r, m) => s"  ${r.threadId}: $m" })
            if failed.isEmpty then ok(lines.mkString("\n"))
            else js.Dynamic.literal("exitCode" -> 1, "stdout" -> (lines.mkString("\n") + "\n"))
          }
        }
      case _ => Future.successful(err(Usage))
    result.recover { case e => err(e.getMessage) }

  private def parseSet(args: List[String], currentThread: Option[String]): Either[String, (String, LabelEdit)] =
    final case class Flags(
        id: Option[String] = None,
        goal: Option[String] = None,
        value: Option[String] = None,
        blocked: Option[String] = None,
        move: Option[String] = None
    )
    @annotation.tailrec
    def loop(rest: List[String], f: Flags): Either[String, Flags] =
      rest match
        case "--goal" :: g :: tail                                    => loop(tail, f.copy(goal = Some(g)))
        case "--value" :: v :: tail                                   => loop(tail, f.copy(value = Some(v)))
        case "--blocked-by" :: b :: tail                              => loop(tail, f.copy(blocked = Some(b)))
        case "--my-move" :: m :: tail                                 => loop(tail, f.copy(move = Some(m)))
        case flag :: Nil if flag.startsWith("--")                     => Left(s"$flag requires a value")
        case other :: tail if f.id.isEmpty && !other.startsWith("--") => loop(tail, f.copy(id = Some(other)))
        case other :: _                                               => Left(s"unexpected argument '$other'\n$Usage")
        case Nil                                                      => Right(f)
    loop(args, Flags()).flatMap { f =>
      f.id.orElse(currentThread).toRight("No thread given and no current thread.").flatMap { threadId =>
        val blockedBy = f.blocked.map(b => if b.equalsIgnoreCase("none") then Nil else b.split(",").toList)
        LabelEdit.parse(threadId, f.goal, f.value, blockedBy, f.move).flatMap { edit =>
          if edit.isEmpty then Left(s"Nothing to set.\n$Usage") else Right(threadId -> edit)
        }
      }
    }
