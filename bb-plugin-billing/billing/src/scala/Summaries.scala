package billing

import activitycore.Window

import bbplugin.Dyn.{isDefined, number, str}
import bbplugin.{BbApi, BbSettingsHandle}
import billingcore.ToolCall

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.util.Try

/** Who said or did a thing in a thread. */
enum Speaker:
  case User, Agent, Work

/** One turn of a thread's conversation: what the user wrote, what the agent answered, or a command or tool the agent
  * ran, with when and its position in the thread.
  */
final case class Message(at: Long, seq: String, speaker: Speaker, text: String, call: Option[ToolCall] = None)

/** Reads a thread's whole conversation from bb's timeline, both sides and the agent's work, oldest first, the way the
  * recap plugin does.
  */
final class Conversation(bb: BbApi)(using ExecutionContext):
  /** Pages of the timeline read at most. The timeline is read from the newest page back, so a thread longer than this
    * loses its oldest part: a stretch from before that has no messages to count or to summarize. The limit only guards
    * against a runaway; a thread of a few thousand rows is read whole.
    */
  private val MaxPages = 600

  /** A timeline row as it is kept between reads: its id, and the messages it and its nested rows hold. The rows
    * themselves are not kept: they carry far more than the messages (tool arguments and output), for every thread read.
    */
  private final case class Row(id: String, messages: Vector[Message])

  /** Each thread's timeline rows from its last read, oldest first, back to the thread's start. */
  private val known = scala.collection.mutable.Map.empty[String, Vector[Row]]

  /** The thread's conversation. A thread read before is read again only from its newest page back to the first page
    * that meets the rows kept from then: the rows of the pages read come fresh (a turn that was running has changed),
    * older rows are taken from then.
    */
  def of(threadId: String): Future[Vector[Message]] =
    val before   = known.getOrElse(threadId, Vector.empty)
    val beforeAt = before.zipWithIndex.map((r, i) => r.id -> i).toMap
    def page(cursor: Option[(String, Double)], acc: Vector[js.Dynamic], n: Int): Future[(Vector[js.Dynamic], Boolean)] =
      val request = js.Dynamic.literal("threadId" -> threadId, "includeNestedRows" -> "true")
      cursor.foreach { (id, seq) =>
        request.updateDynamic("beforeAnchorId")(id)
        request.updateDynamic("beforeAnchorSeq")(seq.toLong.toString)
      }
      bb.sdk.threads.timeline(request).toFuture.flatMap { response =>
        val rows  = response.rows.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].fold(Vector.empty)(_.toVector)
        // A null `olderCursor` means the page reached the thread's start.
        val older = Option(response.timelinePage).filter(isDefined).map(_.olderCursor).filter(isDefined)
        val meets = rows.exists(r => beforeAt.contains(str(r.id)))
        older match
          case Some(c) if n + 1 < MaxPages && rows.nonEmpty && !meets =>
            page(Some((str(c.anchorId), number(c.anchorSeq).getOrElse(0.0))), rows ++ acc, n + 1)
          case _                                                          =>
            Future.successful((rows ++ acc, older.isEmpty || rows.isEmpty))
      }
    page(None, Vector.empty, 0).map { (freshRows, reachedStart) =>
      val fresh     = freshRows.map(r => Row(str(r.id), flatten(Vector(r)).flatMap(message)))
      val freshIds  = fresh.map(_.id).toSet
      val oldestMet = fresh.flatMap(r => beforeAt.get(r.id)).minOption
      val rows      = oldestMet match
        case Some(i) if !reachedStart => before.take(i).filterNot(r => freshIds(r.id)) ++ fresh
        case _                        => fresh
      known.update(threadId, rows)
      rows.flatMap(_.messages).sortBy(_.at)
    }.recover {
      // A deleted thread (bb answers `thread_not_found`) has no conversation left; its recorded time remains.
      case e if isThreadNotFound(e) => Vector.empty
    }

  /** Rows with their nested rows, in order. */
  private def flatten(rows: Vector[js.Dynamic]): Vector[js.Dynamic] =
    rows.flatMap { row =>
      val nested = Vector("children", "childRows").flatMap(k =>
        row.selectDynamic(k).asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].toOption.map(_.toVector).getOrElse(
          Vector.empty
        )
      )
      row +: flatten(nested)
    }

  private def message(row: js.Dynamic): Option[Message] =
    val at  = number(row.createdAt).map(_.toLong)
    val seq =
      Option(str(row.messageSeq)).filter(_.nonEmpty).orElse(Option(str(row.id)).filter(_.nonEmpty)).getOrElse("")
    str(row.kind) match
      case "conversation" =>
        val text = str(row.text).trim
        str(row.role) match
          // The user's own words, as the recorder tells them: not another thread's message, not a bb notice.
          case "user" if str(row.initiator) == "user" && str(row.senderThreadId).isEmpty && !text.startsWith("[bb ") =>
            at.filter(_ => text.nonEmpty).map(Message(_, seq, Speaker.User, text))
          case "assistant" => at.filter(_ => text.nonEmpty).map(Message(_, seq, Speaker.Agent, text))
          case _           => None
      case "work"         =>
        val text = str(row.workKind) match
          case "command" => Option(str(row.command)).filter(_.nonEmpty).map(c => s"ran: $c")
          // Converse's wait for more speech is listening, not work.
          case "tool"    =>
            Option(str(row.toolName)).filter(t => t.nonEmpty && !t.endsWith("converse_wait_for_more_speech")).map(t =>
              s"used $t" + Option(str(row.description)).filter(_.nonEmpty).fold("")(d => s": $d")
            )
          case _         => Option(str(row.description)).filter(_.nonEmpty)
        val call = Option(str(row.toolName)).filter(_.nonEmpty).map { name =>
          val args = Option(row.toolArgs).filter(isDefined).map(a => js.JSON.stringify(a)).getOrElse("")
          ToolCall(at.getOrElse(0L), name, args.take(300))
        }
        at.zip(text).map((a, t) => Message(a, seq, Speaker.Work, t, call))
      case _              => None

/** The agent a summary run uses: a bb provider and one of its models, optionally at a reasoning level. */
final case class ModelSelection(providerId: String, model: String, reasoningLevel: Option[String])

/** One model bb's composer offers, as `bb billing models` lists it. */
final case class ModelChoice(providerId: String, model: String, displayName: String, isDefault: Boolean)

/** A stretch of work to describe: a window with its thread's title and the conversation during it. */
final case class Stretch(window: Window, threadTitle: String, messages: Vector[Message])

/** One-line descriptions of windows, written by a model the way bb's own thread-namer and recap plugins get theirs: the
  * prompt runs in a throwaway hidden thread on a provider and model the user picks (the `summaryModel` setting, else
  * bb's default model), the answer is read from the thread's output, and the thread is stopped and deleted. One run
  * describes every stretch of a submission at once; the lines are cached per window key, so a stretch is described once
  * however often the submission is replanned. A window with no line yet shows the user's first message in it.
  */
final class Summaries(bb: BbApi, settings: BbSettingsHandle, store: Store)(using ExecutionContext):
  /** How long one run may take before it is given up. */
  private val TimeoutMs = 5 * 60_000.0

  /** How much of one turn the prompt quotes, per speaker, and how much of one stretch in all. */
  private val UserChars    = 1500
  private val AgentChars   = 800
  private val WorkChars    = 200
  private val StretchChars = 8000

  private val PermissionPreference = Vector("auto", "accept-edits", "full")

  /** The models bb's composer offers, each with the provider that runs it. */
  def models(): Future[Vector[ModelChoice]] =
    bb.sdk.system.asInstanceOf[js.Dynamic].executionOptions().asInstanceOf[js.Promise[js.Dynamic]].toFuture.map {
      options =>
        val fallbackProvider = options.providers.asInstanceOf[js.Array[js.Dynamic]].find(p =>
          bool(p.available)
        ).map(p => str(p.id))
        options.models.asInstanceOf[js.Array[js.Dynamic]].toVector.flatMap { m =>
          Option(str(m.routeProviderId)).filter(_.nonEmpty).orElse(fallbackProvider).map(provider =>
            ModelChoice(provider, str(m.model), str(m.displayName), bool(m.isDefault))
          )
        }
    }

  /** The picked model when the setting names one as `provider/model`; else the first Haiku bb offers (summaries are
    * cheap work), else bb's default model; None when bb offers no model at all.
    */
  def selection(): Future[Option[ModelSelection]] =
    settings.get().toFuture.flatMap { s =>
      val picked    = str(s.summaryModel).trim
      val reasoning = Option(str(s.summaryReasoningLevel).trim).filter(_.nonEmpty)
      picked.split("/", 2) match
        case Array(provider, model) if provider.nonEmpty && model.nonEmpty =>
          Future.successful(Some(ModelSelection(provider, model, reasoning)))
        case _                                                             =>
          models().map(all =>
            all.find(_.model.contains("haiku")).orElse(all.find(_.isDefault)).orElse(all.headOption).map(m =>
              ModelSelection(m.providerId, m.model, reasoning)
            )
          )
    }

  def available(): Future[Boolean] = selection().map(_.isDefined).recover { case _ => false }

  /** What a window says when no model has described it yet: the user's first message in it, else the agent's first
    * reply, else the thread title.
    */
  def fallbackWindow(threadTitle: String, messages: Vector[Message]): String =
    messages.find(_.speaker == Speaker.User).orElse(messages.find(_.speaker == Speaker.Agent))
      .map(m => plain(m.text).take(100)).filter(_.nonEmpty).getOrElse(threadTitle)

  /** A message without the tags bb and converse put in front of it, on one line. */
  private def plain(text: String): String =
    text
      .replaceAll("\\[/?heard\\]", " ")
      // Leading tags bb and converse add: "[bb message …]", "(Spoken before …)", "(The user interrupted …)".
      .replaceAll("^\\s*(\\[[^\\]]*\\]|\\([^)]*\\))\\s*", "")
      .replaceAll("\\s+", " ")
      .trim

  /** The cached line of each window that has one. */
  def cached(windows: Seq[Window]): Map[String, String] = store.windowSummaries(windows.map(_.key))

  /** A day whose entry notes are wanted: its stretches in the order they happened. */
  final case class DayToNote(day: String, stretches: Vector[Stretch])

  /** The cache key of a day's notes: the day and the stretch lines it is made of, so new or struck stretches ask for
    * new notes while an unchanged day keeps its own.
    */
  def dayKey(day: String, lines: Seq[(String, String)]): String =
    s"$day|" + lines.map((key, line) => s"$key=${line.hashCode}").mkString("|")

  /** Describe, in one worker run, every stretch that has no cached line yet and every day that has no cached notes for
    * its current lines. Lines are one sentence per stretch; notes are one paragraph per day, written from that day's
    * lines (cached or just written), for the client, within Zoho's 500 characters. Both are cached. Fails when there is
    * no model, the run times out, or the answer cannot be read; then stretches keep their fallback lines and days keep
    * their joined lines.
    */
  def describe(
    projectId: String,
    environmentId: Option[String],
    stretches: Vector[Stretch],
    days: Vector[DayToNote],
    clock: Long => String): Future[Unit] =
    val known       = cached(stretches.map(_.window))
    val pending     = stretches.filterNot(s => known.contains(s.window.key))
    // A day needs notes when, with every line in hand, its key has none cached; with lines still to write it needs
    // them too, and the key is settled once the lines come back.
    val daysPending = days.filter(d =>
      d.stretches.exists(s => !known.contains(s.window.key)) ||
        store.dayNotes(dayKey(d.day, d.stretches.map(s => s.window.key -> known(s.window.key)))).isEmpty
    )
    if pending.isEmpty && daysPending.isEmpty then Future.unit
    else
      selection().flatMap {
        case None      => Future.failed(new RuntimeException("bb offers no model to write summaries with."))
        case Some(sel) =>
          runWorker(projectId, environmentId, sel, prompt(pending, daysPending, known, clock)).map { output =>
            val (lines, notes) = parse(output, pending.size, daysPending.map(_.day))
            if pending.nonEmpty && lines.isEmpty then
              throw new RuntimeException(s"The model's answer had no summaries in it: ${output.take(200)}")
            val written = lines.flatMap((index, line) => pending.lift(index - 1).map(s => s.window.key -> line)).toMap
            written.foreach((key, line) => store.saveWindowSummary(key, line))
            val all     = known ++ written
            daysPending.foreach { d =>
              val ls = d.stretches.flatMap(s => all.get(s.window.key).map(s.window.key -> _))
              // Notes are kept only when every line of the day is known: the key must name the lines they came from.
              if ls.size == d.stretches.size then
                notes.get(d.day).foreach(n => store.saveDayNotes(dayKey(d.day, ls), n))
            }
          }
      }

  private def prompt(
    pending: Vector[Stretch],
    days: Vector[DayToNote],
    known: Map[String, String],
    clock: Long => String): String =
    val index       = pending.zipWithIndex.map((s, i) => s.window.key -> (i + 1)).toMap
    val items       = pending.zipWithIndex.map { (s, i) =>
      val inside = s.messages.filter(m => m.at >= s.window.start && m.at < s.window.end)
      val quoted = inside match
        case Vector() => "(no turn in this stretch; the user was reading, or speaking to the agent in another thread)"
        case ms       =>
          val lines = ms.map(m =>
            m.speaker match
              case Speaker.User  => "  - User: " + plain(m.text).take(UserChars)
              case Speaker.Agent => "  - Agent: " + plain(m.text).take(AgentChars)
              case Speaker.Work  => "  - Agent " + m.text.take(WorkChars)
          )
          // A long stretch keeps its start and its end; the middle says so.
          val whole = lines.mkString("\n")
          if whole.length <= StretchChars then whole
          else whole.take(StretchChars / 2) + "\n  […middle omitted…]\n" + whole.takeRight(StretchChars / 2)
      s"${i + 1}. Thread \"${s.threadTitle}\", ${clock(s.window.start)} to ${clock(s.window.end)}:\n$quoted"
    }
    val dayItems    = days.map { d =>
      val parts = d.stretches.map(s =>
        known.get(s.window.key).map(line => s"  - ${clock(s.window.start)}: $line")
          .getOrElse(s"  - ${clock(s.window.start)}: (stretch ${index(s.window.key)} above)")
      )
      s"Day ${d.day}:\n${parts.mkString("\n")}"
    }
    val stretchPart =
      if pending.isEmpty then ""
      else
        s"""Below are ${pending.size} stretches of work a developer did with a coding agent, each with the thread it happened in and the conversation during it: what the developer asked (often dictated, so the transcription may be rough), what the agent answered, and the commands and tools the agent used.
           |
           |For each stretch, write the line of a client's invoice for it: one or two sentences, at most 300 characters, in plain past tense. The client pays for the work but does not read the code: say what the work means for them or their users (a bug fixed, a feature added or changed, a risk removed, a problem found, a decision made) in words they would use. Never use names from the code (classes, functions, files, settings, libraries, tests) or counts of code changes; a merge request or task number may be named. Work the client would not notice, such as restructuring code or tests, gets one plain phrase saying what it was for (for example "simplified the server's startup code so later changes are safer"), not a phrase per change. Write it as the work itself, with no actor: never mention the developer, the agent, threads, tools, reviews, who asked or agreed to what, or internal steps such as rebasing, branches, worktrees, rebuilding or retrying. Leave out open questions and next steps. When a stretch has no turn, describe it from the thread title and the stretches around it.
           |
           |${items.mkString("\n\n")}
           |""".stripMargin
    val dayPart     =
      if days.isEmpty then ""
      else
        s"""Then, for each day below, write the notes of that day's time entry as the client will read them on the invoice: one paragraph of at most 450 characters, in plain past tense, that tells what was accomplished for the client that day from its stretch lines (a stretch referred to by number means the line you wrote for it above). Put first what the client or their users would notice; group the rest by what it was for, drop repetition, and fold work they would not notice into one plain phrase. The same rules as for the lines apply: no names from the code, no actor, no tools, no internal steps, no open questions.
           |
           |${dayItems.mkString("\n\n")}
           |""".stripMargin
    s"""You are writing the lines of a client's invoice. Do not run any tool or read any file.
       |
       |$stretchPart
       |$dayPart
       |Answer with nothing but one JSON object: {"lines": [{"i": 1, "line": "..."}, ...], "days": [{"day": "yyyy-MM-dd", "notes": "..."}, ...]}, with a line for every numbered stretch and notes for every day listed.""".stripMargin

  /** The `(i, line)` pairs and the `day -> notes` of the answer's JSON object; tolerates prose or a code fence around
    * it, and the earlier bare-array answer.
    */
  private def parse(output: String, count: Int, days: Seq[String]): (Vector[(Int, String)], Map[String, String]) =
    val start = output.indexWhere(c => c == '{' || c == '[')
    val end   = math.max(output.lastIndexOf('}'), output.lastIndexOf(']'))
    if start < 0 || end <= start then (Vector.empty, Map.empty)
    else
      val parsed   = Try(js.JSON.parse(output.substring(start, end + 1))).toOption
      val lineRows =
        parsed.map(p =>
          if js.Array.isArray(p) then p.asInstanceOf[js.Array[js.Dynamic]]
          else
            p.lines.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].getOrElse(js.Array[js.Dynamic]())
        ).toVector.flatMap(_.toVector)
      val lines    = lineRows.flatMap { o =>
        val index = number(o.i).map(_.toInt)
        val line  = str(o.line).replaceAll("\\s+", " ").trim
        index.filter(i => i >= 1 && i <= count && line.nonEmpty).map(_ -> line.take(400))
      }
      val dayRows  = parsed.filterNot(js.Array.isArray(_))
        .flatMap(p => p.days.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].toOption).toVector.flatMap(_.toVector)
      val notes    = dayRows.flatMap { o =>
        val day = str(o.day).trim
        val n   = str(o.notes).replaceAll("\\s+", " ").trim
        Option.when(days.contains(day) && n.nonEmpty)(day -> n.take(500))
      }.toMap
      (lines, notes)

  /** The least privileged permission mode `providerId` supports; the run needs no tools at all. */
  private def permissionMode(providerId: String): Future[Option[String]] =
    bb.sdk.asInstanceOf[js.Dynamic].providers.list().asInstanceOf[js.Promise[js.Array[js.Dynamic]]].toFuture
      .map { providers =>
        providers.find(p => str(p.id) == providerId).flatMap { p =>
          val supported = p.capabilities.permissionModes.asInstanceOf[js.Array[String]].toVector
          PermissionPreference.find(supported.contains)
        }
      }
      .recover { case _ => None }

  /** Run the prompt in a hidden thread and return its output; the thread is stopped and deleted on every path. */
  private def runWorker(
    projectId: String,
    environmentId: Option[String],
    sel: ModelSelection,
    text: String): Future[String] =
    val threads                                                         = bb.sdk.threads.asInstanceOf[js.Dynamic]
    def environment(reuse: Boolean)                                     =
      environmentId.filter(_ => reuse) match
        case Some(id) => js.Dynamic.literal("type" -> "reuse", "environmentId" -> id)
        case None     => js.Dynamic.literal("type" -> "project-default")
    def spawn(mode: Option[String], reuse: Boolean): Future[js.Dynamic] =
      val sources = js.Dynamic.literal("providerId" -> "explicit", "model" -> "explicit")
      val args    = js.Dynamic.literal(
        "projectId"             -> projectId,
        "environment"           -> environment(reuse),
        "prompt"                -> text,
        "title"                 -> "Billing summaries",
        "visibility"            -> "hidden",
        "providerId"            -> sel.providerId,
        "model"                 -> sel.model,
        "executionInputSources" -> sources
      )
      sel.reasoningLevel.foreach { level =>
        args.updateDynamic("reasoningLevel")(level)
        sources.updateDynamic("reasoningLevel")("explicit")
      }
      mode.foreach { m =>
        args.updateDynamic("permissionMode")(m)
        sources.updateDynamic("permissionMode")("explicit")
      }
      threads.spawn(args).asInstanceOf[js.Promise[js.Dynamic]].toFuture
    def dispose(id: String): Future[Unit]                               =
      threads.stop(js.Dynamic.literal("threadId" -> id)).asInstanceOf[js.Promise[js.Dynamic]].toFuture
        .recover { case _ => () }
        .flatMap(_ =>
          threads.delete(js.Dynamic.literal("threadId" -> id, "childThreadsConfirmed" -> true))
            .asInstanceOf[js.Promise[js.Dynamic]].toFuture.map(_ => ())
            .recoverWith { case _ =>
              threads.archive(js.Dynamic.literal("threadId" -> id)).asInstanceOf[js.Promise[js.Dynamic]].toFuture
                .map(_ => ()).recover { case _ => () }
            }
        )
    for
      mode   <- permissionMode(sel.providerId)
      // An archived thread's environment may be gone; then the project's default serves.
      worker <-
        spawn(mode, reuse = true).recoverWith { case _ if environmentId.isDefined => spawn(mode, reuse = false) }
      id = str(worker.id)
      output <- (for
                  done <- threads.applyDynamic("wait")(js.Dynamic.literal(
                            "threadId"  -> id,
                            "event"     -> "turn/completed",
                            "timeoutMs" -> TimeoutMs
                          ))
                            .asInstanceOf[js.Promise[js.Dynamic]].toFuture
                            .recoverWith { case e =>
                              Future.failed(new RuntimeException(
                                s"The model did not answer within ${(TimeoutMs / 1000).toInt}s on ${sel.providerId}/${sel.model} (${errorMessage(e)})"
                              ))
                            }
                  _    <- Option(done.event).filter(isDefined).map(_.data) match
                            case Some(data) if str(data.status) != "completed" =>
                              val detail = Option(data.error).filter(isDefined).map(e => str(e.message)).filter(_.nonEmpty)
                              Future.failed(new RuntimeException(
                                s"The model stopped without answering (${detail.getOrElse(str(data.status))})"
                              ))
                            case _                                             => Future.unit
                  out  <-
                    threads.output(js.Dynamic.literal("threadId" -> id)).asInstanceOf[js.Promise[js.Dynamic]].toFuture
                yield Option(str(out.output)).getOrElse(""))
                  .transformWith(result => dispose(id).flatMap(_ => Future.fromTry(result)))
    yield output
