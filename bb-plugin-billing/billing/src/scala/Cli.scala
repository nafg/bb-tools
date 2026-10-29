package billing

import bbplugin.BbApi
import bbplugin.Dyn.str
import billingcore.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** `bb billing …`: everything the page does, for agents and the terminal. Sending to Zoho is the one step that always
  * pauses for the user's confirmation in the thread, through the `billing_send_thread` tool or `bb billing send`.
  */
object Cli:
  private def ok(stdout: String): js.Dynamic   = js.Dynamic.literal("exitCode" -> 0, "stdout" -> stdout)
  private def err(message: String): js.Dynamic = js.Dynamic.literal("exitCode" -> 1, "stderr" -> message)

  private def parseArgs(argv: List[String]): (Map[String, String], List[String]) =
    @annotation.tailrec
    def loop(
      rest: List[String],
      flags: Map[String, String],
      positional: List[String]): (Map[String, String], List[String]) =
      rest match
        case flag :: value :: tail if flag.startsWith("--") && !value.startsWith("--") =>
          loop(tail, flags.updated(flag.drop(2), value), positional)
        case flag :: tail if flag.startsWith("--") => loop(tail, flags.updated(flag.drop(2), "true"), positional)
        case value :: tail                         => loop(tail, flags, positional :+ value)
        case Nil                                   => (flags, positional)
    loop(argv, Map.empty, Nil)

  private val commandTable = Vector(
    (
      "status",
      "Whether Zoho is connected and the activity recorder is reachable",
      "bb billing status"
    ),
    (
      "pending",
      "Archived client threads that still have unbilled recorded time: each needs a decision",
      "bb billing pending [--since yyyy-MM-dd]"
    ),
    (
      "threads",
      "Threads with recorded time since a date, as a tree by bb project, with where each bills and what is unbilled",
      "bb billing threads [--since yyyy-MM-dd]"
    ),
    (
      "plan",
      "The thread's ledger and what sending a draft would do: stretches with keys and a line each, stretches to settle, dated entries, time already in Zoho by hand; --keys picks the draft (default every unbilled stretch); --summaries has a model describe the stretches first",
      "bb billing plan <thread> [--keys <k1,k2,…>] [--no-subthreads] [--summaries]"
    ),
    (
      "exclude",
      "Leave a window out of the thread's billing (it stays listed, struck through)",
      "bb billing exclude <window key>"
    ),
    ("include", "Put a window back", "bb billing include <window key>"),
    (
      "settle",
      "Say who gets a shared stretch: one thread, a split by minutes, or nobody",
      "bb billing settle <thread> <stretch key> (--to <thread> | --split <thread>=<minutes>[,…] | --nobody)"
    ),
    (
      "settle-all",
      "Give every unsettled stretch of a thread's plan that has exactly one client thread to that thread",
      "bb billing settle-all <thread> [--no-subthreads]"
    ),
    ("unsettle", "Take back a stretch's decision", "bb billing unsettle <stretch key>"),
    (
      "notes",
      "Replace the notes of one dated entry before sending (empty text restores the generated notes)",
      "bb billing notes <thread> <yyyy-MM-dd> <text>"
    ),
    (
      "send",
      "Send a draft after the user confirms it in the thread (prefer the billing_send_thread tool)",
      "bb billing send <thread> [--keys <k1,k2,…>] [--no-subthreads]"
    ),
    (
      "skip",
      "Mark a draft settled without sending anything",
      "bb billing skip <thread> [--keys <k1,k2,…>] [--no-subthreads]"
    ),
    (
      "undo",
      "Undo a submission, any one in any order: its Zoho entries are deleted and the stretches it covered are unbilled again",
      "bb billing undo <submission id>"
    ),
    ("submissions", "Past submissions, all or for one thread", "bb billing submissions [<thread>]"),
    (
      "audit",
      "Whether a past submission billed time that was already in Zoho: the Zoho timer calls inside its sent range and the pre-existing Zoho entries on its days",
      "bb billing audit <submission id>"
    ),
    (
      "assign",
      "Bill a thread's time (and its descendants' without their own choice) to a Zoho project and task",
      "bb billing assign --project <name|id> --task <name|id> [--thread <id>]"
    ),
    (
      "unbillable",
      "Mark a thread not client work; threads under it stop following anything above",
      "bb billing unbillable [--thread <id>]"
    ),
    (
      "clear",
      "Remove a thread's own choice so it follows its parent, else its bb project's default",
      "bb billing clear [--thread <id>]"
    ),
    ("mapping", "Where a thread's time is billed and which rule decides it", "bb billing mapping [--thread <id>]"),
    (
      "models",
      "The models bb offers for writing summaries, as provider/model for the summaryModel setting",
      "bb billing models"
    ),
    (
      "projects",
      "Active Zoho projects with their customers and tasks (--refresh fetches from Zoho)",
      "bb billing projects [--refresh]"
    ),
    (
      "project-zoho",
      "The Zoho projects a bb project's threads may bill to; with exactly one, it is the default",
      "bb billing project-zoho [<bb project name|id>] [--add <zoho project> [--task <name|id>]] [--remove <zoho project>]"
    ),
    (
      "project-unbillable",
      "Mark a bb project's threads not client work unless they or a parent choose otherwise; --off takes the mark away",
      "bb billing project-unbillable <bb project name|id> [--off]"
    ),
    (
      "report",
      "Time sent to Zoho for a customer in a date range, per thread, as Markdown",
      "bb billing report --customer <name|id> --from yyyy-MM-dd --to yyyy-MM-dd"
    )
  )

  def register(
    bb: BbApi,
    service: Service,
    commands: Commands,
    projectMappings: ProjectMappings,
    zoho: Zoho,
    summaries: Summaries
  )(using ExecutionContext): Unit =
    bb.cli.register(
      js.Dynamic.literal(
        "name" -> "billing",
        "summary" -> "Bill recorded thread time to Zoho Books: choose where threads bill, review a thread's windows, settle shared time, send up to a moment, report per customer",
        "commands" -> commandTable.map((name, summary, usage) =>
          js.Dynamic.literal("name" -> name, "summary" -> summary, "usage" -> usage)
        ).toJSArray,
        "run"      -> ((argv: js.Array[String], ctx: js.Dynamic) =>
          run(bb, service, commands, projectMappings, zoho, summaries, argv.toList, ctx).toJSPromise
        )
      )
    )

  private def usage: String =
    "Commands:\n" + commandTable.map((_, summary, usage) => s"  $usage\n      $summary").mkString("\n") +
      "\n--thread defaults to the current thread. Window and stretch keys come from `bb billing plan`."

  private def describe(resolved: Resolved): String =
    resolved.billability match
      case Billability.Billable(t) =>
        s"billed to ${t.customerName}: ${t.projectName} / ${t.taskName} (project ${t.projectId}, task ${t.taskId})" +
          (resolved.source match
            case Some(MappingSource.Thread(id)) => s", chosen on thread $id"
            case Some(MappingSource.Project)    => ", a bb project's default"
            case None                           => ""
          )
      case Billability.NotBillable =>
        resolved.source match
          case Some(MappingSource.Thread(id)) => s"not client work: thread $id is marked so"
          case Some(MappingSource.Project)    => "not client work: its bb project is marked so"
          case None                           =>
            "not chosen whether client work: nothing on the thread, above it, or in its bb project says"

  /** `--keys a,b`: the stretches to draft; none means every unbilled stretch. */
  private def pickArg(flags: Map[String, String]): DraftPick =
    flags.get("keys").map(_.split(',').map(_.trim).filter(_.nonEmpty).toSet).filter(_.nonEmpty)
      .fold[DraftPick](DraftPick.AllUnbilled)(DraftPick.Keys.apply)

  private def allocation(flags: Map[String, String]): Either[String, Allocation] =
    (flags.get("to"), flags.get("split"), flags.contains("nobody")) match
      case (Some(t), None, false)    => Right(Allocation.WholeTo(t))
      case (None, Some(spec), false) =>
        val parts = spec.split(',').toList.map(_.trim).filter(_.nonEmpty).map { part =>
          part.split('=') match
            case Array(t, m) => m.trim.toDoubleOption.map(minutes => t.trim -> (minutes * 60_000).toLong).toRight(
                s"not a number of minutes: $m"
              )
            case _           => Left(s"expected <thread>=<minutes>, got: $part")
        }
        parts.collectFirst { case Left(e) => e }.toLeft(Allocation.Portions(parts.collect { case Right(p) => p }.toMap))
      case (None, None, true)        => Right(Allocation.Nobody)
      case _ => Left("say exactly one of --to <thread>, --split <thread>=<minutes>,…, or --nobody")

  private def run(
    bb: BbApi,
    service: Service,
    commands: Commands,
    projectMappings: ProjectMappings,
    zoho: Zoho,
    summaries: Summaries,
    argv: List[String],
    ctx: js.Dynamic
  )(using ExecutionContext): Future[js.Dynamic] =
    val (flags, positional)                                                            = parseArgs(argv.drop(1))
    def thread: Either[String, String]                                                 =
      flags.get("thread").orElse(Option(str(ctx.threadId)).filter(_.nonEmpty)).toRight(
        "no current thread; pass --thread <id>"
      )
    def orErr[A](e: Either[String, A])(f: A => Future[js.Dynamic]): Future[js.Dynamic] =
      e.fold(m => Future.successful(err(m)), f)
    def overview: Future[Either[String, Overview]]                                     =
      service.calendar().flatMap { (cal, _) =>
        val since = flags.get("since").flatMap(CivilDate.parseOption).map(d => cal.startOfDay(d.iso)).getOrElse(
          js.Date.now().toLong - 28L * 24 * 3600_000
        )
        commands.run(BillingRpc.Overview(since)).map(_.overview)
      }
    val subthreads                                                        = !flags.contains("no-subthreads")
    def planFor(root: String, withSummaries: Boolean): Future[PlanResult] =
      val described =
        if withSummaries then commands.run(BillingRpc.Summarize(root, subthreads, pickArg(flags)))
        else Future.unit
      described.flatMap(_ => commands.run(BillingRpc.Draft(root, subthreads, pickArg(flags))))
    val rootArg = positional.headOption.toRight("a thread id is required")

    val result: Future[js.Dynamic] = argv.headOption match
      case Some("status")                    =>
        for
          missing <- zoho.missingSettings()
          rec     <- service.recorderStatus()
          zone    <- zoho.timeZone()
          sel     <- summaries.selection().recover { case _ => None }
        yield ok(Vector(
          if missing.isEmpty then "Zoho Books: connected"
          else s"Zoho Books: not connected (${missing.mkString(", ")} missing in the plugin settings)",
          if rec.available then "Activity recorder: reachable"
          else s"Activity recorder: unavailable (${rec.error.getOrElse("unknown error")})",
          sel match
            case Some(m) =>
              s"Summaries: written by ${m.providerId}/${m.model}${m.reasoningLevel.fold("")(" at " + _)} in a hidden thread when asked (plan --summaries)"
            case None    => "Summaries: no model available; lines are your first message in each stretch"
          ,
          s"Days run midnight to midnight in ${zone._1}" + (if zone._2 == ZoneSource.Server then
                                                              " (the server's zone until Zoho is connected)"
                                                            else "")
        ).mkString("\n"))
      case Some("models")                    =>
        summaries.models().map(ms =>
          if ms.isEmpty then err("bb offers no models.\n")
          else
            ok(ms.map(m =>
              s"${m.providerId}/${m.model}  ${m.displayName}${if m.isDefault then " (bb default)" else ""}"
            ).mkString("\n") + "\n")
        )
      case Some("pending")                   => overview.map(_.fold(err, o => ok(Text.pending(o))))
      case Some("threads")                   => overview.map(_.fold(err, o => ok(Text.threads(o))))
      case Some("plan")                      =>
        orErr(rootArg)(root =>
          planFor(root, withSummaries = flags.contains("summaries")).map(p => ok(Text.plan(p, service.blockers(p))))
        )
      case Some("exclude") | Some("include") =>
        orErr(positional.headOption.filter(_.count(_ == '|') == 2).toRight(
          "a window key from `bb billing plan` is required"
        )) { key =>
          val out = argv.headOption.contains("exclude")
          commands.run(BillingRpc.SetExcluded(key, out)).map(_ =>
            ok(if out then "Window left out." else "Window included.")
          )
        }
      case Some("settle")                    =>
        orErr(for
          root <- rootArg
          key  <- positional.lift(1).flatMap(BlockKey.parse).toRight("a stretch key from `bb billing plan` is required")
          alloc <- allocation(flags)
        yield (root, key, alloc)) { (root, key, alloc) =>
          commands.run(BillingRpc.Decide(root, subthreads, key, alloc)).map(_ => ok("Settled."))
        }
      case Some("settle-all")                =>
        orErr(rootArg) { root =>
          commands.run(BillingRpc.SettleAll(root, subthreads)).map(n =>
            ok(s"Settled $n stretch(es) that had exactly one client thread.")
          )
        }
      case Some("unsettle")                  =>
        orErr(positional.headOption.flatMap(BlockKey.parse).toRight("a stretch key is required")) { key =>
          commands.run(BillingRpc.Undecide(key)).map(_ => ok("Decision removed."))
        }
      case Some("notes")                     =>
        orErr(for
          root <- rootArg
          day  <- positional.lift(1).flatMap(CivilDate.parseOption).toRight("a day as yyyy-MM-dd is required")
        yield (root, day)) { (root, day) =>
          val text = positional.drop(2).mkString(" ").trim
          commands.run(BillingRpc.SetNotes(root, day, Option(text).filter(_.nonEmpty)))
            .map(_ => ok(if text.isEmpty then "Generated notes restored." else "Notes set."))
        }
      case Some("send")                      =>
        orErr(rootArg) { root =>
          val threadId = str(ctx.threadId)
          if threadId.isEmpty then
            Future.successful(
              err("send must run from a bb thread, where the user confirms the plan; or use the Billing page")
            )
          else
            SendConfirmation.run(bb, service, commands, threadId, root, pickArg(flags), subthreads, ctx.signal)
              .map(_.fold(err, ok))
        }
      case Some("skip")                      =>
        orErr(rootArg) { root =>
          commands.run(BillingRpc.Submit(root, subthreads, pickArg(flags), billed = false)).map(s =>
            ok(
              s"Marked settled without billing: ${s.spans.map(_.threadId).distinct.size} thread(s), submission ${s.id}."
            )
          )
        }
      case Some("undo")                      =>
        orErr(positional.headOption.toRight("a submission id is required"))(id =>
          commands.run(BillingRpc.Undo(id)).map(n =>
            ok(s"Undone: $n Zoho entries deleted; the stretches it covered are unbilled again.")
          )
        )
      case Some("audit")                     =>
        orErr(positional.headOption.toRight("a submission id is required (see bb billing submissions)"))(id =>
          commands.run(BillingRpc.Audit(id)).map(a => ok(Text.audit(a)))
        )
      case Some("submissions")               =>
        for
          (cal, _) <- service.calendar()
          reply    <- commands.run(BillingRpc.Submissions)
          subs = positional.headOption.fold(reply.submissions)(t =>
                   reply.submissions.filter(s => s.rootThreadId == t || s.spans.exists(_.threadId == t))
                 )
        yield ok(if subs.isEmpty then "No submissions." else subs.map(Text.submission(_, cal)).mkString("\n"))
      case Some("assign")                    =>
        orErr(for
          threadId <- thread
          project  <- flags.get("project").toRight("--project is required")
          task     <- flags.get("task").toRight("--task is required")
        yield (threadId, project, task)) { (threadId, projectText, taskText) =>
          zoho.catalog(refresh = false).flatMap { catalog =>
            orErr(for
              project <- catalog.findProject(projectText)
              task    <- catalog.findTask(project, taskText)
            yield (project, task)) { (project, task) =>
              val p = project.project
              commands.run(BillingRpc.Assign(threadId, p.projectId, task.taskId)).map(_ =>
                ok(s"Thread $threadId now bills to ${p.customerName}: ${p.projectName} / ${task.taskName}")
              )
            }
          }
        }
      case Some("unbillable")                => orErr(thread)(threadId =>
          commands.run(BillingRpc.MarkUnbillable(threadId)).map(_ => ok(s"Thread $threadId is marked not client work"))
        )
      case Some("clear")                     => orErr(thread)(threadId =>
          commands.run(BillingRpc.ClearMapping(threadId)).map(_ =>
            ok(s"Thread $threadId has no choice of its own now; it follows its parent, else its bb project's default")
          )
        )
      case Some("mapping")                   =>
        orErr(thread)(threadId =>
          commands.run(BillingRpc.Mapping(threadId)).map(m => ok(s"Thread $threadId is ${describe(m.resolved)}"))
        )
      case Some("projects")                  =>
        zoho.catalog(refresh = flags.contains("refresh")).map { catalog =>
          val lines = catalog.projects.map { p =>
            val tasks =
              if p.tasks.isEmpty then "(no tasks)" else p.tasks.map(t => s"${t.taskName} [${t.taskId}]").mkString(", ")
            s"${p.project.customerName}: ${p.project.projectName} [${p.project.projectId}]\n    tasks: $tasks"
          }
          ok(if lines.isEmpty then "No active Zoho projects." else lines.mkString("\n"))
        }
      case Some("project-zoho")              => projectZoho(bb, projectMappings, zoho, positional.headOption, flags)
      case Some("project-unbillable")        =>
        positional.headOption.fold(Future.successful(err("a bb project name or id is required"))) { text =>
          bb.sdk.projects.list(js.Dynamic.literal("includePersonal" -> true)).toFuture.flatMap { list =>
            list.toVector.map(p => str(p.id) -> str(p.name)).find((id, name) =>
              id == text || name.equalsIgnoreCase(text)
            ) match
              case None             => Future.successful(err(s"no bb project named \"$text\""))
              case Some((id, name)) =>
                val on = !flags.contains("off")
                projectMappings.setNotClientWork(id, on).map(_ =>
                  ok(if on then s"$name: not client work unless a thread chooses otherwise"
                     else s"$name: no longer marked not client work")
                )
          }
        }
      case Some("report")                    =>
        orErr(for
          customer <- flags.get("customer").toRight("--customer is required")
          from     <- flags.get("from").flatMap(CivilDate.parseOption).toRight("--from must be yyyy-MM-dd")
          to       <- flags.get("to").flatMap(CivilDate.parseOption).toRight("--to must be yyyy-MM-dd")
        yield (customer, from, to)) { (customerText, from, to) =>
          zoho.cachedCatalog().flatMap { catalog =>
            val customerId = catalog.flatMap(_.customers.find((id, name) =>
              id == customerText || name.equalsIgnoreCase(customerText)
            ).map(_._1)).getOrElse(customerText)
            commands.run(BillingRpc.Report(customerId, from, to)).map(r => ok(r.markdown))
          }
        }
      case other                             => Future.successful(
          err(s"unknown command: ${other.getOrElse("")} (received ${argv.mkString("[", ", ", "]")})\n$usage")
        )
    result.recover { case e => err(errorMessage(e)) }

  private def projectZoho(
    bb: BbApi,
    projectMappings: ProjectMappings,
    zoho: Zoho,
    which: Option[String],
    flags: Map[String, String]
  )(using ExecutionContext): Future[js.Dynamic] =
    for
      bbProjects <- bb.sdk.projects.list(js.Dynamic.literal("includePersonal" -> true)).toFuture.map(_.toVector.map(p =>
                      str(p.id) -> str(p.name)
                    ))
      all        <- projectMappings.all()
      catalog    <- zoho.cachedCatalog()
      result     <- which match
                      case None       =>
                        val lines = bbProjects.map { (id, name) =>
                          val zs = all.getOrElse(id, Vector.empty).map(z =>
                            catalog.flatMap(_.target(z.projectId, z.taskId)).fold(s"${z.projectId}/${z.taskId}")(t =>
                              s"${t.customerName}: ${t.projectName} / ${t.taskName}"
                            )
                          )
                          s"- $name [$id]: " + (if zs.isEmpty then "no Zoho project"
                                                else
                                                  zs.mkString("; ") + (if zs.size == 1 then " (default for its threads)"
                                                                       else " (threads choose)")
                          )
                        }
                        Future.successful(ok(lines.mkString("\n")))
                      case Some(text) =>
                        bbProjects.find((id, name) => id == text || name.equalsIgnoreCase(text)) match
                          case None                 => Future.successful(err(s"no bb project named \"$text\""))
                          case Some((bbId, bbName)) =>
                            val current = all.getOrElse(bbId, Vector.empty)
                            (flags.get("add"), flags.get("remove")) match
                              case (Some(zohoText), _)    =>
                                zoho.catalog(refresh = false).flatMap { cat =>
                                  cat.findProject(zohoText) match
                                    case Left(m)        => Future.successful(err(m))
                                    case Right(project) =>
                                      flags.get("task").fold(
                                        project.tasks.headOption.toRight(s"${project.project.projectName} has no tasks")
                                      )(t => cat.findTask(project, t)) match
                                        case Left(m)  => Future.successful(err(m))
                                        case Right(t) =>
                                          val next =
                                            current.filterNot(_.projectId == project.project.projectId) :+ ProjectZoho(
                                              project.project.projectId,
                                              t.taskId
                                            )
                                          projectMappings.set(bbId, next).map(_ =>
                                            ok(s"$bbName now bills to ${project.project.customerName}: ${project.project.projectName} / ${t.taskName}" + (if next.size == 1
                                                                                                                                                          then " by default"
                                                                                                                                                          else
                                                                                                                                                            s" among ${next.size} Zoho projects"
                                            ))
                                          )
                                }
                              case (None, Some(zohoText)) =>
                                val keep = current.filterNot(z =>
                                  z.projectId == zohoText || catalog.flatMap(_.target(z.projectId, z.taskId)).exists(
                                    _.projectName.equalsIgnoreCase(zohoText)
                                  )
                                )
                                if keep.size == current.size then
                                  Future.successful(err(s"$bbName does not bill to \"$zohoText\""))
                                else projectMappings.set(bbId, keep).map(_ => ok(s"$bbName no longer bills to $zohoText"))
                              case (None, None)           =>
                                val zs = current.map(z =>
                                  catalog.flatMap(_.target(z.projectId, z.taskId)).fold(s"${z.projectId}/${z.taskId}")(t =>
                                    s"${t.customerName}: ${t.projectName} / ${t.taskName}"
                                  )
                                )
                                Future.successful(ok(s"$bbName: " + (if zs.isEmpty then "no Zoho project"
                                                                     else zs.mkString("; "))))
    yield result
