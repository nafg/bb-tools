package billingcore

/** What a reply is about; the page says how it reads each one. */
object Reply:
  sealed trait Ok
  sealed trait Overview
  sealed trait Ledger
  sealed trait Plan
  sealed trait Submission
  sealed trait Deleted
  sealed trait Submissions
  sealed trait Catalog
  sealed trait Mapping
  sealed trait ProjectMappings
  sealed trait Report
  sealed trait Audit
  sealed trait SessionScreen
  sealed trait SessionShared

  /** How many shared stretches a one-click settle decided. */
  sealed trait Decided

/** A stretch of clock time, `[start, end)` in epoch milliseconds. */
final case class Period(start: Long, end: Long)

/** Everything the page can ask the plugin: one case per method, carrying exactly the input that method needs. The wire
  * form is `(method, input)`; `encode` and `decode` are the only two places that know its field names.
  */
enum BillingRpc[Reply]:
  /** The threads with recorded time since `since`, with where each bills and what is unbilled. */
  case Overview(since: Long) extends BillingRpc[Reply.Overview]

  /** The sessions of the recent past, and the stretches of every thread in one of them: the session starting at `at`,
    * else the latest.
    */
  case SessionScreen(at: Option[Long]) extends BillingRpc[Reply.SessionScreen]

  /** The undecided shared stretches reaching into `period` in the ledger of each of `roots` (with its subthreads): what
    * must be decided before their time there can be sent, whatever is ticked.
    */
  case SessionShared(roots: Vector[String], period: Period) extends BillingRpc[Reply.SessionShared]

  /** The ledger of a thread and, when asked, of its subthreads that bill to the same place. */
  case Ledger(threadId: String, subthreads: Boolean) extends BillingRpc[Reply.Ledger]

  /** Prices the draft: the entries with their notes, and what stops a send. */
  case Draft(threadId: String, subthreads: Boolean, pick: DraftPick) extends BillingRpc[Reply.Plan]

  /** One model run that writes the draft's missing lines and its days' notes. The page's request answers at once and
    * `BillingEvent.SummariesWritten` or `SummariesFailed` says when the run ended; the CLI and the agent wait for it.
    */
  case Summarize(threadId: String, subthreads: Boolean, pick: DraftPick) extends BillingRpc[Reply.Ok]

  /** Strikes a stretch out of every draft, or puts it back. */
  case SetExcluded(windowKey: String, excluded: Boolean) extends BillingRpc[Reply.Ok]

  /** The user's own notes for a day's entry; `None` returns to the generated notes. */
  case SetNotes(threadId: String, day: CivilDate, notes: Option[String]) extends BillingRpc[Reply.Ok]

  /** Who gets a shared stretch of the thread's ledger. */
  case Decide(threadId: String, subthreads: Boolean, key: BlockKey, allocation: Allocation)
      extends BillingRpc[Reply.Ok]

  case Undecide(key: BlockKey) extends BillingRpc[Reply.Ok]

  /** Every undecided shared stretch of the ledger with exactly one client thread goes to that thread: of those
    * reaching into `within`, when given.
    */
  case SettleAll(threadId: String, subthreads: Boolean, within: Option[Period]) extends BillingRpc[Reply.Decided]

  /** Sends the draft to Zoho, or settles it without billing. */
  case Submit(threadId: String, subthreads: Boolean, pick: DraftPick, billed: Boolean)
      extends BillingRpc[Reply.Submission]

  /** Deletes a submission's Zoho entries and returns its stretches to unbilled. */
  case Undo(submissionId: String) extends BillingRpc[Reply.Deleted]

  /** Whether a past submission billed time that was already in Zoho. */
  case Audit(submissionId: String) extends BillingRpc[Reply.Audit]

  case Submissions extends BillingRpc[Reply.Submissions]

  /** Zoho's customers, projects and tasks, from the cache unless `refresh`. */
  case Catalog(refresh: Boolean) extends BillingRpc[Reply.Catalog]

  /** Where a thread's time bills and which rule decides it. */
  case Mapping(threadId: String) extends BillingRpc[Reply.Mapping]

  case Assign(threadId: String, projectId: String, taskId: String)     extends BillingRpc[Reply.Ok]
  case MarkUnbillable(threadId: String)                                extends BillingRpc[Reply.Ok]
  case ClearMapping(threadId: String)                                  extends BillingRpc[Reply.Ok]
  case ProjectMappings                                                 extends BillingRpc[Reply.ProjectMappings]
  case SetProjectMapping(projectId: String, zoho: Vector[ProjectZoho]) extends BillingRpc[Reply.Ok]
  case SetProjectNotClientWork(projectId: String, notClientWork: Boolean) extends BillingRpc[Reply.Ok]
  case Report(customerId: String, from: CivilDate, to: CivilDate)      extends BillingRpc[Reply.Report]

  private def encodePeriod(p: Period): Wire =
    Wire.obj("start" -> Wire.Num(p.start.toDouble), "end" -> Wire.Num(p.end.toDouble))

  /** The method name on the wire. */
  def method: String =
    val name = productPrefix
    name.head.toLower +: name.tail

  /** The input on the wire. */
  def encode: Wire.Obj =
    import Wire.{Bool, Num, Str}
    this match
      case Overview(since)                   => Wire.obj("since" -> Num(since.toDouble))
      case SessionScreen(at)                 => Wire.obj("at" -> Wire.opt(at.map(a => Num(a.toDouble))))
      case SessionShared(roots, period)      => Wire.obj("roots" -> Wire.strings(roots), "period" -> encodePeriod(period))
      case Ledger(threadId, subthreads)      => Wire.obj("threadId" -> Str(threadId), "subthreads" -> Bool(subthreads))
      case Draft(threadId, subthreads, pick) =>
        Wire.obj("threadId" -> Str(threadId), "subthreads" -> Bool(subthreads), "pick" -> pick.encode)
      case Summarize(threadId, subthreads, pick) =>
        Wire.obj("threadId" -> Str(threadId), "subthreads" -> Bool(subthreads), "pick" -> pick.encode)
      case SetExcluded(windowKey, excluded) => Wire.obj("windowKey" -> Str(windowKey), "excluded" -> Bool(excluded))
      case SetNotes(threadId, day, notes)   =>
        Wire.obj("threadId" -> Str(threadId), "day" -> Str(day.iso), "notes" -> Wire.opt(notes.map(Str.apply)))
      case Decide(threadId, subthreads, key, allocation) =>
        Wire.obj(
          "threadId"   -> Str(threadId),
          "subthreads" -> Bool(subthreads),
          "key"        -> Str(key.text),
          "allocation" -> allocation.encode
        )
      case Undecide(key)                                 => Wire.obj("key" -> Str(key.text))
      case SettleAll(threadId, subthreads, within)       =>
        Wire.obj(
          "threadId"   -> Str(threadId),
          "subthreads" -> Bool(subthreads),
          "within"     -> Wire.opt(within.map(encodePeriod))
        )
      case Submit(threadId, subthreads, pick, billed)    =>
        Wire.obj(
          "threadId"   -> Str(threadId),
          "subthreads" -> Bool(subthreads),
          "pick"       -> pick.encode,
          "billed"     -> Bool(billed)
        )
      case Undo(submissionId)                            => Wire.obj("submissionId" -> Str(submissionId))
      case Audit(submissionId)                           => Wire.obj("submissionId" -> Str(submissionId))
      case Submissions                                   => Wire.obj()
      case Catalog(refresh)                              => Wire.obj("refresh" -> Bool(refresh))
      case Mapping(threadId)                             => Wire.obj("threadId" -> Str(threadId))
      case Assign(threadId, projectId, taskId)           =>
        Wire.obj("threadId" -> Str(threadId), "projectId" -> Str(projectId), "taskId" -> Str(taskId))
      case MarkUnbillable(threadId)                      => Wire.obj("threadId" -> Str(threadId))
      case ClearMapping(threadId)                        => Wire.obj("threadId" -> Str(threadId))
      case ProjectMappings                               => Wire.obj()
      case SetProjectNotClientWork(projectId, on)        =>
        Wire.obj("projectId" -> Str(projectId), "notClientWork" -> Wire.Bool(on))
      case SetProjectMapping(projectId, zoho)            =>
        Wire.obj(
          "projectId" -> Str(projectId),
          "zoho"      -> Wire.Arr(zoho.map(z => Wire.obj("projectId" -> Str(z.projectId), "taskId" -> Str(z.taskId))))
        )
      case Report(customerId, from, to)                  =>
        Wire.obj("customerId" -> Str(customerId), "from" -> Str(from.iso), "to" -> Str(to.iso))

/** What the plugin tells the page on the "billing" realtime channel. */
enum BillingEvent:
  /** These changes were made: what was computed from what they touch is stale (see `Inputs.affectedBy`). */
  case Changed(changes: Vector[Change])

  /** The run `BillingRpc.Summarize` started for this thread wrote its lines and notes. */
  case SummariesWritten(threadId: String)

  case SummariesFailed(threadId: String, error: String)

  def encode: Wire.Obj =
    this match
      case Changed(changes)                 =>
        Wire.obj("type" -> Wire.Str("changed"), "changes" -> Wire.Arr(changes.map(_.encode)))
      case SummariesWritten(threadId)       =>
        Wire.obj("type" -> Wire.Str("summariesWritten"), "threadId" -> Wire.Str(threadId))
      case SummariesFailed(threadId, error) =>
        Wire.obj("type" -> Wire.Str("summariesFailed"), "threadId" -> Wire.Str(threadId), "error" -> Wire.Str(error))

object BillingEvent:
  def decode(value: Wire): Option[BillingEvent] =
    def field(name: String) = value.field(name).flatMap(_.asStr)
    field("type") match
      case Some("changed")          =>
        value.field("changes").flatMap(_.asArr).map(cs => Changed(cs.flatMap(Change.decode)))
      case Some("summariesWritten") => field("threadId").map(SummariesWritten.apply)
      case Some("summariesFailed")  =>
        for threadId <- field("threadId"); error <- field("error") yield SummariesFailed(threadId, error)
      case _                        => None

object BillingRpc:
  /** Every method name, in the order the plugin registers them. */
  val methods: Vector[String] = Vector(
    "overview",
    "sessionScreen",
    "sessionShared",
    "ledger",
    "draft",
    "summarize",
    "setExcluded",
    "setNotes",
    "decide",
    "undecide",
    "settleAll",
    "submit",
    "undo",
    "audit",
    "submissions",
    "catalog",
    "mapping",
    "assign",
    "markUnbillable",
    "clearMapping",
    "projectMappings",
    "setProjectMapping",
    "setProjectNotClientWork",
    "report"
  )

  /** The request a method name and its input mean, or what is wrong with the input. */
  def decode(method: String, input: Wire): Either[String, BillingRpc[?]] =
    def required(name: String): Either[String, String] =
      input.field(name).flatMap(_.asStr).filter(_.nonEmpty).toRight(s"$name is required")
    def flag(name: String, default: Boolean): Boolean  = input.field(name).flatMap(_.asBool).getOrElse(default)
    def pick: Either[String, DraftPick]                =
      input.field("pick").flatMap(DraftPick.decode).toRight("pick must be the keys to draft or \"all\"")
    def date(name: String): Either[String, CivilDate]  =
      required(name).flatMap(s => CivilDate.parseOption(s).toRight(s"$name must be yyyy-MM-dd"))
    def blockKey: Either[String, BlockKey]             =
      required("key").flatMap(s => BlockKey.parse(s).toRight("key must name a shared stretch"))
    def subthreads: Boolean                            = flag("subthreads", default = true)
    def period(value: Wire): Option[Period]            =
      for s <- value.field("start").flatMap(_.asNum); e <- value.field("end").flatMap(_.asNum)
      yield Period(s.toLong, e.toLong)

    method match
      case "overview"          =>
        input.field("since").flatMap(_.asNum).map(_.toLong).toRight("since is required").map(Overview.apply)
      case "sessionScreen"     => Right(SessionScreen(input.field("at").flatMap(_.asNum).map(_.toLong)))
      case "sessionShared"     =>
        for
          roots <- input.field("roots").flatMap(_.asArr).map(_.flatMap(_.asStr)).toRight("roots is required")
          p     <- input.field("period").flatMap(period).toRight("period must have a start and an end")
        yield SessionShared(roots, p)
      case "ledger"            => required("threadId").map(Ledger(_, subthreads))
      case "draft"             => for threadId <- required("threadId"); p <- pick yield Draft(threadId, subthreads, p)
      case "summarize"         =>
        for threadId <- required("threadId"); p <- pick yield Summarize(threadId, subthreads, p)
      case "setExcluded"       => required("windowKey").map(SetExcluded(_, flag("excluded", default = true)))
      case "setNotes"          =>
        for threadId <- required("threadId"); day <- date("day")
        yield SetNotes(threadId, day, input.field("notes").flatMap(_.asStr).map(_.trim).filter(_.nonEmpty))
      case "decide"            =>
        for
          threadId   <- required("threadId")
          key        <- blockKey
          allocation <- input.field("allocation").flatMap(Allocation.decode)
                          .toRight("allocation must be wholeTo, portions, or nobody")
        yield Decide(threadId, subthreads, key, allocation)
      case "undecide"          => blockKey.map(Undecide.apply)
      case "settleAll"         =>
        required("threadId").map(SettleAll(_, subthreads, input.field("within").flatMap(period)))
      case "submit"            =>
        for threadId <- required("threadId"); p <- pick
        yield Submit(threadId, subthreads, p, flag("billed", default = false))
      case "undo"              => required("submissionId").map(Undo.apply)
      case "audit"             => required("submissionId").map(Audit.apply)
      case "submissions"       => Right(Submissions)
      case "catalog"           => Right(Catalog(flag("refresh", default = false)))
      case "mapping"           => required("threadId").map(Mapping.apply)
      case "assign"            =>
        for threadId <- required("threadId"); projectId <- required("projectId"); taskId <- required("taskId")
        yield Assign(threadId, projectId, taskId)
      case "markUnbillable"    => required("threadId").map(MarkUnbillable.apply)
      case "clearMapping"      => required("threadId").map(ClearMapping.apply)
      case "projectMappings"   => Right(ProjectMappings)
      case "setProjectMapping" =>
        required("projectId").map(projectId =>
          SetProjectMapping(
            projectId,
            input.field("zoho").flatMap(_.asArr).getOrElse(Vector.empty).flatMap(z =>
              for
                p <- z.field("projectId").flatMap(_.asStr).filter(_.nonEmpty);
                t <- z.field("taskId").flatMap(_.asStr).filter(_.nonEmpty)
              yield ProjectZoho(p, t)
            )
          )
        )
      case "setProjectNotClientWork" =>
        required("projectId").map(projectId =>
          SetProjectNotClientWork(projectId, input.field("notClientWork").flatMap(_.asBool).getOrElse(false))
        )
      case "report"            =>
        for customerId <- required("customerId"); from <- date("from"); to <- date("to")
        yield Report(customerId, from, to)
      case other               => Left(s"unknown method $other")
