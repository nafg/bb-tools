package billingcore

/** What a thread's `billing` metadata namespace says about it. */
enum MappingEntry:
  /** `zoho` names a project and task. */
  case Assigned(target: ZohoTarget)

  /** `zoho` is explicitly null: not billable, and neither ancestors nor the project default are consulted. */
  case Unbillable

  /** No `zoho` key: inherit from the nearest ancestor that has one, else the bb project's default. */
  case Absent

/** Which rule decided where a thread's time belongs. */
enum MappingSource:
  /** The entry on this thread (`threadId` is the thread itself) or on the nearest ancestor with one. */
  case Thread(threadId: String)

  /** The rule of the bb project of the topmost thread in the chain that has one (see [[ProjectRule]]). */
  case Project

/** What a bb project says about threads that choose nothing. */
enum ProjectRule:
  /** It maps to exactly one Zoho project with a default task. */
  case Default(target: ZohoTarget)

  /** It is marked not client work. */
  case NotClientWork

object Mapping:
  /** The project rule a thread falls back to: that of the topmost thread in its chain whose bb project has one, so a
    * thread follows its parent rather than its own bb project. `ruleOf` answers for a thread id.
    */
  def projectRuleFor(
    threadId: String,
    ancestorsParentFirst: Seq[String],
    ruleOf: String => Option[ProjectRule]): Option[ProjectRule] =
    (ancestorsParentFirst.reverse :+ threadId).iterator.flatMap(ruleOf(_)).nextOption()

  /** Where a thread's time belongs: its own entry if it has one, else the nearest ancestor's (parent first), else the
    * bb project's default when it has one. Nothing anywhere means not billable.
    */
  def resolve(
    threadId: String,
    ancestorsParentFirst: Seq[String],
    entries: String => MappingEntry,
    projectRule: Option[ProjectRule]): Resolved =
    (threadId +: ancestorsParentFirst).iterator
      .map(t => t -> entries(t))
      .collectFirst {
        case (t, MappingEntry.Assigned(target)) => Resolved(Billability.Billable(target), Some(MappingSource.Thread(t)))
        case (t, MappingEntry.Unbillable)       => Resolved(Billability.NotBillable, Some(MappingSource.Thread(t)))
      }
      .getOrElse(
        projectRule match
          case Some(ProjectRule.Default(target)) => Resolved(Billability.Billable(target), Some(MappingSource.Project))
          case Some(ProjectRule.NotClientWork)   => Resolved(Billability.NotBillable, Some(MappingSource.Project))
          case None                              => Resolved(Billability.NotBillable, None)
      )

/** `source` is the rule that decided the outcome; `None` when no thread in the chain has an entry and the project has
  * no default.
  */
final case class Resolved(billability: Billability, source: Option[MappingSource]):
  def encode: Wire.Obj =
    Wire.obj(
      "target" -> (billability match
        case Billability.Billable(t) => t.encode
        case Billability.NotBillable => Wire.Null
      ),
      "source" -> (source match
        case Some(MappingSource.Thread(id)) => Wire.obj("kind" -> Wire.Str("thread"), "threadId" -> Wire.Str(id))
        case Some(MappingSource.Project)    => Wire.obj("kind" -> Wire.Str("project"))
        case None                           => Wire.Null
      )
    )

object Resolved:
  def decode(value: Wire): Option[Resolved] =
    val source = value.field("source").flatMap(s =>
      s.field("kind").flatMap(_.asStr) match
        case Some("thread")  => s.field("threadId").flatMap(_.asStr).map(MappingSource.Thread.apply)
        case Some("project") => Some(MappingSource.Project)
        case _               => None
    )
    value.field("target") match
      case None    => Some(Resolved(Billability.NotBillable, source))
      case Some(t) => ZohoTarget.decode(t).map(z => Resolved(Billability.Billable(z), source))

/** A bb project's Zoho projects, each with the task a thread is billed to unless it chooses another. */
final case class ProjectZoho(projectId: String, taskId: String)

object ProjectMapping:
  /** The default for threads of a bb project: only when it maps to exactly one Zoho project. */
  def default(mapped: Seq[ProjectZoho], target: (String, String) => Option[ZohoTarget]): Option[ZohoTarget] =
    mapped match
      case Seq(only) => target(only.projectId, only.taskId)
      case _         => None
