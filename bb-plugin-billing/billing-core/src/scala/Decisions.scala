package billingcore

/** How the reviewer divided one overlap block. */
enum Allocation:
  /** The whole block goes to one of its threads. */
  case WholeTo(threadId: String)

  /** Each listed thread gets this many milliseconds; the rest of the block is billed to nobody. */
  case Portions(ms: Map[String, Long])

  /** None of the block is billed. */
  case Nobody

  /** What each thread receives from a block of `blockMs`. */
  def share(blockMs: Long): Map[String, Long] =
    this match
      case WholeTo(threadId) => Map(threadId -> blockMs)
      case Portions(ms)      => ms
      case Nobody            => Map.empty

  /** The wire and storage form: `{"kind":"wholeTo","threadId":…}`, `{"kind":"portions","ms":{…}}`, `{"kind":"nobody"}`.
    */
  def encode: Wire.Obj =
    this match
      case WholeTo(threadId) => Wire.obj("kind" -> Wire.Str("wholeTo"), "threadId" -> Wire.Str(threadId))
      case Portions(ms)      =>
        Wire.obj("kind" -> Wire.Str("portions"), "ms" -> Wire.Obj(ms.toVector.map((t, v) => t -> Wire.Num(v.toDouble))))
      case Nobody            => Wire.obj("kind" -> Wire.Str("nobody"))

object Allocation:
  def decode(value: Wire): Option[Allocation] =
    value.field("kind").flatMap(_.asStr) match
      case Some("wholeTo")  => value.field("threadId").flatMap(_.asStr).filter(_.nonEmpty).map(WholeTo.apply)
      case Some("portions") =>
        value.field("ms").flatMap(_.asObj).flatMap { entries =>
          val ms = entries.map((t, v) => t -> v.asNum.map(_.toLong))
          Option.when(ms.forall(_._2.nonEmpty))(Portions(ms.map((t, v) => t -> v.get).toMap))
        }
      case Some("nobody")   => Some(Nobody)
      case _                => None

  /** Rejects an allocation that names threads outside the block or hands out more than the block holds. */
  def validate(allocation: Allocation, block: OverlapBlock): Either[String, Allocation] =
    allocation match
      case WholeTo(threadId) if !block.threadIds(threadId) => Left(s"thread $threadId is not part of this overlap")
      case Portions(ms)                                    =>
        val strangers = ms.keySet -- block.threadIds
        if strangers.nonEmpty then Left(s"threads not part of this overlap: ${strangers.toSeq.sorted.mkString(", ")}")
        else if ms.values.exists(_ < 0) then Left("portions cannot be negative")
        else if ms.values.sum > block.durationMs then Left("portions add up to more than the overlap holds")
        else Right(Portions(ms.filter(_._2 > 0)))
      case other                                           => Right(other)

final case class Decision(key: BlockKey, allocation: Allocation)

/** Whether a thread's time is billable, and to what. */
enum Billability:
  case Billable(target: ZohoTarget)
  case NotBillable

final case class ZohoTarget(
  projectId: String,
  projectName: String,
  taskId: String,
  taskName: String,
  customerId: String,
  customerName: String):
  def encode: Wire.Obj =
    Wire.obj(
      "projectId"    -> Wire.Str(projectId),
      "projectName"  -> Wire.Str(projectName),
      "taskId"       -> Wire.Str(taskId),
      "taskName"     -> Wire.Str(taskName),
      "customerId"   -> Wire.Str(customerId),
      "customerName" -> Wire.Str(customerName)
    )

object ZohoTarget:
  def decode(value: Wire): Option[ZohoTarget] =
    def str(name: String) = value.field(name).flatMap(_.asStr)
    for
      projectId <- str("projectId"); projectName <- str("projectName"); taskId      <- str("taskId")
      taskName  <- str("taskName"); customerId   <- str("customerId"); customerName <- str("customerName")
    yield ZohoTarget(projectId, projectName, taskId, taskName, customerId, customerName)
