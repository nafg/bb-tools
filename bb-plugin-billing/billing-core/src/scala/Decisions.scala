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
  /** One allocation for back-to-back pieces of a stretch, each `(its length, its allocation)`: all to one thread or
    * to nobody when every piece says so, else each thread's shares added up.
    */
  def join(pieces: Seq[(Long, Allocation)]): Allocation =
    pieces.map(_._2).distinct match
      case Seq(only @ (WholeTo(_) | Nobody)) => only
      case _                                 =>
        Portions(pieces.flatMap((ms, a) => a.share(ms)).groupMapReduce(_._1)(_._2)(_ + _).filter(_._2 > 0))

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

object Decisions:
  /** The decision of each block: its own, else the joined decisions of pieces of it that cover it exactly, with the
    * same threads. Pieces come from stretches once cut where a ledger cut its own threads' long windows; a block now
    * spans them. A block only partly covered by pieces has no decision.
    */
  def resolve(blocks: Seq[OverlapBlock], stored: Map[BlockKey, Allocation]): Map[BlockKey, Allocation] =
    blocks.flatMap { b =>
      stored.get(b.key).orElse {
        val pieces = piecesOf(b.key, stored.keys).sortBy(_.start)
        val tiles  = pieces.nonEmpty && pieces.head.start == b.start && pieces.last.end == b.end &&
          pieces.zip(pieces.drop(1)).forall((x, y) => x.end == y.start)
        Option.when(tiles)(Allocation.join(pieces.map(k => (k.end - k.start) -> stored(k))))
      }.map(b.key -> _)
    }.toMap

  /** The stored keys other than `key` that lie inside it, with its threads: what a decision on it replaces. */
  def piecesOf(key: BlockKey, stored: Iterable[BlockKey]): Vector[BlockKey] =
    stored.iterator.filter(k =>
      k != key && k.day == key.day && k.threadIds == key.threadIds && key.start <= k.start && k.end <= key.end
    ).toVector

/** A span of one thread's recording that a shared-stretch decision gave to `to` (nobody when empty). */
final case class GivenSpan(span: Span, to: Set[String])

/** The time each thread gave up in decided shared stretches: every thread of a decided stretch that gets none of it. */
final case class GivenAway(spans: Vector[GivenSpan]):
  private val coverage = Coverage.of(spans.map(_.span))

  /** True when the whole of `[start, end)` of `threadId` was given away. */
  def coversAll(threadId: String, start: Long, end: Long): Boolean = coverage.coversAll(threadId, start, end)

  /** Every thread that got some of `[start, end)` of `threadId`. */
  def recipients(threadId: String, start: Long, end: Long): Set[String] =
    spans.iterator.filter(g => g.span.threadId == threadId && g.span.start < end && g.span.end > start)
      .flatMap(_.to).toSet

object GivenAway:
  val none: GivenAway = GivenAway(Vector.empty)

  def of(decisions: Map[BlockKey, Allocation]): GivenAway =
    GivenAway(decisions.toVector.flatMap { (k, a) =>
      val to = a.share(k.end - k.start).filter(_._2 > 0).keySet
      (k.threadIds -- to).toVector.map(t => GivenSpan(Span(t, k.start, k.end), to))
    })

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
