package goalscore

final case class GradeRow(threadId: String, goal: Goal, value: Option[Int])
final case class Skipped(line: Int, threadId: String, reason: String)

/** Reads a grades spreadsheet: one row per thread with at least `thread_id` and `goal` columns and optionally `value`. Rows without a goal,
  * or with a goal or value that does not parse, are reported, not imported.
  */
object GradesCsv:
  def parse(text: String): (List[GradeRow], List[Skipped]) =
    val lines = text.linesIterator.filter(_.trim.nonEmpty).toList
    lines match
      case Nil                => (Nil, Nil)
      case headerLine :: rows =>
        val header                                     = splitLine(headerLine).map(_.trim)
        def column(name: String): Either[Skipped, Int] =
          header.indexOf(name) match
            case -1 => Left(Skipped(1, "", s"header lacks column '$name'"))
            case i  => Right(i)
        (for
          idCol   <- column("thread_id")
          goalCol <- column("goal")
        yield
          val valueCol = header.indexOf("value")
          val parsed   = rows.zipWithIndex.map { case (line, i) =>
            val fields       = splitLine(line)
            def at(col: Int) = if col >= 0 && col < fields.size then fields(col).trim else ""
            val id           = at(idCol)
            val lineNo       = i + 2
            val attempt      =
              for
                threadId <- ThreadLabels.parseThreadId(id)
                goal <- (if at(goalCol).isEmpty then Left("no goal") else Goal.parse(at(goalCol)).toRight(s"unknown goal '${at(goalCol)}'"))
                value <- ThreadLabels.parseValueWrite(at(valueCol))
              yield GradeRow(threadId, goal, value)
            attempt.left.map(reason => Skipped(lineNo, id, reason))
          }
          (parsed.collect { case Right(r) => r }, parsed.collect { case Left(s) => s })
        ).fold(skipped => (Nil, List(skipped)), identity)

  /** One CSV line into fields: commas separate, double quotes enclose a field that may hold commas, and a doubled quote inside one is a
    * literal quote.
    */
  def splitLine(line: String): List[String] =
    val fields  = List.newBuilder[String]
    val current = new StringBuilder
    var quoted  = false
    var i       = 0
    while i < line.length do
      val c = line.charAt(i)
      if quoted then
        if c == '"' then
          if i + 1 < line.length && line.charAt(i + 1) == '"' then
            current.append('"')
            i += 1
          else quoted = false
        else current.append(c)
      else if c == '"' then quoted = true
      else if c == ',' then
        fields += current.result()
        current.clear()
      else current.append(c)
      i += 1
    fields += current.result()
    fields.result()
