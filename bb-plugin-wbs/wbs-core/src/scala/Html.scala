package wbscore

/** Renders an estimate as an HTML fragment: a tree table with totals, then the
  * open questions. Styling is inline on the elements so the fragment can be
  * pasted into a document or an HTML email as-is; there is no wrapper, script,
  * stylesheet, or external resource. Callers that need a page wrap it.
  */
object Html:
  def render(nodes: List[Node], depth: Option[Int]): String =
    val body      = nodes.flatMap(rows(_, 1, 0, depth))
    val total     = Wbs.estimate(nodes)
    val questions = openQuestions(nodes)
    s"""<table style="$TableStyle">
       |<thead><tr><th style="$CellStyle$HeadStyle">Item</th><th style="$CellStyle$HeadStyle$HoursStyle">Estimate</th></tr></thead>
       |<tbody>
       |${body.mkString("\n")}
       |</tbody>
       |<tfoot><tr><td style="$CellStyle$TotalStyle">Total</td><td style="$CellStyle$TotalStyle$HoursStyle">${escape(total.text)}</td></tr></tfoot>
       |</table>
       |$questions""".stripMargin.trim

  def errorFragment(message: String): String =
    s"""<p style="font-family:$Font;color:$NoteColor">${escape(message)}</p>"""

  private val Font       = "system-ui,sans-serif"
  private val CondColor  = "#7a5c00"
  private val NoteColor  = "#a33333"
  private val TableStyle = s"font-family:$Font;border-collapse:collapse;min-width:24rem"
  private val CellStyle  = "padding:5px 10px;border-bottom:1px solid #ddd;text-align:left;vertical-align:top;"
  private val HeadStyle  = "font-weight:700;border-bottom:1px solid #999;"
  private val HoursStyle = "text-align:right;white-space:nowrap;"
  private val TotalStyle = "font-weight:700;border-top:2px solid #333;border-bottom:none;"
  private val TopStyle   = "border-top:1px solid #bbb;"

  private def row(level: Int, indent: Int, bold: Boolean, label: String, hours: String): String =
    val weight = if bold then "font-weight:600;" else ""
    val top    = if level == 1 && indent == 0 then TopStyle else ""
    val pad    = s"padding-left:${10 + indent * 22}px;"
    s"""<tr><td style="$CellStyle$top$weight$pad">$label</td><td style="$CellStyle$top$weight$HoursStyle">$hours</td></tr>"""

  private def cond(text: String): String = s"""<span style="color:$CondColor;font-weight:400">${escape(text)}</span>"""
  private def note(text: String): String = s"""<span style="color:$NoteColor;font-style:italic;font-weight:400">${escape(text)}</span>"""

  /** `level` counts named items (for depth collapsing); `indent` is the visual nesting, which forks add to. */
  private def rows(node: Node, level: Int, indent: Int, depth: Option[Int]): List[String] =
    node match
      case Node.Item(name, value) =>
        val est       = Wbs.estimate(node)
        val collapsed = depth.exists(level >= _)
        val hours = value match
          case Node.Value.Leaf(Leaf.Placeholder(text)) => note(text)
          case _                                       => escape(est.text)
        val isGroup = value match
          case Node.Value.Group(_) => true
          case _                   => false
        val self = row(level, indent, isGroup, escape(name), hours)
        value match
          case Node.Value.Group(children) if !collapsed => self :: children.flatMap(rows(_, level + 1, indent + 1, depth))
          case _                                        => List(self)
      case Node.Fork(proposition, t, e) =>
        if depth.exists(level > _) then Nil
        else
          val head = row(level, indent, false, cond(s"if $proposition"), escape(Wbs.estimate(node).text))
          def branch(name: String, ns: List[Node]): List[String] =
            if ns.isEmpty then Nil
            else row(level, indent + 1, false, cond(name), escape(Wbs.estimate(ns).text)) ::
              ns.flatMap(rows(_, level, indent + 2, depth))
          head :: branch("then", t) ++ branch("else", e)

  private def openQuestions(nodes: List[Node]): String =
    val props        = Wbs.propositions(nodes).map((_, text) => s"<li>${escape(text)}</li>")
    val placeholders = Wbs.placeholders(nodes).map((path, text) => s"<li>${escape(path.mkString(" / "))}: ${note(text)}</li>")
    if props.isEmpty && placeholders.isEmpty then ""
    else
      val h3 = "font-size:0.95rem;margin:10px 0 3px"
      val p  = if props.isEmpty then "" else s"""<h3 style="$h3">To find out or decide</h3><ul>${props.mkString}</ul>"""
      val u  = if placeholders.isEmpty then "" else s"""<h3 style="$h3">Not yet estimated</h3><ul>${placeholders.mkString}</ul>"""
      s"""<section style="font-family:$Font;margin-top:24px"><h2 style="font-size:1.1rem">Open questions</h2>$p$u</section>"""

  def escape(s: String): String =
    s.flatMap {
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case c   => c.toString
    }
