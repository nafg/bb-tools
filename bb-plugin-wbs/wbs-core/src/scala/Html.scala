package wbscore

/** Renders an estimate as a self-contained HTML document: a tree table with
  * totals, then the open questions. No scripts or external resources, so the
  * same output serves the bb panel and standalone files or email.
  */
object Html:
  final case class Options(title: String, depth: Option[Int])

  def render(nodes: List[Node], options: Options): String =
    val body      = nodes.flatMap(rows(_, 1, 0, options.depth))
    val total     = Wbs.estimate(nodes)
    val questions = openQuestions(nodes)
    s"""<!doctype html>
       |<html lang="en">
       |<head>
       |<meta charset="utf-8">
       |<title>${escape(options.title)}</title>
       |<style>$Css</style>
       |</head>
       |<body>
       |<table class="wbs">
       |<thead><tr><th>Item</th><th class="hours">Estimate</th></tr></thead>
       |<tbody>
       |${body.mkString("\n")}
       |</tbody>
       |<tfoot><tr><td>Total</td><td class="hours">${escape(total.text)}</td></tr></tfoot>
       |</table>
       |$questions
       |</body>
       |</html>
       |""".stripMargin

  def errorPage(title: String, message: String): String =
    s"""<!doctype html>
       |<html lang="en"><head><meta charset="utf-8"><title>${escape(title)}</title><style>$Css</style></head>
       |<body><h1>${escape(title)}</h1><p class="error">${escape(message)}</p></body></html>
       |""".stripMargin

  private def row(level: Int, indent: Int, kind: String, label: String, hours: String): String =
    s"""<tr class="l$level $kind"><td><span class="indent" style="--level:$indent"></span>$label</td><td class="hours">$hours</td></tr>"""

  /** `level` counts named items (for depth collapsing); `indent` is the visual nesting, which forks add to. */
  private def rows(node: Node, level: Int, indent: Int, depth: Option[Int]): List[String] =
    val collapsed = depth.exists(level > _)
    node match
      case Node.Item(name, value) =>
        val est = Wbs.estimate(node)
        val kind = value match
          case Node.Value.Leaf(Leaf.Placeholder(_)) => "placeholder"
          case Node.Value.Leaf(_)                   => "leaf"
          case Node.Value.Group(_)                  => if depth.exists(level >= _) then "collapsed" else "group"
        val hours = value match
          case Node.Value.Leaf(Leaf.Placeholder(note)) => s"""<span class="note">${escape(note)}</span>"""
          case _                                       => escape(est.text)
        val self = row(level, indent, kind, escape(name), hours)
        value match
          case Node.Value.Group(children) if kind == "group" => self :: children.flatMap(rows(_, level + 1, indent + 1, depth))
          case _                                             => List(self)
      case Node.Fork(proposition, t, e) =>
        if collapsed then Nil
        else
          val head = row(level, indent, "fork", s"""<span class="cond">if ${escape(proposition)}</span>""", escape(Wbs.estimate(node).text))
          def branch(name: String, ns: List[Node]): List[String] =
            if ns.isEmpty then Nil
            else row(level, indent + 1, "branch", s"""<span class="cond">$name</span>""", escape(Wbs.estimate(ns).text)) ::
              ns.flatMap(rows(_, level, indent + 2, depth))
          head :: branch("then", t) ++ branch("else", e)

  private def openQuestions(nodes: List[Node]): String =
    val props        = Wbs.propositions(nodes).map((_, text) => s"<li>${escape(text)}</li>")
    val placeholders = Wbs.placeholders(nodes).map { (path, note) =>
      s"<li>${escape(path.mkString(" / "))}: <span class=\"note\">${escape(note)}</span></li>"
    }
    if props.isEmpty && placeholders.isEmpty then ""
    else
      val p = if props.isEmpty then "" else s"<h3>To find out or decide</h3><ul>${props.mkString}</ul>"
      val u = if placeholders.isEmpty then "" else s"<h3>Not yet estimated</h3><ul>${placeholders.mkString}</ul>"
      s"""<section class="questions"><h2>Open questions</h2>$p$u</section>"""

  def escape(s: String): String =
    s.flatMap {
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case c   => c.toString
    }

  private val Css =
    """body{font-family:system-ui,sans-serif;margin:1.5rem;color:#1a1a1a;background:#fff}
      |table.wbs{border-collapse:collapse;min-width:24rem}
      |.wbs th,.wbs td{padding:.3rem .6rem;border-bottom:1px solid #ddd;text-align:left;vertical-align:top}
      |.wbs th.hours,.wbs td.hours{text-align:right;white-space:nowrap;font-variant-numeric:tabular-nums}
      |.wbs .indent{display:inline-block;width:calc(var(--level) * 1.4rem)}
      |.wbs tr.group td,.wbs tr.collapsed td{font-weight:600}
      |.wbs tr.fork td,.wbs tr.branch td{font-weight:400}
      |.wbs tr.l1 td{border-top:1px solid #bbb}
      |.wbs tfoot td{font-weight:700;border-top:2px solid #333;border-bottom:none}
      |.cond{color:#7a5c00;font-weight:400;font-size:.9em}
      |.note{color:#a33;font-style:italic;font-weight:400}
      |.questions{margin-top:1.5rem}.questions h2{font-size:1.1rem}.questions h3{font-size:.95rem;margin:.6rem 0 .2rem}
      |.error{color:#a33}
      |@media (prefers-color-scheme:dark){body{color:#e6e6e6;background:#151515}.wbs th,.wbs td{border-color:#333}.wbs tr.l1 td{border-top-color:#555}.wbs tfoot td{border-top-color:#aaa}.cond{color:#d9b34a}.note{color:#e07b7b}}
      |""".stripMargin.replace("\n", "")
