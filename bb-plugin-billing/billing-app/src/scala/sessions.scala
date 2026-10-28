package billingapp

import billingcore.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*

import scala.scalajs.js

/** The Sessions tab: one session of attention across every thread, drawn as thread boxes down a shared time axis, each
  * stretch showing its state and what was said or done in it.
  */
object SessionsView:
  final case class Props(call: RpcCall, openThread: String => Callback, event: RealtimeEventJs | Null)
  final case class State(screen: Option[SessionScreenJs], loading: Boolean, error: Option[String])

  private val ColPx   = 170
  private val AxisPx  = 64

  private def billingOf(t: ThreadAccount): Billing =
    if t.billsTo.isDefined then Billing.Bills
    else if t.resolved.source.isDefined then Billing.NotClientWork
    else Billing.Unmarked

  private def stateOf(s: StretchStateJs): StretchState =
    s.kind match
      case "leftOut"      => StretchState.LeftOut
      case "sent"         => StretchState.Sent(s.submissionId.getOrElse(""))
      case "settled"      => StretchState.Settled(s.submissionId.getOrElse(""))
      case "inZohoByHand" =>
        val t = s.trace.toOption
        StretchState.InZohoByHand(
          t.fold(ZohoTrace.TimerLeftRunning(0L, ""))(t =>
            if t.kind == "timerRan" then ZohoTrace.TimerRan(t.start.toLong, t.end.toLong, t.detail)
            else ZohoTrace.TimerLeftRunning(t.start.toLong, t.detail)
          )
        )
      case _              => StretchState.Unbilled

  /** What a stretch shows without a click: the first thing the user said in it, else the summary. */
  private def textOf(l: LedgerStretchJs): Option[String] =
    l.messages.find(_.speaker == "User").map(_.text.trim).filter(_.nonEmpty)
      .orElse(Option(l.summary).map(_.trim).filter(_.nonEmpty))

  private def stateClass(st: StretchState): String =
    st match
      case StretchState.Unbilled        => "border-blue-500 bg-blue-200/70 dark:bg-blue-500/30"
      case StretchState.LeftOut         => "border-border bg-muted text-muted-foreground line-through"
      case StretchState.Sent(_)         => "border-green-500 bg-green-200/70 dark:bg-green-500/30"
      case StretchState.Settled(_)      => "border-border bg-muted text-muted-foreground"
      case StretchState.InZohoByHand(_) => "border-amber-500 bg-amber-200/70 dark:bg-amber-500/30"

  private def stateWord(st: StretchState): String =
    st match
      case StretchState.Unbilled        => "unbilled"
      case StretchState.LeftOut         => "left out"
      case StretchState.Sent(_)         => "sent"
      case StretchState.Settled(_)      => "settled"
      case StretchState.InZohoByHand(_) => "in Zoho by hand"

  private def markView(tz: String, pm: PlacedMark, column: Int): VdomNode =
    val members = pm.mark.members
    val height  = math.max(pm.yBottom - pm.yTop - 2, 14.0)
    val (cls, head, body) = pm.mark match
      case Mark.One(st)        =>
        (
          stateClass(st.state),
          s"${hoursMinutes(st.durationMs.toDouble)} ${clock(st.window.start.toDouble, tz)} · ${stateWord(st.state)}",
          st.text.getOrElse("")
        )
      case Mark.Cluster(f, rs) =>
        val states = members.map(m => stateWord(m.state)).distinct
        (
          stateClass(f.state) + " border-dotted",
          s"${members.length} short · ${hoursMinutes(pm.mark.durationMs.toDouble)} ${clock(f.window.start.toDouble, tz)}" +
            (if states.length > 1 then " · mixed" else s" · ${states.head}"),
          members.flatMap(_.text).headOption.getOrElse("")
        )
    <.div(
      ^.key       := members.head.window.key,
      ^.title     := (head + (if body.nonEmpty then s"\n$body" else "")),
      ^.className := s"absolute overflow-hidden rounded border px-1 text-[10px] leading-tight $cls",
      ^.style     := js.Dynamic.literal(
        "left"   -> (AxisPx + column * ColPx + 2),
        "top"    -> (pm.yTop + 14),
        "width"  -> (ColPx - 8),
        "height" -> height
      ),
      <.div(^.className := "font-medium whitespace-nowrap", head),
      <.div(^.className := "break-words text-muted-foreground", body.take(160)).when(body.nonEmpty && height > 30)
    )

  private def boxView(p: PlacedBox, openThread: String => Callback): Vector[VdomNode] =
    val billing = p.box.info.billing
    val cls     = billing match
      case Billing.Bills         => "border-blue-500/70 bg-blue-500/5"
      case Billing.NotClientWork => "border-border bg-muted/40"
      case Billing.Unmarked      => "border-dashed border-amber-500/70 bg-amber-500/5"
    val open    = p.box.top == BoxEdge.ClippedAtSession
    val frame   = <.div(
      ^.key       := s"box-${p.box.info.id}",
      ^.className := s"absolute rounded border $cls",
      ^.style     := js.Dynamic.literal(
        "left"   -> (AxisPx + p.column * ColPx),
        "top"    -> p.yTop,
        "width"  -> (p.span * ColPx - 4),
        "height" -> math.max(p.yBottom - p.yTop, 16.0)
      )
    )
    val title   = <.button(
      ^.`type`    := "button",
      ^.key       := s"title-${p.box.info.id}",
      ^.title     := (p.box.info.title + (billing match
        case Billing.Bills         => ""
        case Billing.NotClientWork => " (not client work)"
        case Billing.Unmarked      => " (never marked as client work or not)")),
      ^.className := "absolute truncate text-left text-[10px] font-semibold hover:underline",
      ^.style     := js.Dynamic.literal(
        "left"  -> (AxisPx + p.column * ColPx + 4),
        "top"   -> (p.yTop + 1),
        "width" -> (ColPx - 10)
      ),
      ^.onClick --> openThread(p.box.info.id),
      p.box.info.title
    )
    Vector(frame, title) ++ p.children.flatMap(boxView(_, openThread))

  private def allMarks(p: PlacedBox): Vector[(PlacedMark, Int)] =
    p.own.map(_ -> p.column) ++ p.children.flatMap(allMarks)

  private def view(tz: String, screen: SessionScreenJs, openThread: String => Callback): VdomNode =
    val accts    = accounts(screen.threads)
    val infos    = accts.map(t =>
      ThreadInfo(t.threadId, t.title, t.parentThreadId, billingOf(t), t.createdAt, t.archivedAt)
    )
    val ref      = screen.sessions(screen.current)
    val session  = Session(ref.start.toLong, ref.end.toLong, Set.empty)
    val stretches = screen.stretches.toVector.map(l =>
      SessionStretch(
        Window(l.threadId, l.day, l.start.toLong, l.end.toLong),
        stateOf(l.state),
        textOf(l)
      )
    )
    val forest = SessionView.forest(session, infos, stretches)
    val layout = SessionGeometry.place(session, forest)
    <.div(
      ^.className := "relative",
      ^.style     := js.Dynamic.literal(
        "height" -> (layout.height + 40),
        "width"  -> (layout.columns * ColPx + AxisPx + 20)
      ),
      layout.roots.flatMap(boxView(_, openThread)).toVdomArray,
      layout.roots.flatMap(allMarks).map((pm, c) => markView(tz, pm, c)).toVdomArray,
      layout.quiet.zipWithIndex.map { (q, i) =>
        <.div(
          ^.key       := s"quiet-$i",
          ^.className := "absolute left-0 bg-muted/60 text-center text-[10px] text-muted-foreground",
          ^.style     := js.Dynamic.literal(
            "top"    -> q.yTop,
            "height" -> (q.yBottom - q.yTop),
            "width"  -> (layout.columns * ColPx + AxisPx)
          ),
          s"${hoursMinutes(q.durationMs.toDouble)} with no attention on any thread"
        )
      }.toVdomArray,
      layout.axis.zipWithIndex.map { (a, i) =>
        <.div(
          ^.key       := s"ax-$i",
          ^.className := "absolute left-0 text-right text-[10px] text-muted-foreground",
          ^.style     := js.Dynamic.literal("top" -> a._1, "width" -> (AxisPx - 6)),
          clock(a._2.toDouble, tz)
        )
      }.toVdomArray
    )

  final class Backend($ : BackendScope[Props, State]):
    def load(at: Option[Long]): Callback =
      $.props.flatMap { p =>
        $.modState(_.copy(loading = true, error = None)) >>
          Callback.future(
            rpc(p.call, BillingRpc.SessionScreen(at))
              .map(r => $.modState(_.copy(screen = Some(r), loading = false)))
              .recover { case e => $.modState(_.copy(loading = false, error = Some(errorMessage(e)))) }
          )
      }

    def render(p: Props, s: State): VdomElement =
      <.div(
        ^.className := "space-y-3",
        s.error.map(e => banner("error", e)),
        s.screen.map { screen =>
          val tz = screen.timeZone
          if screen.current < 0 then <.div(^.className := "text-sm text-muted-foreground", "No attention recorded yet.")
          else
            val ref  = screen.sessions(screen.current)
            def go(i: Int) = load(Some(screen.sessions(i).start.toLong))
            <.div(
              ^.className := "space-y-3",
              <.div(
                ^.className := "flex flex-wrap items-center gap-2",
                <.button(
                  ^.`type`    := "button",
                  ^.className := buttonClass,
                  ^.disabled  := (screen.current == 0 || s.loading),
                  ^.onClick --> go(screen.current - 1),
                  "← Earlier session"
                ),
                <.span(
                  ^.className := "text-sm font-medium",
                  s"${stamp(ref.start, tz)} to ${clock(ref.end, tz)} · ${hoursMinutes(ref.end - ref.start)} · ${ref.threads} threads"
                ),
                <.button(
                  ^.`type`    := "button",
                  ^.className := buttonClass,
                  ^.disabled  := (screen.current == screen.sessions.length - 1 || s.loading),
                  ^.onClick --> go(screen.current + 1),
                  "Later session →"
                ),
                <.span(
                  ^.className := "text-xs text-muted-foreground",
                  s"session ${screen.current + 1} of ${screen.sessions.length}; a gap of 2 hours with no attention starts a new one"
                )
              ),
              <.div(
                ^.className := "flex flex-wrap gap-3 text-xs text-muted-foreground",
                <.span("blue: unbilled"),
                <.span("green: sent"),
                <.span("grey: left out or settled"),
                <.span("amber: in Zoho by hand"),
                <.span("dashed box: thread never marked as client work or not")
              ),
              <.div(^.className := "overflow-auto rounded-md border border-border p-2", view(tz, screen, p.openThread))
            )
        }.getOrElse(<.div(^.className := "text-sm text-muted-foreground", "Loading…").when(s.loading))
      )

  val Component = ScalaComponent
    .builder[Props]("SessionsView")
    .initialState(State(None, loading = false, error = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.load(None))
    .build
