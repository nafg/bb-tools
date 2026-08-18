package agentmailapp

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import japgolly.scalajs.react.vdom.svg_<^ as SVG
import org.scalajs.dom

import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

private given ExecutionContext = ExecutionContext.parasitic

private val inputClass =
  "w-full rounded-md border border-input bg-background px-2.5 py-1.5 text-sm " +
    "outline-none focus-visible:ring-2 focus-visible:ring-ring"

private def field(label: String)(control: VdomNode): VdomElement =
  <.label(
    ^.className := "block space-y-1",
    <.span(^.className := "text-xs font-medium text-muted-foreground", label),
    control
  )

private def mailIcon(cls: String): VdomElement =
  SVG.<.svg(
    VdomAttr("className")   := cls,
    VdomAttr("aria-hidden") := "true",
    SVG.^.viewBox           := "0 0 24 24",
    SVG.^.fill              := "none",
    SVG.^.stroke            := "currentColor",
    SVG.^.strokeWidth       := 2,
    SVG.^.strokeLinecap     := "round",
    SVG.^.strokeLinejoin    := "round",
    SVG.<.rect(SVG.^.x := 3, SVG.^.y := 5, SVG.^.width := 18, SVG.^.height := 14, SVG.^.rx := 2),
    SVG.<.path(SVG.^.d := "m3 7 9 6 9-6")
  )

private val AutosaveDelayMs = 1200.0

object DraftCard:
  final case class Props(draft: DraftSummary, call: RpcCall, onChanged: Callback)

  final case class State(
      baseline: DraftSummary,
      to: String,
      cc: String,
      subject: String,
      body: String,
      busy: Boolean,
      stale: Boolean,
      error: Option[String]
  ):
    def dirty: Boolean =
      to != baseline.to.mkString(", ") ||
        cc != baseline.cc.mkString(", ") ||
        subject != baseline.subject ||
        body != baseline.body

  private def stateFrom(draft: DraftSummary, busy: Boolean = false, error: Option[String] = None): State =
    State(
      baseline = draft,
      to = draft.to.mkString(", "),
      cc = draft.cc.mkString(", "),
      subject = draft.subject,
      body = draft.body,
      busy = busy,
      stale = false,
      error = error
    )

  final class Backend($ : BackendScope[Props, State]):
    private var timer: Option[js.timers.SetTimeoutHandle] = None

    def cancelTimer(): Unit =
      timer.foreach(js.timers.clearTimeout)
      timer = None

    private def scheduleSave: Callback = Callback {
      cancelTimer()
      timer = Some(js.timers.setTimeout(AutosaveDelayMs)(runSave()))
    }

    private def runSave(): Unit =
      val props = $.props.runNow()
      val state = $.state.runNow()
      if state.dirty && !state.stale then
        val base    = state.baseline
        val changes = js.Dynamic.literal("draftId" -> props.draft.draftId)
        if state.to != base.to.mkString(", ") then changes.updateDynamic("to")(splitAddresses(state.to).toJSArray)
        if state.cc != base.cc.mkString(", ") then changes.updateDynamic("cc")(splitAddresses(state.cc).toJSArray)
        if state.subject != base.subject then changes.updateDynamic("subject")(state.subject)
        if state.body != base.body then changes.updateDynamic("body")(state.body)
        props.call("saveDraft", changes).toFuture.onComplete { result =>
          val update = result.fold(
            e => $.modState(_.copy(error = Some(errorMessage(e)))),
            saved => $.modState(_.copy(baseline = saved.asInstanceOf[DraftSummary], error = None))
          )
          update.runNow()
        }

    def setField(f: (State, String) => State)(e: ReactEventFromInput): Callback =
      val value = e.target.value
      $.modState(s => f(s, value)) >> scheduleSave

    def adopt(draft: DraftSummary): Callback =
      $.modState(_ => stateFrom(draft))

    /** Props changed under us (agent revision, refetch): merge into a clean
      * card, or flag stale rather than clobber mid-edit keystrokes.
      */
    def reconcile(next: DraftSummary): Callback =
      $.state.flatMap { s =>
        if next.updatedAt == s.baseline.updatedAt then Callback.empty
        else if s.dirty then $.modState(_.copy(stale = true))
        else adopt(next)
      }

    private def act(run: => scala.concurrent.Future[Callback]): Callback =
      $.modState(_.copy(busy = true, error = None)) >>
        Callback.future(
          run.recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
            .map(_ >> $.modState(_.copy(busy = false)))
        )

    def send: Callback =
      $.props.flatMap { props =>
        act(
          props
            .call("sendDraft", js.Dynamic.literal("draftId" -> props.draft.draftId))
            .toFuture
            .map(_ => props.onChanged)
        )
      }

    def discard: Callback =
      $.props.flatMap { props =>
        if !dom.window.confirm("Discard this draft?") then Callback.empty
        else
          act(
            props
              .call("discardDraft", js.Dynamic.literal("draftId" -> props.draft.draftId))
              .toFuture
              .map(_ => props.onChanged)
          )
      }

    def render(props: Props, s: State): VdomElement =
      val draft   = props.draft
      val canSend = s.body.trim.nonEmpty && (draft.isReply || splitAddresses(s.to).nonEmpty)
      <.div(
        ^.className := "space-y-3 rounded-lg border border-border bg-card p-4 text-sm",
        <.div(
          ^.className := "flex items-center justify-between",
          <.div(
            ^.className := "font-medium",
            if draft.isReply then "Reply draft awaiting review" else "Draft awaiting review"
          ),
          <.div(^.className := "font-mono text-xs text-muted-foreground", draft.draftId)
        ),
        <.div(
          ^.className := "flex items-center justify-between rounded-md border border-border bg-accent/50 px-3 py-2 text-xs",
          <.span("This draft was updated elsewhere while you were editing."),
          <.button(
            ^.`type`    := "button",
            ^.className := "rounded-md border border-border px-2 py-1 hover:bg-accent",
            ^.onClick --> adopt(draft),
            "Load latest"
          )
        ).when(s.stale),
        if draft.isReply then
          <.div(
            ^.className := "space-y-0.5 text-xs text-muted-foreground",
            <.div(s"To: ${draft.to.mkString(", ")}").when(draft.to.nonEmpty),
            <.div(s"Subject: ${draft.subject}").when(draft.subject.nonEmpty)
          )
        else
          React.Fragment(
            field("To")(
              <.input(^.className := inputClass, ^.value := s.to, ^.onChange ==> setField((st, v) => st.copy(to = v)))
            ),
            field("Cc")(
              <.input(^.className := inputClass, ^.value := s.cc, ^.onChange ==> setField((st, v) => st.copy(cc = v)))
            ),
            field("Subject")(
              <.input(
                ^.className := inputClass,
                ^.value     := s.subject,
                ^.onChange ==> setField((st, v) => st.copy(subject = v))
              )
            )
          ),
        field("Body")(
          <.textarea(
            ^.className := (inputClass + " min-h-40 resize-y"),
            ^.rows      := 10,
            ^.value     := s.body,
            ^.onChange ==> setField((st, v) => st.copy(body = v))
          )
        ),
        <.div(
          ^.className := "space-y-0.5 text-xs text-muted-foreground",
          <.div(^.className := "font-medium", "Attachments"),
          draft.attachments.map(name => <.div(^.key := name, ^.className := "font-mono", name): VdomNode).toVdomArray
        ).when(draft.attachments.nonEmpty),
        <.div(
          ^.className := "text-xs text-muted-foreground",
          "An HTML version was also composed; if you edit the body text, the email is sent as plain text only."
        ).when(draft.hasHtml),
        s.error.map(message => <.div(^.className := "text-xs text-destructive", message)),
        <.div(
          ^.className := "flex items-center justify-between gap-2",
          <.span(^.className := "text-xs text-muted-foreground", if s.dirty then "Saving…" else "Saved"),
          <.div(
            ^.className := "flex items-center gap-2",
            <.button(
              ^.`type`    := "button",
              ^.disabled  := s.busy,
              ^.className := "rounded-md border border-border px-3 py-1.5 text-sm hover:bg-accent disabled:opacity-50",
              ^.onClick --> discard,
              "Discard"
            ),
            <.button(
              ^.`type`   := "button",
              ^.disabled := (s.busy || s.dirty || s.stale || !canSend),
              (^.title := "Waiting for your edits to save").when(s.dirty),
              ^.className := "rounded-md bg-primary px-3 py-1.5 text-sm font-medium text-primary-foreground hover:bg-primary/90 disabled:opacity-50",
              ^.onClick --> send,
              "Send"
            )
          )
        )
      )

  val Component = ScalaComponent
    .builder[Props]("DraftCard")
    .initialStateFromProps(p => stateFrom(p.draft))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidUpdate(x => x.backend.reconcile(x.currentProps.draft))
    .componentWillUnmount(x => Callback(x.backend.cancelTimer()))
    .build

object EmailThreadCard:
  final case class Props(thread: EmailThreadSummary, call: RpcCall)

  final case class State(
      expanded: Boolean,
      messages: Option[Vector[EmailMessage]],
      error: Option[String]
  )

  final class Backend($ : BackendScope[Props, State]):
    private def load: Callback =
      $.props.flatMap { props =>
        Callback.future(
          props
            .call("threadMessages", js.Dynamic.literal("emailThreadId" -> props.thread.emailThreadId))
            .toFuture
            .map { result =>
              val messages = result.asInstanceOf[ThreadMessagesResult].messages.toVector
              $.modState(_.copy(messages = Some(messages), error = None))
            }
            .recover { case e => $.modState(_.copy(error = Some(errorMessage(e)))) }
        )
      }

    def toggle: Callback =
      $.state.flatMap { s =>
        val opening = !s.expanded
        $.modState(_.copy(expanded = opening)) >>
          (if opening && s.messages.isEmpty then load else Callback.empty)
      }

    private def renderMessage(m: EmailMessage): VdomElement =
      val sent = m.direction == "sent"
      <.div(
        ^.key       := m.messageId,
        ^.className := "space-y-1",
        <.div(
          ^.className := "flex items-center gap-2 text-xs text-muted-foreground",
          <.span(
            ^.className := ("rounded px-1.5 py-0.5 font-medium " +
              (if sent then "bg-primary/10 text-primary" else "bg-accent text-accent-foreground")),
            if sent then "Sent" else "Received"
          ),
          <.span(^.className := "truncate", m.from),
          <.span(^.className := "ml-auto shrink-0", m.timestamp)
        ),
        <.div(^.className := "whitespace-pre-wrap", m.body),
        <.div(
          ^.className := "text-xs text-muted-foreground",
          m.attachments
            .map(a => <.div(^.key := a.attachmentId, ^.className := "font-mono", s"📎 ${a.filename}"): VdomNode)
            .toVdomArray
        ).when(m.attachments.nonEmpty)
      )

    def render(props: Props, s: State): VdomElement =
      val thread = props.thread
      <.div(
        ^.className := "rounded-lg border border-border bg-card text-sm",
        <.button(
          ^.`type`    := "button",
          ^.className := "flex w-full items-center justify-between gap-2 px-4 py-3 text-left hover:bg-accent/50",
          ^.onClick --> toggle,
          <.span(
            ^.className := "min-w-0",
            <.span(
              ^.className := "block truncate font-medium",
              if thread.subject.nonEmpty then thread.subject else "(no subject)"
            ),
            <.span(^.className := "block truncate text-xs text-muted-foreground", s"with ${thread.counterparty}")
          ),
          <.span(^.className := "text-xs text-muted-foreground", if s.expanded then "▾" else "▸")
        ),
        <.div(
          ^.className := "space-y-3 border-t border-border px-4 py-3",
          s.error.map(message => <.div(^.className := "text-xs text-destructive", message)),
          <.div(^.className := "text-xs text-muted-foreground", "Loading…")
            .when(s.messages.isEmpty && s.error.isEmpty),
          s.messages.map(_.map(renderMessage: EmailMessage => VdomNode).toVdomArray)
        ).when(s.expanded)
      )

  val Component = ScalaComponent
    .builder[Props]("EmailThreadCard")
    .initialState(State(expanded = false, messages = None, error = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .build

object EmailPanel:
  final case class State(
      loaded: Boolean,
      drafts: Vector[DraftSummary],
      threads: Vector[EmailThreadSummary],
      error: Option[String]
  )

  final class Backend($ : BackendScope[PanelProps, State]):
    def load: Callback =
      $.props.flatMap { props =>
        Callback.future(
          props
            .call("emailState", js.Dynamic.literal("threadId" -> props.threadId))
            .toFuture
            .map { result =>
              val state = result.asInstanceOf[EmailStateResult]
              $.modState(
                _.copy(loaded = true, drafts = state.drafts.toVector, threads = state.threads.toVector, error = None)
              )
            }
            .recover { case e => $.modState(_.copy(loaded = true, error = Some(errorMessage(e)))) }
        )
      }

    def render(props: PanelProps, s: State): VdomElement =
      <.div(
        ^.className := "space-y-4",
        s.error.map(message => <.div(^.className := "text-sm text-destructive", message)),
        <.div(^.className := "text-sm text-muted-foreground", "Loading…")
          .when(!s.loaded && s.error.isEmpty),
        <.div(
          ^.className := "text-sm text-muted-foreground",
          "No email activity in this thread yet. Ask the agent to draft an email; it will appear here for your review."
        ).when(s.loaded && s.drafts.isEmpty && s.threads.isEmpty && s.error.isEmpty),
        s.drafts
          .map(d => DraftCard.Component.withKey(d.draftId)(DraftCard.Props(d, props.call, load)): VdomNode)
          .toVdomArray,
        <.div(
          ^.className := "space-y-2",
          <.div(^.className := "text-xs font-medium text-muted-foreground", "Email threads"),
          s.threads
            .map(t => EmailThreadCard.Component.withKey(t.emailThreadId)(EmailThreadCard.Props(t, props.call)): VdomNode)
            .toVdomArray
        ).when(s.threads.nonEmpty)
      )

  val Component = ScalaComponent
    .builder[PanelProps]("EmailPanel")
    .initialState(State(loaded = false, drafts = Vector.empty, threads = Vector.empty, error = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.load)
    .componentDidUpdate(x =>
      if x.prevProps.signal != x.currentProps.signal then x.backend.load else Callback.empty
    )
    .build

object EmailHeaderButton:
  final case class State(badge: Option[EmailBadgeResult])

  final class Backend($ : BackendScope[HeaderButtonProps, State]):
    def load: Callback =
      $.props.flatMap { props =>
        Callback.future(
          props
            .call("emailBadge", js.Dynamic.literal("threadId" -> props.threadId))
            .toFuture
            .map(result => $.modState(_.copy(badge = Some(result.asInstanceOf[EmailBadgeResult]))))
            .recover { case _ => $.modState(_.copy(badge = None)) }
        )
      }

    def render(props: HeaderButtonProps, s: State): VdomNode =
      s.badge.filter(b => b.pendingDrafts > 0 || b.emailThreads > 0) match
        case None => EmptyVdom
        case Some(badge) =>
          val label =
            if badge.pendingDrafts > 0 then
              s"Email: ${badge.pendingDrafts} draft${if badge.pendingDrafts == 1 then "" else "s"} awaiting review"
            else "Email"
          <.button(
            ^.`type`     := "button",
            ^.aria.label := label,
            ^.title      := label,
            ^.className := "relative flex h-7 w-7 items-center justify-center rounded-md text-muted-foreground hover:bg-accent hover:text-foreground",
            ^.onClick --> Callback(props.openPanel()),
            mailIcon("h-4 w-4"),
            <.span(^.className := "absolute right-0.5 top-0.5 h-2 w-2 rounded-full bg-primary")
              .when(badge.pendingDrafts > 0)
          )

  val Component = ScalaComponent
    .builder[HeaderButtonProps]("EmailHeaderButton")
    .initialState(State(badge = None))
    .backend(new Backend(_))
    .render($ => $.backend.render($.props, $.state))
    .componentDidMount(_.backend.load)
    .componentDidUpdate(x =>
      if x.prevProps.signal != x.currentProps.signal then x.backend.load else Callback.empty
    )
    .build

object AgentmailDirective:
  val Component = ScalaFnComponent[DirectiveProps] { props =>
    val draft = props.draft.getOrElse("")
    val label =
      if draft.nonEmpty then "Email draft awaiting your review" else "Email activity in this thread"
    <.button(
      ^.`type`    := "button",
      ^.className := "my-1 flex items-center gap-2 rounded-lg border border-border bg-card px-3 py-2 text-sm hover:bg-accent",
      ^.onClick --> Callback(props.openPanel()),
      mailIcon("h-4 w-4 text-muted-foreground"),
      <.span(label),
      <.span(^.className := "font-mono text-xs text-muted-foreground", draft).when(draft.nonEmpty),
      <.span(^.className := "text-xs text-muted-foreground", "Open Email panel →")
    )
  }

/** Raw function components consumed by app.tsx: React calls each with the
  * plain-JS props object and receives the rendered element.
  */
object Exports:
  @JSExportTopLevel("EmailPanel")
  val emailPanel: js.Function1[PanelProps, js.Any] =
    props => EmailPanel.Component(props).vdomElement.rawElement

  @JSExportTopLevel("EmailHeaderButton")
  val emailHeaderButton: js.Function1[HeaderButtonProps, js.Any] =
    props => EmailHeaderButton.Component(props).vdomElement.rawElement

  @JSExportTopLevel("AgentmailDirective")
  val agentmailDirective: js.Function1[DirectiveProps, js.Any] =
    props => AgentmailDirective.Component(props).vdomElement.rawElement
