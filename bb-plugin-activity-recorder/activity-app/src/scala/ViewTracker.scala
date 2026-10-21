package activityapp

import activitycore.{ViewReport, ViewSpanPolicy}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

/** Records `view` spans: the stretches in which a thread's pane is focused,
  * the page is visible and focused, and the user has given input recently
  * (the rule is [[ViewSpanPolicy]]). One span per pane session is reported
  * when it opens, every so often while it stays open, and when it closes,
  * each report carrying the span's end so far. A span's reports are sent one
  * after another, so a close cannot be overtaken by the heartbeat before it.
  */
final class ViewTracker(report: js.Function2[js.Dynamic, Boolean, js.Promise[js.Dynamic]]):
  import ViewTracker.*

  private final class OpenSpan(val id: String, val start: Long):
    var reportedAt: Long       = start
    var sent: Future[Unit]     = Future.unit

  private final class Pane(val threadId: String):
    var focused: Boolean       = false
    var span: Option[OpenSpan] = None
    val listeners              = mutable.LinkedHashSet.empty[() => Unit]

    def setSpan(next: Option[OpenSpan]): Unit =
      val wasOpen = span.isDefined
      span = next
      if wasOpen != next.isDefined then listeners.foreach(listener => listener())

  private val panes       = mutable.LinkedHashSet.empty[Pane]
  private var lastInputAt = now()
  private val document    = js.Dynamic.global.document
  private val window      = js.Dynamic.global.window

  private val onInput: js.Function1[js.Dynamic, Unit]    = _ => lastInputAt = now()
  private val onChange: js.Function1[js.Dynamic, Unit]   = _ => evaluate(unloading = false)
  private val onPageHide: js.Function1[js.Dynamic, Unit] = _ => closeAll(unloading = true)

  locally {
    InputEvents.foreach(name => discard(window.addEventListener(name, onInput, js.Dynamic.literal("passive" -> true))))
    discard(document.addEventListener("visibilitychange", onChange))
    discard(window.addEventListener("focus", onChange))
    discard(window.addEventListener("blur", onChange))
    discard(window.addEventListener("pagehide", onPageHide))
  }
  private val tick = js.timers.setInterval(TickMs.toDouble)(evaluate(unloading = false))

  /** Starts following one pane that shows `threadId`. */
  def attach(threadId: String): PaneHandle =
    val pane = new Pane(threadId)
    panes += pane
    new PaneHandle:
      def setFocused(focused: Boolean): Unit =
        pane.focused = focused
        evaluate(unloading = false)
      def recording: Boolean = pane.span.isDefined
      def subscribe(listener: () => Unit): () => Unit =
        pane.listeners += listener
        () => pane.listeners -= listener
      def dispose(): Unit =
        close(pane, now(), unloading = false)
        panes -= pane

  def dispose(): Unit =
    js.timers.clearInterval(tick)
    InputEvents.foreach(name => discard(window.removeEventListener(name, onInput)))
    discard(document.removeEventListener("visibilitychange", onChange))
    discard(window.removeEventListener("focus", onChange))
    discard(window.removeEventListener("blur", onChange))
    discard(window.removeEventListener("pagehide", onPageHide))
    closeAll(unloading = false)
    panes.clear()

  private def now(): Long = js.Date.now().toLong

  private def discard(value: js.Dynamic): Unit = ()

  private def pageViewed: Boolean =
    document.visibilityState.asInstanceOf[String] == "visible" && document.hasFocus().asInstanceOf[Boolean]

  private def evaluate(unloading: Boolean): Unit =
    val at = now()
    panes.foreach { pane =>
      Policy.decide(pageViewed && pane.focused, at, lastInputAt, pane.span.map(s => (s.start, s.reportedAt))) match
        case Some(ViewReport.Open) =>
          val span = new OpenSpan(js.Dynamic.global.crypto.randomUUID().asInstanceOf[String], at)
          pane.setSpan(Some(span))
          send(pane, span, at, unloading)
        case Some(ViewReport.Extend(end)) => pane.span.foreach(span => send(pane, span, end, unloading))
        case Some(ViewReport.Close(end))  => close(pane, end, unloading)
        case None                         => ()
    }

  private def closeAll(unloading: Boolean): Unit =
    panes.foreach(pane => close(pane, now(), unloading))

  private def close(pane: Pane, end: Long, unloading: Boolean): Unit =
    pane.span.foreach { span =>
      pane.setSpan(None)
      send(pane, span, end, unloading)
    }

  private def send(pane: Pane, span: OpenSpan, end: Long, unloading: Boolean): Unit =
    span.reportedAt = end
    val input = js.Dynamic.literal("id" -> span.id, "threadId" -> pane.threadId, "start" -> span.start.toDouble, "end" -> end.toDouble)
    span.sent = span.sent
      .flatMap(_ => report(input, unloading).toFuture)
      .map(_ => ())
      .recover { case e => js.Dynamic.global.console.debug(s"activity-recorder: view report failed: ${e.getMessage}"): Unit }

object ViewTracker:
  val TickMs = 5_000
  val Policy = ViewSpanPolicy(heartbeatMs = 30_000, idleMs = 300_000)
  private val InputEvents = List("pointerdown", "pointermove", "keydown", "wheel", "touchstart")

trait PaneHandle:
  def setFocused(focused: Boolean): Unit
  /** A view span is open for this pane right now. */
  def recording: Boolean
  /** Calls `listener` whenever `recording` changes; returns the unsubscribe. */
  def subscribe(listener: () => Unit): () => Unit
  def dispose(): Unit

@JSExportTopLevel("createViewTracker")
def createViewTracker(deps: js.Dynamic): js.Dynamic =
  val tracker = new ViewTracker(deps.report.asInstanceOf[js.Function2[js.Dynamic, Boolean, js.Promise[js.Dynamic]]])
  js.Dynamic.literal(
    "attach" -> js.Any.fromFunction1 { (threadId: String) =>
      val handle = tracker.attach(threadId)
      js.Dynamic.literal(
        "setFocused" -> js.Any.fromFunction1((focused: Boolean) => handle.setFocused(focused)),
        "recording"  -> js.Any.fromFunction0(() => handle.recording),
        "subscribe" -> js.Any.fromFunction1 { (listener: js.Function0[Unit]) =>
          val unsubscribe = handle.subscribe(() => listener())
          js.Any.fromFunction0(() => unsubscribe())
        },
        "dispose" -> js.Any.fromFunction0(() => handle.dispose())
      )
    },
    "dispose" -> js.Any.fromFunction0(() => tracker.dispose())
  )
