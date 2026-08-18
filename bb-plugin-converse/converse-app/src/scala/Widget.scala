package converseapp

import scala.scalajs.js

/** The floating chrome-level voice widget: a strip fixed at the bottom of the
  * app window, outside any thread surface. Rendered with plain DOM from the
  * content script; styled with the host theme's CSS variables.
  */
object Widget:
  private val MicSvg =
    """<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor"
      | stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
      |<path d="M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z"/>
      |<path d="M19 10v2a7 7 0 0 1-14 0v-2"/><line x1="12" x2="12" y1="19" y2="22"/></svg>""".stripMargin

  private val phaseLabels = Map(
    "starting"     -> "Starting…",
    "listening"    -> "Listening",
    "recording"    -> "Hearing you…",
    "transcribing" -> "Transcribing…",
    "waiting"      -> "Thinking…",
    "speaking"     -> "Speaking",
    "error"        -> "Voice error"
  )

  private val phaseColors = Map(
    "starting"     -> "var(--muted-foreground, #888)",
    "listening"    -> "var(--success, #22c55e)",
    "recording"    -> "var(--destructive, #ef4444)",
    "transcribing" -> "var(--primary, #6366f1)",
    "waiting"      -> "var(--primary, #6366f1)",
    "speaking"     -> "var(--primary, #6366f1)",
    "error"        -> "var(--destructive, #ef4444)"
  )

  /** Mounts the widget into the sidebar, above its footer (anchored on the
    * stable /settings link); re-attaches when bb re-renders the sidebar and
    * hides while it is collapsed. Returns a disposer.
    */
  def mount(ctl: Controller): js.Function0[Unit] =
    val document = js.Dynamic.global.document

    val root = document.createElement("div")
    root.setAttribute("data-converse-widget", "")
    root.style.cssText =
      "font-size:11px;line-height:1.4;margin:4px 8px;" +
        "font-family:var(--font-sans, system-ui, sans-serif);"

    val pill = document.createElement("div")
    pill.style.cssText =
      "display:flex;flex-direction:column;gap:2px;padding:5px 8px;border-radius:10px;" +
        "background:var(--card, var(--background, #1e1e1e));color:var(--card-foreground, inherit);" +
        "border:1px solid var(--border, #4443);"
    val _ = root.appendChild(pill)

    val row = document.createElement("div")
    row.style.cssText = "display:flex;align-items:center;gap:8px;"
    val _ = pill.appendChild(row)

    val button = document.createElement("button")
    button.setAttribute("type", "button")
    button.style.cssText =
      "display:flex;align-items:center;justify-content:center;width:24px;height:24px;" +
        "border-radius:999px;border:none;background:transparent;cursor:pointer;color:inherit;padding:0;"
    button.innerHTML = MicSvg
    val _ = row.appendChild(button)

    val label = document.createElement("span")
    label.style.cssText = "white-space:nowrap;"
    val _ = row.appendChild(label)

    val meter = document.createElement("span")
    meter.style.cssText =
      "display:inline-block;width:32px;height:4px;border-radius:999px;overflow:hidden;" +
        "background:var(--border, #4443);flex-shrink:0;"
    val meterFill = document.createElement("span")
    meterFill.style.cssText = "display:block;height:100%;border-radius:999px;width:0%;"
    val _ = meter.appendChild(meterFill)
    val _ = row.appendChild(meter)

    val elsewhere = document.createElement("span")
    elsewhere.style.cssText = "color:var(--muted-foreground, #888);white-space:nowrap;"
    elsewhere.textContent = "→ another thread"
    val _ = row.appendChild(elsewhere)

    val text = document.createElement("div")
    text.style.cssText = "overflow-wrap:anywhere;max-height:4.2em;overflow-y:auto;"
    val _ = pill.appendChild(text)

    button.onclick = js.Any.fromFunction1 { (_: js.Any) =>
      val snap = ctl.getSnapshot()
      if snap.phase.asInstanceOf[String] != "idle" then ctl.stop() else ctl.startViewed()
    }

    def render(): Unit =
      val snap    = ctl.getSnapshot()
      val phase   = snap.phase.asInstanceOf[String]
      val active  = phase != "idle"
      val viewed  = snap.viewed.asInstanceOf[String | Null]
      val target  = snap.threadId.asInstanceOf[String | Null]
      val error   = snap.error.asInstanceOf[String | Null]
      val interim = snap.interim.asInstanceOf[String | Null]
      val heard   = snap.heard.asInstanceOf[String | Null]

      val color = phaseColors.getOrElse(phase, "var(--muted-foreground, #888)")
      button.style.color = if active then color else "var(--muted-foreground, #888)"
      button.setAttribute(
        "title",
        if active then "Voice conversation is on — click to stop"
        else if viewed == null then "Open a thread to start a voice conversation"
        else "Start a voice conversation (utterances go to the thread you are viewing)"
      )
      button.setAttribute("aria-label", button.getAttribute("title").asInstanceOf[String])
      button.disabled = !active && viewed == null

      label.style.color = color
      label.textContent = if active then phaseLabels.getOrElse(phase, phase) else "Voice"

      meter.style.display = if active then "inline-block" else "none"
      if active then
        val level     = snap.level.asInstanceOf[Double]
        val threshold = snap.threshold.asInstanceOf[Double]
        val pct       = math.min(100.0, level / (threshold * 8) * 100.0)
        meterFill.style.width = s"$pct%"
        meterFill.style.background =
          if level > threshold then "var(--destructive, #ef4444)" else "var(--success, #22c55e)"

      elsewhere.style.display =
        if active && target != null && viewed != null && target != viewed then "inline" else "none"

      val shown = if error != null then error else if interim != null then interim else heard
      text.style.display = if active && shown != null then "block" else "none"
      if shown != null then
        text.textContent = shown
        text.style.color =
          if error != null then "var(--destructive, #ef4444)"
          else if interim != null then "var(--foreground, inherit)"
          else "var(--muted-foreground, #888)"
        text.style.fontStyle = if error == null && interim != null then "italic" else "normal"

    render()
    val unsubscribe = ctl.subscribe(() => render())

    // The sidebar is host-owned React DOM: anchor above its footer list (the
    // /settings link is the stable landmark) and re-attach whenever a
    // re-render drops our node. Hidden while the sidebar is collapsed.
    def attach(): Unit =
      val link = document.querySelector("a[href='/settings']")
      if link != null then
        val list = link.closest("ul,ol,[role='list']")
        if list != null && list.parentElement != null then
          val _ = list.parentElement.insertBefore(root, list)

    // The host-rendered fallback toggle (the sidebarFooterAction in app.tsx,
    // matched by its title) is redundant while this widget is anchored: keep
    // it visible only when the widget has nowhere to dock.
    def syncFallbackToggle(show: Boolean): Unit =
      val selector =
        "button[title='Toggle voice conversation'],button[aria-label='Toggle voice conversation']"
      val buttons = document.querySelectorAll(selector)
      for i <- 0 until buttons.length.asInstanceOf[Int] do
        buttons.item(i).style.display = if show then "" else "none"

    attach()
    syncFallbackToggle(show = !root.isConnected.asInstanceOf[Boolean])
    var reattachQueued = false
    val observer = js.Dynamic.newInstance(js.Dynamic.global.MutationObserver)(
      js.Any.fromFunction2 { (_: js.Any, _: js.Any) =>
        if !root.isConnected.asInstanceOf[Boolean] && !reattachQueued then
          reattachQueued = true
          val _ = js.Dynamic.global.setTimeout(
            js.Any.fromFunction0 { () =>
              reattachQueued = false
              if !root.isConnected.asInstanceOf[Boolean] then attach()
              syncFallbackToggle(show = !root.isConnected.asInstanceOf[Boolean])
            },
            200
          )
        // The host may re-render the footer at any time, recreating the
        // fallback button visible; re-hide it while the widget is anchored.
        else if root.isConnected.asInstanceOf[Boolean] then syncFallbackToggle(show = false)
      }
    )
    val _ = observer.observe(document.body, js.Dynamic.literal("childList" -> true, "subtree" -> true))

    () =>
      val _ = observer.disconnect()
      unsubscribe()
      syncFallbackToggle(show = true)
      val _ = root.remove()
