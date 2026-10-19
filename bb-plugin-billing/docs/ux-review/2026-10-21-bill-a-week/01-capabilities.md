# Capabilities: what the Billing plugin gives a user today (from the code, no opinions)

Use case under review: "Bill last week's work on a client's project to Zoho Books."

Sources: `bb-plugin-billing/billing-app/src/scala/components.scala` (frontend), `bb-plugin-billing/billing/src/scala/Plugin.scala` (RPC, CLI, settings), `app.tsx` (where surfaces mount).

## Entry points

- **Navigation rail icon "Billing"** (app.tsx navPanel, path `billing`). Opens the Billing page at `/plugins/billing/billing`. Lands on the "Week review" tab for the current week.
- **Thread right panel → "+" new-tab menu → "Billing"** (app.tsx threadPanelAction). Opens the per-thread mapping panel for that thread only. No link to it exists from the Billing page.
- **Settings → Installed plugins → Billing**: six settings (Zoho OAuth client id, client secret, organization id, refresh token, Billing increment, Week starts on). Not linked from the Billing page.
- **CLI `bb billing assign|unbillable|clear|mapping|projects|report`** for agents and terminals.

## Billing page: "Week review" tab (components.scala WeekReview, DayCard, BlockCard, Timeline)

Header row: "← Previous week", "Week of Sun yyyy-MM-dd", "Next week →", "This week", "Reload", "days in <zone>".

Banners (shown only when their condition holds): recorder unavailable (red); recorder speech feed stale (amber); lost speech spans (amber); "Zoho is not configured, so days follow the server's zone…" (amber, shown whenever Zoho settings are empty); mapping warnings (amber).

Then seven day cards, one per day of the week, always all seven, each with:
- Heading "Sun 2026-10-18"; right side "billable 0m", and when relevant "unresolved Xm in N overlap(s)" (red) and "N stale decision(s)" (amber).
- If the day has no intervals: "No recorded attention." Otherwise:
- **Timeline** (SVG): hour ticks every 3h, one lane per thread with bars (blue = billable, gray = not), overlaps shaded red (undecided) or green (decided). Bar tooltips show times. No clicks do anything.
- **Table** columns: Thread (button: opens the thread), Billed to ("not billable" or "Customer: Project / Task"), Alone, From overlaps, Total, Pushed. Nothing in the row changes the mapping.
- **Overlaps (N)**: a card per overlap block: time range, duration, badges ("needs a decision" / "no billable thread involved" / decision summary), the threads involved; buttons "All to <thread>" per thread, "Split minutes", "Bill none of it", "Undo decision" — shown only when a billable thread is involved or a decision exists. A day-level button "Give all of this day's overlaps to the billable thread where there is exactly one" appears when undecided > 0.
- **Stale decisions (N)** list with "Discard" per row and "Discard all stale".
- Button **"Preview Zoho entries"** on every day, including empty ones. Clicking shows: "Entries for <day> (billing increment N min)", blockers in red (e.g. "nothing billable on this day, and nothing pushed before to take back"), a table of entries, "Will delete from Zoho" list, and **"Push to Zoho Books"** (disabled while blockers exist) with a confirm dialog.

Precondition for any billable time: at least one thread involved must resolve to a Zoho project/task via thread metadata. With no mappings, every row says "not billable", every overlap says "no billable thread involved", every preview says "nothing billable on this day".

## Billing page: "Client report" tab (ReportView)

Customer select (filled from the Zoho catalog; empty when Zoho is not configured, with the error "Zoho projects unavailable: Zoho is not configured…"), From/To date inputs, "Generate". Shows total, a table per thread, and a Markdown textarea. Reports only what was pushed by this plugin.

## Thread panel "Billing" (AssignPanel)

Thread title; a sentence stating where its time is billed ("Not billable: no mapping on this thread or any ancestor."); a sentence about its own mapping ("This thread has no mapping of its own; it inherits from its ancestors."); "Bill this thread to": project select ("Customer: Project"), task select, "Assign", "Refresh projects from Zoho"; then "Mark not billable", "Clear own mapping (inherit)". When Zoho is not configured: "Zoho projects are unavailable; configure the Billing plugin's Zoho settings to assign a project." and the selects are absent.

## What does not exist

- No setup or status summary on the Billing page (whether Zoho is connected, whether any thread is mapped).
- No way to set a thread's mapping from the Billing page; no link from a row to the thread panel.
- No link from the Billing page to the plugin settings.
- No explanation on the page of what "Alone", "From overlaps", "overlap", "decision", "stale", or "Preview Zoho entries" mean.
- No filter to show only billable threads or only days with billable time; empty days still render a "Preview Zoho entries" button.
- No week-level push; push is per day.
- No sign on a day card that it is ready to push (all overlaps decided, has billable time).
