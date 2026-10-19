# Capabilities: what the code gives a user for billing a thread on a phone

Factual inventory from the code as of commit dafe18f, no opinions. File references are
`billing-app/src/scala/components.scala` (C), `billing-app/src/scala/submission.scala` (S),
`billing/src/scala/Service.scala` (Svc), `billing/src/scala/Cli.scala` (CLI). Screenshots at 390 px
wide: `shot-01-to-bill-phone.png`, `shot-02-panel-phone.png`; rendered structure in
`00-rendered-structure-390px.txt`.

## Page frame (C:1180-1200)

- Four tab buttons on one row: "To bill", "Projects", "Submissions", "Client report". At 390 px the
  labels wrap to two lines ("To / bill", "Client / report").
- Content is `max-w-5xl` with `p-4`; no mobile-specific layout anywhere. Body text is `text-sm`; most
  explanations, list rows, table cells and controls are `text-xs` (49 uses in C, 22 in S).
- Buttons: `buttonClass` is a bordered `text-xs` button with `px-2.5 py-1` (about 26 px tall);
  `primaryButtonClass` is `text-sm px-3 py-1.5`.

## To bill tab, top to bottom

### Setup card (C:660-712)
- One paragraph of about 70 words explaining the model (C:667), always shown.
- Three numbered steps, each a title and a status sentence: "Connect Zoho Books" (status
  "Connected. Days run midnight to midnight in <zone>." or "Not connected: … Open plugin settings"),
  "Say which Zoho projects each bb project bills to" ("7 of 26 bb projects with recent time have
  Zoho projects. Set them up on the Projects tab." with a link button), "Summaries" (a sentence about
  "Write summaries" and the summaryModel setting). The card is always shown, including when every
  step is done.

### "Archived client threads to settle (N)" (C:596-650)
- Explanation sentence (C:606).
- One row per thread whose standing is "archived, to settle", sorted by unbilled time descending.
  Each row: the thread title as a button (opens the thread page, C:629), "<bb project>; <customer>:
  <project> / <task>" (C:633), "<Xh Ym> unbilled, <Zh> already in N submissions" or "… unbilled,
  nothing sent before" (C:613-616), and a primary button "Review and bill…" which becomes
  "Reviewing below ↓" (plain style) while that thread's panel is open (C:636-641). Rows are
  `flex-wrap justify-between`, so on a phone the text and the button wrap into two lines (shot-01).
- Clicking "Review and bill…" sets the open thread and mounts the submission panel below the Recent
  activity card (C:731-740); the panel scrolls itself into view on mount (S:716-720).

### "Recent activity" card (C:731; SubmissionsView C:796-875)
- The five newest submissions across all threads, each: root title (button, opens the thread),
  "<stamp>: sent <thread> from <stamp> to <stamp>; …: <day> <Xh Ym> (entry <id>), …" or "settled
  without billing …" (C:850-861), and a button "Undo this send" / "Undo this settle" (C:862-868).
  Undo asks `window.confirm("Undo this submission? Its Zoho entries are deleted and the threads become
  unbilled again from where they were.")` (C:819-821), calls `undo`, reloads the list. The panel does
  not reload on its own after this; the overview does (realtime "changed").
- Nothing marks which submissions belong to the thread whose panel is open.

### Submission panel (S:52-721), mounted below Recent activity
- Header: "Bill: <thread title>" (`text-base font-semibold`) and a "Close" button, `flex-wrap`; on a
  phone the title wraps to two lines and Close drops to its own line (shot-02).
- Status strip (S:262-289), always present, one line of `text-xs font-medium`: one of "Loading what
  is unbilled…", "Updating…", "Writing summaries in a hidden thread, about 15 seconds. Nothing is
  sent.", "Sending to Zoho Books… nothing can be changed until it answers.", "Marking settled…",
  "Failed. The message below says what did and did not happen.", "Sent. Those stretches are billed
  now.", "Settled. …", or "Reviewing. Nothing is sent until you press Send: <N> stretches (<K> struck
  out), <Xh Ym> to bill plus <Zm> shared time awaiting a decision, up to <stamp>."
- Phases are an enum (S:16-48): Loading, Reviewing, Sending, Sent, Failed; controls are disabled
  unless Reviewing (`frozen`, S:386). A reply to a superseded request is dropped (S:66-72).
- "Where you stand" box (S:388-413): either "Nothing of these threads was sent or settled before." or
  "Sent or settled before, newest first; each can be undone on the Recent activity card:" followed by
  a bullet per submission that covers any covered thread: "Sent to Zoho on <stamp>, <thread title
  (40 chars)> from <stamp> to <stamp>; <second thread …>: <day> <Xh Ym> (entry <19-digit id>)"
  (S:80-86, C:34-43). Then "This draft covers <N> stretches (<K> struck out), <Xh Ym> to bill plus …,
  up to <stamp> and leaves <M> later stretches (<Zh>) unbilled, recorded up to <stamp>." (S:403-409).
- Controls row (S:414-460), `flex-wrap items-end gap-3 text-xs`: "Bills to <customer>: <project> /
  <task>" text; label "Up to" over a native `datetime-local` input (S:424); button "Now"; button
  "When archived" (only for archived threads); checkbox "Include subthreads that bill to the same
  place" (default on); text "days in <zone>". On a phone these wrap into five lines (shot-02).
  Changing Up to, Now, When archived or the checkbox reloads the plan (S:112-115).
- "Covers: <titles>; …" and "Left out: …" line (S:461-468), when more than one thread.
- Banners: "No model is available …" when no model; "Summaries not written: <error>" after a failed
  run (S:469-473).
- "What you did" (S:475-516): heading "What you did (<N> stretches (<K> struck out), <Xh Ym> to bill
  plus …)" and a button "Write summaries" on one `flex-wrap` row (S:478-489). Help text (S:493).
  Then a `<table class="w-full text-xs">` with one row per stretch (S:321-382): cell 1
  `whitespace-nowrap` "Thu 10-08 21:14–21:30"; cell 2 `whitespace-nowrap text-right` "15m"; cell 3
  the line (up to 400 characters), the thread title in parentheses when more than one thread,
  amber chips "already in Zoho: Zoho timer 02:34–03:27 (53m): project: …, task: …, description:
  …" (S:333-342), and a `<details>` whose summary reads "<a> from you, <b> replies, <c> actions"
  (S:347-351) opening to a bulleted list of timed turns ("02:40 You: …", "02:41 Agent: …", actions
  in italics) and a button "Open the thread" (S:353-358); cell 4 `whitespace-nowrap text-right` with
  two buttons "up to here" and "✕" (S:362-381). A struck row is `line-through text-muted-foreground`
  and its "✕" reads "undo". Four columns with three `whitespace-nowrap` cells: at 390 px the table
  is wider than the screen; the page scrolls horizontally (shot-02 bottom, the stretch rows cut off).
- "Billing stops at <stamp>. <M> later stretches (<Zh>) stay unbilled for a later send:" and a second
  table of the same shape whose first button reads "include up to here" (S:503-515).
- "Shared time to settle (N)" (S:518-563): help sentence; button "Give shared time to the client
  thread wherever it is the only one" (when any stretch has exactly one billable thread); one card
  per undecided stretch (BlockCard C:109-230): "<start>–<end> <len> who gets it?"; per thread: title,
  "(<bb project>)", "not client work" when so, an "open" link button, and a `<details>` "<a> from
  you, <b> replies, <c> actions during the stretch" with the same turn list; then buttons "All to
  <title (30 chars)>" per thread, "Split minutes", "Nobody gets it"; "Split minutes" opens per-thread
  `w-24` minute inputs and "Save split". A `<details>` "<N> settled stretch(es)" lists decided ones
  with "Undo decision".
- "Zoho entries this would create" (S:565-612): help sentence; per day a bordered box: "<Day label>",
  "<Xh Ym> recorded, <HH:mm> to Zoho", "(notes edited)" or "(written by the model from the day's
  lines)" or "(the day's lines joined; "Write summaries" writes a note)"; a 2-row textarea with the
  notes; buttons "Save notes" (enabled when changed) and "Use the generated notes" (when edited).
  "No time to send." when empty.
- "Already in Zoho on these days, not from bb (N)" (S:617-634): Zoho entries on the plan's days that
  the plugin did not create: "<date>: <Xh Ym> <task> by <user>: <notes> (<status>)".
- "Time put into Zoho from these threads by hand (N)" (S:636-651): per trace "<thread>: Zoho timer
  02:34–03:27 (53m): …" or "Zoho timer started 01:03, not stopped in this thread: …".
- Action row (S:655-676): primary button "Send <Xh Ym> in <N> entries to Zoho Books" (disabled when
  frozen or blockers exist), button "Don't bill, mark settled", and blocker texts in red ("1 shared
  stretch(es) still need a decision", "no time to send: …"). Send asks `window.confirm` with the
  amount, days, destination, "Those stretches (up to <stamp>) count as billed from then on." and what
  stays unbilled (S:208-226). Settle asks similarly (S:225).
- After a send: phase Sent (S:291-316): banner "Sent to Zoho on <stamp>, <spans>: <day> <Xh Ym>
  (entry <id>)", text "This draft is finished. Whatever these threads recorded outside it is still
  unbilled; review that as a new draft, undo this if it was not meant, or close.", buttons "Review
  what is still unbilled" (reloads up to now), "Undo this send", "Close".
- After a failure: red banner with the message and a "Reload" button; the draft stays visible.

### Range row and thread tree (C:742-770, ThreadTree C:380-528)
- "Showing threads with recorded time in the last" 7 / 28 / 90 days buttons and "Reload".
- "Where threads bill": per bb project a heading with its Zoho project and a "change" link, then a
  five-column table: Thread (title button, standing chip e.g. "archived, to settle"), Bill to (a
  `<select>` listing "Same as parent thread" / "Project default: …" / "Not client work" / every
  Zoho project and task, with a status sentence under it), Unbilled, "Sent or settled" ("nothing" or
  "<Xh> in N submissions"), and "Review and bill…". The Bill to column is `w-2/5`. Threads that are
  not client work are hidden behind "Show threads that are not client work".

## Submissions tab (C:796-875)
- The same list as Recent activity without the five-row limit; title "Submissions".

## CLI (CLI:40-130) and agent tool
- `bb billing pending`, `threads`, `plan <thread> [--until] [--no-subthreads] [--summaries]`,
  `exclude`/`include <key>`, `settle`, `settle-all`, `unsettle`, `notes`, `send` (asks the user to
  confirm in the thread composer), `skip`, `undo <id>`, `submissions`, `audit <id>`, `assign`,
  `unbillable`, `clear`, `mapping`, `models`, `projects`, `project-zoho`, `report`.
- Agent tool `billing_send_thread {threadId, until?, subthreads?}` shows a confirmation form in the
  composer (SendThreadForm C:1370-1440) with the plan and "Send to Zoho Books" / "Don't send".

## What does not exist
- No layout that changes with screen width; no stacking of table columns; no bottom action bar.
- No way to open a thread at a given message; "Open the thread" opens the thread's top.
- No per-stretch "bill this one" or "bill these" selection; selection is "everything up to a moment
  minus struck-out stretches".
- No summary of a thread's whole history in the pending row beyond "<X> unbilled, <Y> already in N
  submissions".
- No grouping of stretches by day in "What you did"; the day appears in each row's first cell.
- No way to see, from the pending list, whether a thread has time already in Zoho by hand; that
  appears only inside the panel.
- No collapse of the setup card once set up.
