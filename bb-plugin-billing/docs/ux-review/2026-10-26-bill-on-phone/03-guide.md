# Guide: billing an archived client thread from a phone, as the product is today

Literal and exhaustive. Written from `01-capabilities.md` and `02-goals.md`. "Know this:" marks what
the screen does not say but you must know.

## Screen map (phone, 390 px wide)

- **Top bar**: "Billing", a Close-pane button, a right-panel button.
- **Tabs**: "To bill" (default), "Projects", "Submissions", "Client report". The labels wrap to two
  lines each.
- **Setup card**: a seven-line paragraph about the model, then steps 1 Connect Zoho Books, 2 Say
  which Zoho projects each bb project bills to, 3 Summaries. Always shown.
- **"Archived client threads to settle (N)"**: a list; each thread takes three or four lines (title,
  "project; customer: project / task", "Xh unbilled, Y already in N submissions", and a "Review and
  bill…" button that wraps under the numbers).
- **"Recent activity"**: the last five sends or settles across all threads, each three to five lines
  of text and an "Undo this send" button.
- **Submission panel** (appears below Recent activity after "Review and bill…"): header "Bill:
  <title>" and Close; a grey **status strip**; a **"Where you stand"** box; the **Up to** controls
  (date-time input, "Now", "When archived", the subthreads checkbox, "days in <zone>"); "Covers:";
  **"What you did"** heading, "Write summaries" button, help text, and a four-column **stretch
  table** (date and clock times, length, description with a disclosure, "up to here" and "✕"); an
  optional **"Billing stops at …"** second table; **"Shared time to settle"** cards; **"Zoho entries
  this would create"** boxes with a textarea each; **"Already in Zoho on these days, not from bb"**;
  **"Time put into Zoho from these threads by hand"**; the **action row** "Send … to Zoho Books" and
  "Don't bill, mark settled" with red blocker text.
- Below the panel: the **range row** (7 / 28 / 90 days, Reload) and **"Where threads bill"**, a
  five-column table per bb project.

Know this: on a phone the stretch table and the "Where threads bill" table are wider than the
screen. You scroll sideways inside the page to reach the "up to here" and "✕" buttons; the
description column is the one that shrinks.

## G1. See what is waiting and open one

1. Open Billing. Scroll past the setup card (about one screen) to "Archived client threads to
   settle (N)".
2. Read each entry's third line: "<X> unbilled, <Y> already in N submissions".
3. Tap "Review and bill…" on the thread you want. The button turns into "Reviewing below ↓". The
   page scrolls down to the panel on its own.

Know this: the panel opens below the Recent activity card, so the scroll lands you on the panel
header; the list you came from is above. Know this: a second "Review and bill…" tap on another
thread replaces the panel. Know this: "Reviewing below ↓" is the only sign of which thread the panel
is for, apart from the panel's own title.

## G2. Know what a stretch was

Method A (lines): in "What you did", each row's third column is a description. If "Write
summaries" has never run for this stretch, the column shows your first message in the stretch
(or the thread title) with no sign that it is a placeholder, except that the CLI says "first
message; no summary yet". Tap "Write summaries": the status strip reads "Writing summaries in a
hidden thread, about 15 seconds. Nothing is sent." and every control greys out; when it returns,
each row shows two or three sentences and each day's notes box shows a paragraph.

Method B (the conversation): under the description, tap "<a> from you, <b> replies, <c> actions".
A bulleted list opens: time, "You:" or "Agent:", the text; the agent's commands and tools in
italics. The list can be dozens of items; it is inside the table cell, so on a phone it is a narrow
column of long lines. Tap "Open the thread" to open the thread's top.

Know this: "Open the thread" cannot jump to the stretch's place in the thread; you scroll in the
thread by the clock times. Know this: lines are written once per stretch and cached; "Write
summaries" only writes what is missing, and also writes the day notes.

## G3. Pick what goes into the submission

Method A (cut at a moment): tap "up to here" on a row (scroll sideways to reach it). The panel
reloads: rows after that moment move to a second table under "Billing stops at <stamp>. M later
stretches stay unbilled for a later send:", where each row has "include up to here".
Either/or: set the "Up to" date-time input, or tap "Now", or tap "When archived" (archived
threads only).

Method B (leave one out): tap "✕" on a row. The row is struck through; its button reads "undo".

Method C (subthreads): untick "Include subthreads that bill to the same place" to bill only this
thread; the "Covers:" line changes.

Know this: a struck-out stretch stays struck out for every future submission of that thread until
you tap "undo" on it; there is no list of struck-out stretches outside the panel. Know this: a
strike-out or a cut reloads the whole panel ("Updating…"), which can take several seconds for two
threads, and every control greys out meanwhile. Know this: there is no way to pick a set of
stretches directly; the submission is "everything before the moment that is not struck out".

## G4. Settle shared time

1. Scroll to "Shared time to settle (N)". Each card shows "<start>–<end> <len> who gets it?", then
   each thread's title, bb project, "not client work" when so, an "open" link, and a disclosure
   with that thread's turns during the stretch.
2. Tap "All to <title>", or "Split minutes" then type minutes and "Save split", or "Nobody gets
   it". Or, above the cards, "Give shared time to the client thread wherever it is the only one".
3. The panel reloads. Decided stretches move into "<N> settled stretch(es)" with "Undo decision".

Know this: Send is disabled while any stretch here is undecided; the red text next to Send says
"1 shared stretch(es) still need a decision". Know this: the "All to" labels cut titles at 30
characters.

## G5. Avoid double billing

Method A (marks): rows whose time the agent put into Zoho by timer carry an amber chip "already
in Zoho: Zoho timer 02:34–03:27 (53m): project: …, task: …, description: …". For a timer that was
never stopped in the thread, the chip's span is the thread's recorded activity from the start until
the logged time of Zoho's own entry for it is used up, and the chip says "… logged, measured over
recorded activity". Strike those rows out with "✕".

Method B (lists): "Time put into Zoho from these threads by hand (N)" lists the timer runs; "Already
in Zoho on these days, not from bb (N)" lists Zoho's own entries on the plan's days with their task,
notes and status.

Method C (after the fact): `bb billing audit <submission id>` prints the timer calls inside a past
submission's spans and Zoho's pre-existing entries on those days, with a verdict.

Know this: the chip appears only inside the panel; the pending list does not say that a thread has
time in Zoho by hand. Know this: the chip does not strike anything out for you. Know this: Method C
exists only in the CLI.

## G6. Read and fix the notes

1. Scroll to "Zoho entries this would create". Each day box shows the amount and one of "(written
   by the model from the day's lines)", "(the day's lines joined; "Write summaries" writes a note)",
   or "(notes edited)".
2. Read the 2-row textarea; drag its corner or scroll inside it to read a long paragraph.
3. Edit, then tap "Save notes". "Use the generated notes" restores the model's text.

Know this: without "Write summaries" the notes are the lines joined with semicolons, cut at 500
characters; nothing marks this as a placeholder except the small grey text. Know this: edited
notes are kept until the send and then dropped.

## G7. Send and know what went

1. Read the status strip: "Reviewing. Nothing is sent until you press Send: N stretches (K struck
   out), X to bill plus Z shared time awaiting a decision, up to <stamp>."
2. Scroll to the bottom. Tap "Send <X> in <N> entries to Zoho Books".
3. A native confirm dialog states the amount, days, destination and what stays unbilled. Tap OK.
4. The strip reads "Sending to Zoho Books…". Then the panel is replaced by: "Sent. Those stretches
   are billed now.", a banner "Sent to Zoho on <stamp>, <thread> from <stamp> to <stamp>: <day>
   <Xh> (entry <id>)", and the buttons "Review what is still unbilled", "Undo this send", "Close".
   The pending list above updates its numbers.

Know this: the strip and the amounts update only after reloads; after a strike-out, wait for
"Updating…" to end before reading them. Know this: "Send" is disabled while a reload is running.

## G8. Undo a send

Either: in the Sent screen, tap "Undo this send"; or on the Recent activity card (or the Submissions
tab), find the submission by its root title and timestamp and tap "Undo this send". A native
confirm appears. The Zoho entries are deleted, the stretches become unbilled again.

Know this: only the submission you chose changes; later or earlier sends of the same thread stand.
Know this: the Recent activity rows read "sent <thread (40 chars)> from <stamp> to <stamp>; <other
thread> from … to …: <day> <Xh> (entry <19-digit id>)"; to pick the right one you match the
timestamps.

## G9. Do it on a phone

Know this: the stretch table is four columns with no-wrap times and buttons; it overflows a 390 px
screen and the page scrolls sideways. Know this: most text is extra-small (text-xs). Know this: the
Up to controls wrap into five lines; the datetime-local input is the native one. Know this: the tab
labels wrap. Know this: the setup card is a full screen of text before the list. Know this: long
thread titles repeat in every row ("(Consolidate the 31 kept write-ups from t)") when more than one
thread is covered. Know this: Recent activity shows five submissions of every thread, with
19-digit entry ids, between the list and the panel.

## G10. Understand the state right now

The status strip at the top of the panel names the phase. "Where you stand" lists the submissions
covering the threads of the draft. The pending list row says "<X> unbilled, <Y> already in N
submissions". The "Where threads bill" table has "Unbilled" and "Sent or settled" columns.

Know this: three places say related things in different words; none of them says per stretch
"billed in submission <n>", because sent stretches are simply absent from the table. Know this:
"struck out" stretches are counted as "unbilled" nowhere and as "to bill" nowhere; they show only as
struck rows.

## G11. Model runs

Only "Write summaries" (or `plan --summaries`) runs a model, once for the stretches and days that
have nothing cached; the strip says so while it runs. Opening the panel, striking out, cutting and
sending never run a model.
