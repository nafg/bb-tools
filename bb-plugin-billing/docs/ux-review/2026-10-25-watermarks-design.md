> **Superseded.** The watermark (`billedUntil`) model below was replaced: the state is the set of submissions and
> their spans, and "billed until" exists nowhere (see the README's section on submissions). This file is kept as the
> record of the earlier design and of the requirements it answered; do not build from it.

# Design: billing per thread up to a watermark

The day was the wrong unit. Zoho entries are dated, but the work is a thread.

## Model

- Every thread has a **watermark**, `billedUntil`: everything recorded before it is settled (sent to Zoho or deliberately not billed); everything after it is unbilled. A thread with no watermark is unbilled from its first recorded attention.
- A **submission** covers a thread and, by default, the subthreads under it that bill to the same Zoho project and task, each from its own watermark up to one instant `until` (now, or the moment of archiving). It yields one Zoho time entry per calendar date the covered windows fall on, under the shared project and task. Sending moves every covered thread's watermark to `until`. "Don't bill" records the same submission without entries and moves the watermarks the same way.
- A window belongs to exactly one thread and each thread has one watermark, so time billed under a subthread is never billed again under its parent. Time two threads shared at once is still a **shared stretch** with the same decision keys as before; a decision is global and stable whatever any thread's watermark is, because stretches are cut on the unclipped recording and only summed within each submission's range.
- Within a submission every **window** (one recorded interval of one covered thread) can be left out, for time already invoiced by hand or not client work; the entries sum only the included windows.
- **Archived** client threads with unbilled recorded attention are the must-decide list. `thread.archived` refreshes it; nothing is auto-billed.
- Undoing a submission deletes the Zoho entries it created and restores the watermarks it moved.

## Descriptions

Each window gets a one-line summary from Claude Haiku 5.5 of what the user wrote in that window (their own messages only, read from the thread's events), cached per window. Each date's entry notes are the summary of that date's window summaries, editable before sending. Without an Anthropic API key the notes are the thread title plus the window times, and the page says so.

## Hand-entered Zoho time

Before a submission is sent, the plugin lists Zoho's existing time entries on the covered dates for the same project that this plugin did not create, with their hours and notes, as a warning in the preview and in `bb billing plan`. It does not block: the user decides whether to leave windows out or to not bill. Zoho entries carry no clock times, so no automatic subtraction is possible.

## Surfaces

Page tabs: To bill (archived threads needing a decision, open threads with unbilled time, where threads bill), Projects, Client report. The per-thread panel opens a submission: until, subthreads, windows with include boxes and summaries, shared stretches to settle, the dated entries with editable notes, hand-entered Zoho time, Send or Don't bill.

CLI: `threads`, `pending`, `plan <thread> [--until] [--no-subthreads]`, `exclude`/`include <thread> <start> <end>`, `settle`, `settle-all <thread>`, `send <thread>` (user confirms in the thread), `skip <thread>`, `undo <submission>`, `submissions`, plus the mapping and report commands. Tool `billing_send_thread` for agents.
