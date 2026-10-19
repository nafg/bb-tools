# Design from the root causes (proposal, not built)

The roast's items group under three model flaws. Fixing the flaws removes the groups; the phone
layout follows from the model rather than from media queries alone.

## Flaw A: the stretch is a table row, and the panel is a draft, not the thread

Roast 1, 2, 6, 8. The unit the user reasons about is the **stretch** of one thread's recording,
and the thing they want to see is the **thread's whole recording with each stretch's state**.

**Model.** One screen per thread, the *thread ledger*: every recorded stretch of the thread (and
of the subthreads it covers), newest day first, grouped by day. Each stretch has exactly one state,
a Scala 3 enum shown as a chip:

- `Unbilled` (default)
- `InDraft` (ticked for the entry being built)
- `Sent(submission, entryId)` ("sent Oct 26")
- `Settled(submission)` ("settled, not billed")
- `LeftOut` (struck out; stays unbilled, hidden from drafts until put back)
- `InZohoByHand(trace)` (the timer evidence; counts as left out unless the user overrides)
- `Shared(block)` overlay when the stretch overlaps another thread's recording and is undecided

The ledger is the screen; the draft is a selection on it. Nothing leaves the screen when it is
sent, struck out or cut: its chip changes. "Where you stand" becomes a two-line summary at the top
("6h 07m unbilled · 2h 25m sent in 2 entries · 2h 55m in Zoho by hand"), and the per-submission
history is the chips themselves plus a "Sent" filter.

**Layout.** A stretch is a card, full width: first line "Thu Oct 8, 02:02–02:47 · 44m" and the
state chip; then the description (two or three sentences); then an "Evidence" link that opens a
sheet with the structured context (your asks, the agent's conclusions, commits, Zoho timer calls,
"Open thread" at the first message); then the tick to include it. No table, no sideways scroll.
Days are section headers with the day's total and, when the day has ticked stretches, the entry
notes box for that day directly under the header.

## Flaw B: the selection is "a moment plus global exclusions" instead of a set of stretches

Roast 4, 5, 13, and the owner's "I only wanted those activities in that particular entry".

**Model.** A draft is a set of stretch keys per thread. Ticking a stretch adds it; a day header has
"tick all"; "everything unbilled" is a one-tap preset; "up to here" is a gesture that ticks all
unbilled stretches at or before this one (a convenience command, as the owner said, not a state).
Left-out is a stretch state the user sets, independent of drafts. The stored submission already is a
set of spans, so this makes the UI say what the store means.

**Behaviour.** Ticks are local and instant: the client holds the ledger and recomputes the day
totals; shared-time decisions and the notes are the only things that need the server. Sending posts
the ticked keys; the confirm sheet lists the days and amounts; afterwards the ticked cards flip to
`Sent` in place.

## Flaw C: "already in Zoho" is a decoration, and the history is described by ids and clock spans

Roast 3, 7, 12.

**Model.** The timer evidence becomes the `InZohoByHand` state above: those stretches are not in a
draft unless the user taps "include anyway" on the chip, the pending row says "2h 55m in Zoho by
hand", and the audit's verdict shows on the ledger ("Oct 26 send overlaps 2h 55m already in Zoho")
with an Undo on the entry. A submission is shown as its stretches ("Oct 8 morning, 5 stretches,
3h 21m, entry sent Oct 26") never as an id or a clock span.

## What stays

The submission-as-state store, the phase enum (now Loading / Reviewing / Sending / Sent / Failed
on the ledger), the summaries and day-notes pipeline, the Zoho trace detection, the CLI and the
agent tool. The CLI's `plan` would print the ledger in the same words.

## Order of work, if approved

1. Server: a `ledger` RPC returning every stretch of a thread (and covered subthreads) with its
   state, grouped by day, plus the day totals; `submit` takes stretch keys.
2. Core: `StretchState` enum and the draft-total computation (shared-time shares included),
   unit-tested.
3. App: the ledger screen replaces the panel; cards, day headers, evidence sheet, bottom action
   bar with the running total and Send; the pending list rows gain the hand-entered amount; Recent
   activity moves into the ledger (chips and a "Sent" filter) and the Submissions tab.
4. Phone pass: 16 px base text, 44 px tap targets, no table anywhere on To bill, tabs as a
   segmented control that fits.
