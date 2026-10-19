# bb-plugin-billing

A [bb](https://github.com/get-bb/bb) plugin that bills thread time to
[Zoho Books](https://www.zoho.com/books/). The activity-recorder plugin
records when a thread had your attention; this plugin turns that into Zoho
time entries after your review, and reports what was billed per client.

Written in Scala.js: bb loads the linked ES module (`server.js`) as the
backend, and `app.tsx` is a thin React shell over the linked frontend
(`app-core.cjs`).

## Layout

- `billing-core/` — pure logic, cross-built for the JVM and JS: splitting
  intervals at local midnight, shared-time stretches and decisions, mapping
  inheritance, submission plans over what submissions cover, window subdivision, and the
  Zoho entry arithmetic (rounding, `HH:mm`, notes).
- `billing-core-test/` — munit tests for the pure logic (JVM).
- `billing/` — plugin backend: settings, the Zoho REST client, the SQLite
  store of decisions and pushed entries, the activity-recorder bridge, thread
  metadata access, the RPC, and the `bb billing` CLI.
- `billing-app/` — frontend (scalajs-react): the Billing page (to bill,
  projects, submissions, client report), the per-thread Billing panel, and the
  confirmation form an agent's send opens.
- `app.tsx` — registers the page (`navPanel`) and the thread panel.

## Build and install

Requires [bleep](https://bleep.build) to build; the built plugin is plain
JavaScript.

```sh
npm run build          # bleep link + copy server.js and app-core.cjs
bb plugin install .    # register this directory as a path plugin
```

After changing Scala sources: `npm run reload`.

## Settings

- **Zoho OAuth client id / client secret / organization id / refresh
  token** — a Zoho OAuth client with the `ZohoBooks.fullaccess.all` scope
  and a refresh token issued to it. Secrets stay on the server.
- **Billing increment (minutes)** — each dated entry sent to Zoho is rounded
  up to a multiple of this (default 1). The review shows exact minutes.

## How time is attributed

A thread's time goes to a Zoho project and task decided in this order:

1. The thread's own choice, kept in its `billing` metadata namespace as
   `{ "zoho": { projectId, projectName, taskId, taskName, customerId,
   customerName } }`. `{ "zoho": null }` means "not client work" and stops
   the search.
2. The nearest ancestor thread's choice.
3. The bb project's default: on the Billing page's **Projects** tab each bb
   project lists the Zoho projects its threads may bill to, each with a
   default task. A bb project with exactly one Zoho project bills every
   thread of it there unless the thread says otherwise.

A thread with none of these is not client work. Choose for a thread on the
Billing page's thread list, in the thread's own **Billing**
panel, or with `bb billing assign --project <name|id> --task <name|id>`.

## Billing a thread

The state is the set of **submissions**. Each records the exact stretches
(spans of a thread's recording) it sent to Zoho, or deliberately did not
bill, plus the Zoho entries it created. A stretch is billed when a
submission's span covers it, and not otherwise; "unbilled" is the recording
minus every submission's spans and minus struck-out stretches. There is no
"billed until": a draft is a set of stretches, ticked on the page or named by
key on the CLI, and "up to here" is only a way to tick them. A submission
covers the thread and the subthreads under it that bill to the same place,
and creates one Zoho time entry per calendar day the stretches fall on.
"Settle without billing" records the draft as a submission without entries. Time billed under a subthread is never billed again under its
parent, because each stretch belongs to one thread. Any submission can be
undone, in any order: its Zoho entries are deleted and its stretches are
unbilled again; nothing else changes.

The **To bill** tab is one list: every thread with recorded time as a tree by
bb project, with its "Bill to" choice, its account (unbilled time, what
submissions already cover, what is left out) and its standing. An archived
client thread with unbilled time needs a decision (archiving a parent archives
its subthreads too) and is marked so. "Review and bill…" on a row opens the
thread's **ledger**:

- **Every stretch, with its state.** The ledger shows the whole recording of
  the thread (and of the subthreads that bill with it), grouped by day, oldest
  first, one card per stretch: its clock times, its length, its description,
  a disclosure with the conversation during it (your turns, the agent's
  replies, the commands it ran) and a way into the thread. Each stretch is in
  exactly one state (`StretchState` in `billing-core`): unbilled, in Zoho by
  hand (the agent ran the Zoho timer over it), left out (struck out by you),
  sent (with the submission's date), or settled. Nothing leaves the screen
  when it is sent or struck out: its chip changes. The top line sums them:
  "4h 23m unbilled · 4h 23m ticked · 2h 25m sent in 2 entries · 5h 31m left
  out · 1h 43m in Zoho by hand".
- **A draft is the ticked stretches.** Unbilled stretches start ticked. Tick
  and untick cards, "Tick day", "Tick all unbilled", or "tick everything up to
  here" on a card, which is the convenience command for billing up to a
  moment. Stretches in Zoho by hand are unticked until you tick them; "leave
  out for good" strikes a stretch out across drafts until "put back". Ticks
  are instant (the totals are computed on the page with the same core code);
  the server prices the draft after each change: the entries per day, with
  their notes, under that day's cards.
- **Lines and notes.** Until "Write summaries" is pressed a card shows your
  first message in the stretch. The button has a model write, in one hidden
  thread on the model named by the `summaryModel` setting (`bb billing
  models` lists them; empty means the first Haiku bb offers), a few sentences
  per ticked stretch without a line and one paragraph per day from that day's
  sentences; the paragraph is the entry's notes, cached for that exact set of
  lines, editable before sending. The run can take minutes, so the request
  only starts it; the plugin publishes a `BillingEvent` on the "billing"
  realtime channel when it ends, and the screen reloads or says why it failed. Long stretches are cut at your own messages
  into pieces of about an hour; lines are cached per stretch.
- **Shared time to settle**: a stretch when two or more threads were open at
  once, cut anew whenever the set of open threads changes, shown under its
  day with every thread named and its context. Every stretch involving a
  client thread needs a choice before sending: all of it to one thread, a
  split by minutes, or nobody. Decisions are keyed by the stretch itself.
- **State, always stated**: the screen is a state machine (`LedgerPhase` in
  `billing-app`: loading, reviewing, writing summaries, sending, sent,
  failed); every screen is a function of the phase, a change that does not
  fit the phase is ignored, and a reply to a superseded request is dropped.
  The bottom bar carries the running total and the Send button; the
  confirmation repeats the amount, the days and the destination; after a send
  the ticked cards flip to "sent" in place, with Undo beside the banner. "Sent
  or settled before" lists the submissions covering these threads, each with
  Undo, in any order. Every thread carries its standing (`Standing` in
  `billing-core`): not client work, nothing recorded, open and unbilled, open
  and partly billed, archived and to settle, or settled.
- **In Zoho on these days, not from bb**: Zoho's own entries on the ledger's
  days, for checking against the "in Zoho by hand" stretches.

Without the activity-recorder plugin, the page says so and nothing can be
sent. The recorder's intervals are read through its own `activity-core`
parser: output it cannot read is shown as an error, never as lower totals.
When the recorder reports its speech feed as stale, or as having lost spans
converse discarded before they were synced, the page says that too.

## From an agent

Everything on the page is also a `bb billing` command, documented for agents
in the plugin's skill: `status`, `pending`, `threads`, `plan`, `exclude`,
`include`, `settle`, `settle-all`, `unsettle`, `notes`, `send`, `skip`,
`undo`, `submissions`, `assign`, `unbillable`, `clear`, `mapping`,
`projects`, `project-zoho`, and `report`. Sending from an agent goes through
the `billing_send_thread` tool: it shows the plan in the thread's composer
and writes to Zoho only when the user confirms there; if the plan changed
between the review and the confirmation, nothing is sent.

## Reports

**Client report** lists, per thread, the time sent to Zoho for a customer in
a date range (the current month so far by default; click the customer), with
Markdown to copy. `bb billing report --customer <name|id> --from yyyy-MM-dd
--to yyyy-MM-dd` prints the same Markdown. Only sent time counts.

Why it is this way, and what was turned down: [docs/decisions.md](docs/decisions.md).
