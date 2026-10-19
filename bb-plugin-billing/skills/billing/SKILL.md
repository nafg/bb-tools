---
name: billing
description: Bill recorded bb thread time to Zoho Books: say which Zoho project a bb project or thread bills to, review a thread's stretches of work, settle time shared between threads, send a thread up to a moment after the user confirms, mark a thread settled without billing, and report time sent per customer. Use when the user talks about billing, invoicing, timesheets, client hours, Zoho Books, or which client a thread's work is for.
---

# Billing: recorded thread time to Zoho Books

bb records when each thread has the user's attention (speaking, typing,
reading). Nothing is logged by hand. The state is the set of **submissions**:
each records the exact stretches of recording it sent to Zoho (or settled
without billing) and the Zoho entries it made. A stretch is billed when a
submission covers it, and not otherwise; there is no "billed until". A
submission is made by choosing stretches, normally everything unbilled **up
to a moment**, and sending them as one Zoho Books time entry per calendar
day, with a summary of what the user did. Archiving a thread is the natural
time to bill it; an open thread can be billed at any time. Any submission can
be undone, in any order. You can do all of it with `bb billing …`, except
that sending always pauses for the user to confirm.

## Where a thread's time goes

In this order: the thread's own choice; the nearest ancestor thread's choice;
the bb project's default (a bb project with exactly one Zoho project bills
every thread of it there); else not client work.

- `bb billing project-zoho` lists bb projects and their Zoho projects.
  `bb billing project-zoho <bb project> --add <zoho project> [--task <task>]`
  adds one; `--remove <zoho project>` drops it. `bb billing projects` lists
  the Zoho projects and tasks (`--refresh` fetches from Zoho).
- `bb billing assign --project <name|id> --task <name|id> [--thread <id>]`
  chooses for one thread (and its descendants without their own choice).
  `bb billing unbillable` marks a thread not client work; `bb billing clear`
  removes its own choice; `bb billing mapping` shows where it bills and why.
  `--thread` defaults to the current thread.

## What needs a decision

- `bb billing pending`: archived client threads that still have unbilled
  recorded time. Each needs billing or `skip`.
- `bb billing threads [--since yyyy-MM-dd]`: every thread with recorded time
  as a tree by bb project, with where it bills, what is unbilled, and what
  submissions already cover.

## Reviewing and sending a thread

- `bb billing models`: the models bb offers for the summaries, as
  `provider/model` for the `summaryModel` plugin setting (empty uses bb's
  default model). The status line says which one is in use.

- `bb billing plan <thread> [--keys <k1,k2,…>] [--no-subthreads]
  [--summaries]`: the thread's account and what sending the draft would do.
  The draft is a set of stretches: every unbilled stretch unless `--keys`
  names the ones to send. It lists every stretch of work with its clock
  times, a line describing it and its key. Without `--summaries` the line is
  the user's first message in the stretch; with it, a model first describes
  every stretch of the draft that has no line yet, in one hidden thread (one
  agent run; the lines are cached, so ask once per submission, after the user
  has settled which stretches go); subthreads that bill to the same place are
  covered too; the shared stretches still to settle with their keys; the
  dated Zoho entries with their notes; and any time already in Zoho on those
  days that bb did not create (entered by hand; check for double billing).
- `bb billing exclude <window key>` strikes a stretch out of the billing
  (already invoiced by hand, not client work); `include` puts it back.
- A shared stretch is a span when two or more threads were open at once; it
  needs the user's decision when a client thread is involved. Ask who gets
  it; do not guess. `bb billing settle <thread> <key> --to <thread>` gives it
  all to one thread; `--split <thread>=<minutes>,…` divides it; `--nobody`
  bills none. `bb billing settle-all <thread>` gives every stretch that has
  exactly one client thread to it. `bb billing unsettle <key>` takes one back.
- `bb billing notes <thread> <yyyy-MM-dd> <text>` replaces one entry's notes.
- To send, call the **`billing_send_thread`** tool with the thread id (and
  `keys` when only some stretches should go). It shows the user the draft in
  this thread and sends only if they confirm; never say time was sent unless
  the tool said so. `bb billing send` does the same from a terminal, but a
  shell may time out while the user decides.
- `bb billing skip <thread> [--keys …]` marks the draft settled without
  billing anything. `bb billing undo <submission id>` deletes a submission's
  Zoho entries and makes the time unbilled again; `bb billing submissions`
  lists them.

## Reports

`bb billing report --customer <name|id> --from yyyy-MM-dd --to yyyy-MM-dd`
prints, per thread, the time sent to Zoho for a customer in that range.
`bb billing status` says whether Zoho is connected and the recorder reachable.
