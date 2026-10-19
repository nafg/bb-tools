# Design: what the roast's root causes change

## Root cause A: the page shows stage three of a three-stage model (roast 1, 7, 9)

The model is: (1) connect Zoho, (2) mark which threads are client work and for which project and task, (3) review each day's time, settle shared time, and send it. The page showed only (3). Now the week review opens with a **Setup** strip that states each stage and offers its next step in place: "Zoho: not connected → Open plugin settings" / "connected"; "Client threads this week: 0 of 10 → pick a project in the table below"; "Send: N days ready". The zone note moves into the Zoho line. The report tab says what it needs instead of showing an error.

## Root cause B: the mapping editor is bound to the thread page (roast 2, 8)

The mapping belongs to the thread, so it is editable wherever the thread is shown. The week review gets a **Threads this week** table, one row per thread with a "Bill to" select (every "Customer: Project / Task" from Zoho, plus "Not client work" and "Same as parent thread"). Changing it saves and reloads the week. The thread panel stays for use from the thread itself.

## Root cause C: billable and other threads are treated alike (roast 3, 5, 6)

Billable threads are the output; other threads matter only where they share time with a billable one. So: the threads table lists client threads first with week totals per customer above it, and folds the rest under "N other threads"; day cards render in full only for days that have client time or shared time to settle, and the rest collapse to one line; each day states its readiness ("ready to send: 2 entries" or "2 shared stretches to settle first") before any button is pressed; the per-day table shows client threads and only the other threads that share time with them, with "show all" to expand.

## Words (roast 4)

Overlap → "shared time" (time when two or more threads were open at once); Alone → "On its own"; From overlaps → "From shared time"; decision → "who gets it"; stale → "decisions that no longer match the recording"; Preview Zoho entries → "Review what goes to Zoho"; the day action → "Give shared time to the client thread wherever it is the only one". Each section heading carries a one-line explanation.
