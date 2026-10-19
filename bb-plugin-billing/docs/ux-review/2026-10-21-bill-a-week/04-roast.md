# Roast of the Billing page (from the goals and guide only)

Ranked by harm to the main goal, "bill last week's work on a client's project".

## Breaks the main goal

1. **The page shows the end of a pipeline whose beginning is somewhere else.** (G1, G2.) First visit: Zoho unconfigured, no thread mapped. The page renders seven day cards, ten threads all "not billable", overlaps with "no billable thread involved", and a "Preview Zoho entries" button on every day. Nothing says "connect Zoho, then say which threads are client work". A user expects the screen to tell them the next step, or to offer it in place. Root cause: the model has three stages (connect, map, review and push) but the UI exposes only the third and hides the first two in other screens.

2. **Mapping a thread cannot be done where the thread is shown.** (G2.) The table has a "Billed to" column reading "not billable" with no control. The real control is three clicks away under a "+" menu on a different screen, and a sixth step is "come back and Reload". A user expects "Billed to" to be editable there. Root cause: ownership. The mapping is stored on the thread (right), but the only editor is bound to the thread *page* instead of to the thread *wherever it appears*.

3. **Not-billable threads dominate every table.** (G3, G8.) In the sample week, all ten rows are not billable, so the billable signal is zero rows among ten, and the "billable 0m" summary looks like a bug. A user expects to see client work, with other threads out of the way. Root cause: the review treats billable and unbillable threads alike, though they matter for different reasons (billable rows are the output; unbillable rows matter only when they overlap a billable one).

## Slow or confusing

4. **Undefined vocabulary.** (G4.) "overlap", "Alone", "From overlaps", "decision", "stale", "Preview Zoho entries", "Give all of this day's overlaps to the billable thread where there is exactly one". None is explained on screen. A user expects plain words: "at the same time as", "on its own", "counted for this thread".

5. **Seven day cards regardless of content.** (G5.) Days with no attention still get a card and a Preview button. Overlaps nobody can act on are listed. A user expects empty days collapsed and actionable items first.

6. **No sense of readiness or of the week as a whole.** (G3, G5.) Nothing says "this day is ready to push" before the preview, and there is no week total per client. A user expects a summary line: "Acme: 6h 20m across 4 days, 2 overlaps to settle, 3 days ready".

7. **The zone banner on first visit is noise.** (G1.) It warns about decisions going stale before the user has made any. It also hints at "Zoho is not configured" without saying where to fix that.

## Edges

8. "Reload" is needed after mapping in another screen; nothing refreshes by itself.
9. The report tab fails with a Zoho error on first visit instead of saying what it needs.
