# Roast of the Billing page on a phone (from the goals and guide only)

Ranked by harm to the main goal: "bill an archived client thread's unbilled work from my phone and
be sure nothing is billed twice."

## Breaks the goal

1. **The one table that matters does not fit the screen.** (G3, G9; guide screen map, G3 Method A,
   G9.) The stretch table has four columns, three of them no-wrap, so at 390 px the description is
   crushed and the controls sit off-screen to the right. Picking stretches means scrolling sideways
   for every row. A user expects each stretch to be a card that fits the width, with its controls
   under it. Root cause: the stretch is modelled as a table row (a grid of fields), not as an item
   with a body and actions.

2. **"Billed" and "not billed" are shown by absence.** (G10; guide G10 "Know this".) A sent stretch
   vanishes from the table; a struck-out stretch stays as a crossed line; a stretch after the cut
   moves to a second table. There is no place where a thread's recording is shown whole with each
   stretch's state. The owner's "shows already submitted only on one 28m span" and "will it lose the
   activity after it?" are this: the page never says, per stretch, "sent on Oct 26 (entry …)",
   "struck out", "after the cut", "unbilled". A user expects the thread's timeline with a state on
   each stretch. Root cause: the panel is a draft of one submission, not a view of the thread; the
   thread's billing state (the set of submissions covering it) has no screen of its own.

3. **Double-billing evidence is three screens down and does nothing.** (G5.) The chip that says
   "already in Zoho" sits inside the crushed description cell; the two lists are below the notes
   and the shared stretches; the audit exists only in the CLI. The user must notice, understand the
   timer wording, then strike rows out one by one. A user expects the page to say up front "2h 55m
   of this is already in Zoho by hand" and offer "leave it out". Root cause: "already in Zoho" is
   modelled as a decoration on a line, not as a state of the stretch that the submission honours.

4. **The cut and the strike-out are not the selection the user means.** (G3, G8.) "Up to here" is a
   moment; the stretches after it leave the table, then come back in a second table with a
   different button label. "✕" is permanent across submissions and invisible elsewhere. The owner
   said "I clicked up to here because I only wanted those activities in that particular entry", a
   per-stretch selection, and the product gave him a time cut with side effects. A user expects to
   tick the stretches that go into this entry. Root cause: the submission's input is modelled as
   (moment, global exclusions) instead of (set of stretches), even though the stored state is a set
   of spans.

## Slow or confusing

5. **Every action reloads everything.** (G3, G4, G6; guide "Know this" on reloads.) A strike-out
   greys out the whole panel for seconds while two threads' conversations are refetched and the
   whole plan is recomputed. On a phone each tap costs a wait and a scroll back. A user expects a
   strike-out to be instant. Root cause: the plan is one opaque server computation; the client holds
   no model of the stretches, so it cannot apply a local change and send the delta.

6. **The page tells before it shows.** (G1, G9.) A seven-line paragraph and three setup steps come
   before the list, every time, even when set up. The panel opens with a status sentence, a "Where
   you stand" box of two long bullets with 19-digit ids, five lines of controls, a "Covers" line, a
   help sentence, then the data. A user expects the list first and help on demand. Root cause: the
   prose stands in for structure; the screen explains the model because the model is not visible in
   the layout.

7. **"Where you stand" and "Recent activity" are the same thing twice, with ids.** (G7, G8, G10.)
   Recent activity lists five submissions of all threads between the pending list and the panel,
   each with "(entry 1175410000015086002)"; the panel repeats the ones for this thread. Neither
   says which stretches they are. A user expects one history, per thread, in words ("Oct 8 morning,
   3h 21m, sent Oct 26"). Root cause: submissions are keyed by id and described by spans of clock
   time instead of by the stretches they contain.

8. **Reading a stretch means reading its whole transcript or a 400-character line.** (G2.) The
   disclosure dumps every turn into a narrow cell. A user expects a line, then a short list of the
   decisions and commits, then a way to the exact place. Root cause: there is no summary layer
   between "line" and "transcript", and no message anchor.

9. **The vocabulary shifts.** "stretch", "window", "line", "notes", "entry", "span", "submission",
   "settled", "struck out", "left out", "cut", "covers". The guide needs eleven terms for one task.
   A user expects "stretch", "entry", "sent". Root cause: internal names leak; the UI speaks the
   implementation's terms.

## Edges

10. The tab labels wrap; the native datetime-local on a phone is a two-step picker for a value the
    user rarely types.
11. "Write summaries" is the only way to get real lines and notes, yet nothing on a placeholder line
    says it is a placeholder (G2, G6).
12. The pending row cannot say that a thread holds hand-entered Zoho time (G5).
13. After "Undo this send" on the Recent activity card the open panel does not reload (G8).
