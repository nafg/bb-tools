# bb-plugin-goals

See bb threads through the goals they serve. Each thread can carry a goal
(B billable client work, C client work not billable, P product or future
income, S sanity, M maintainer obligations, F fun), a value within that goal
(1 biggest lever to 3), the threads it waits on, and what the user still owes
it. The Goals page draws the threads active in a window, archived ones
included, as nested boxes grouped by goal and sized by attention time from the
activity-recorder plugin.

## What it stores

Four optional keys in the plugin's thread metadata namespace (`goals`):
`goal` (a letter), `value` (1 to 3), `myMove` (`quick`, `focus`, or
`later`), and `myMoveAt` (when it was set).

The threads a thread waits on are not stored here. Each wait is the
subscriptions plugin's subscription of the blocked thread to the blocker
being archived (`bb:thread/<id>/archived`), so a blocked thread is always told
when its blocker finishes. Goals reads them with that plugin's `threadWaits`
rpc and changes them with `waitOnThread` and `stopWaitingOnThread`. Without
the subscriptions plugin no thread waits on any other, and setting a wait
fails. A wait on a thread that has already finished is not made. When the
blocker is archived or deleted, the subscription ends and so does the wait.
On start, goals moves any `blockedBy` list it stored itself before this into
subscriptions and removes the key; an archived thread's list is dropped.

- A thread without its own `goal` is read as having the nearest labeled
  ancestor's goal and value, the value always coming from the same thread as
  the goal.
- `blockedBy` never inherits. Each open, labeled thread lends its goal and
  value to every thread it waits on, transitively and through closed threads,
  which raises the blocker's importance without changing what it stores.
- `myMove` is what the user owes the thread, independent of unread. It clears
  by itself when the user next writes to the thread, unless it was set after
  that message.
  The user sets it; nothing suggests it, because plugin SDK 0.6.49 lets a
  plugin register an AI service but gives it no way to call a model.

## Surfaces

- A **Goal** pill in the thread header; its popover is the same editor as the
  Goals page's pane header (goal, value, my move, waiting on).
- **Goal** and **Goal value** entries in thread menus, and a glyph on sidebar
  rows of labeled threads. Both appear only in lists that render plugin menu
  actions and row statuses; bb-sidebar 0.2.34 renders neither, so there only
  the header pill and the Goals page show labels.
- The `goals_set_label` agent tool and `bb goals show | set | list | import`
  (see `skills/goals/SKILL.md`).
- The **Goals** page, with two views:
  - **Map**: containment and area say goal and attention (parallel threads each
    counted in full, so the sum is not hours of the day). Fill hue is the
    thread's goal; fill strength is its importance, lending included; the
    outline marks only the open thread; the corner marker means "your turn"
    (bb's attention states, or a quick or focus mark) or, as a pulsing ring,
    that the agent is working. Where a label came from and whether it has a
    value are in the tooltip, the table, and the pane header.
  - **Last 24 hours**: a donut of attention by goal with its legend, and the
    threads that received it.
  - A **Waiting on you** list above the map: the user's marks and bb's
    attention states, grouped quick, focus, unread, later, each most important
    first; Next and the `n` key walk it. A pane beside the map shows the
    picked thread's chat and label editor; its width is draggable and kept in
    the browser.

A thread appears for a window when it was last attended since the window began
(bb's latest attention time, which archiving does not move), or has attention
recorded in it, or carries a my-move mark. Without the activity recorder the
page says so and draws the tree unsized.

## Layout

- `goals-core/`: pure logic, cross-built JVM/JS: label parsing, inheritance,
  lending, the squarified treemap, the overview model, and CSV import.
- `goals-core-test/`: munit tests for the above (JVM; `scripts/test-js.sh`
  at the repo root also runs them under Scala.js).
- `goals/`: plugin backend (Scala.js): rpc, the agent tool, the CLI, and
  the activity-recorder calls. It depends on the recorder's `activity-core`
  project for parsing `intervals`, so the recorder's branch must land first.
- `goals-app/`: frontend core (Scala.js): `layoutOverview`, the treemap
  layout consumed by `app.tsx`.
- `app.tsx`: the React surfaces.

## Build and install

```sh
npm install
npm run build          # bleep-links server.js and app-core.cjs
bb plugin install .    # from this directory
```

`npm run reload` rebuilds and reloads an installed copy. The plugin builds
against plugin SDK 0.6.49, newer than the converse and wbs plugins' pin.
