# bb-plugin-goals

See bb threads through the goals they serve. Each thread can carry a goal
(B billable client work, C client work not billable, P product or future
income, S sanity, M maintainer obligations, F fun), a value within that goal
(1 biggest lever to 3), and the threads it waits on. The Goals page draws
every open thread as nested boxes, grouped by goal and sized by attention time
from the activity-recorder plugin.

## What it stores

The plugin keeps three optional keys in its thread plugin-metadata namespace
(`goals`): `goal` (a letter), `value` (1 to 3), and `blockedBy` (thread ids).
A thread without its own `goal` is read as having the nearest labeled
ancestor's goal and value, the value always coming from the same thread as the
goal. `blockedBy` never inherits; instead each open, labeled thread lends its
goal and value to every thread it is waiting on, transitively, which raises
the blocker's importance without changing what it stores.

## Surfaces

- A **Goal** control in the thread header: the current label, and a popover to
  set goal, value, and the threads this one waits on.
- **Goal** and **Goal value** entries in thread menus (bb's own thread list
  and thread header menus).
- A glyph on sidebar rows of labeled threads, in lists that draw plugin row
  statuses.
- The `goals_set_label` agent tool, so a label can be set by telling the agent.
- `bb goals show | set | list | import` (see `skills/goals/SKILL.md`).
- The **Goals** page: attention per goal as bars, then the nested-box map.
  Box area is attention time in the chosen window plus a floor, so every
  thread shows; parallel threads are each counted in full, so the sum is not
  hours of the day. Dashed borders mark inherited labels; an arrow runs from a
  waiting thread to the one it waits on. Without the activity recorder the
  page says so and draws the tree unsized.

## Layout

- `goals-core/` — pure logic, cross-built JVM/JS: label parsing, inheritance,
  lending, the squarified treemap, the overview model, and CSV import.
- `goals-core-test/` — munit tests for the above (JVM; `scripts/test-js.sh`
  at the repo root also runs them under Scala.js).
- `goals/` — plugin backend (Scala.js): rpc, the agent tool, the CLI, and
  the activity-recorder calls.
- `goals-app/` — frontend core (Scala.js): `layoutOverview`, the treemap
  layout consumed by `app.tsx`.
- `app.tsx` — the React surfaces.

## Build and install

```sh
npm install
npm run build          # bleep-links server.js and app-core.cjs
bb plugin install .    # from this directory
```

`npm run reload` rebuilds and reloads an installed copy.
