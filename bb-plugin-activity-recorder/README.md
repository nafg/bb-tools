# bb-plugin-activity-recorder

Records when you give each bb thread attention, and serves per-thread
activity intervals to other plugins over RPC. It knows nothing about goals,
clients, or billing; those are other plugins' concerns.

## What it records

Raw signals, one row each. The recorder never deletes them, but it can
only keep what it managed to pull: converse expires its spans after 90
days, so a stretch in which the recorder was off or its sync failing for
longer than that loses that voice time, and `status` then reports `lost`
with the converse sequences concerned.

- `speech` and `playback`: you spoke to the thread, or its reply was read
  aloud. Pulled from the converse plugin's `activitySpans` RPC once a minute.
  Without converse, or before it serves that method, the status reports it
  unavailable and the rest keeps working.
- `message`: you sent the thread a typed message. Messages sent by agents,
  by bb, or by automations ("[bb ..." prefix) are not recorded.
- `turn`: the thread's agent ran a turn. Recorded, but left out of intervals
  unless asked for: it is the agent's time, not yours.
- `view`: the thread's pane was focused, the page visible and focused, and
  you had given input within the last 5 minutes. Reported by the frontend
  every 30 seconds while that holds.

Messages and turns come from bb's thread event log, including everything
from before the plugin was installed: on load it reads every thread once, and
then follows new events as they arrive.

Each thread pane's header shows the thread's attention today as a timer. It
runs, with a red dot, while that pane is being viewed and counting; it
stands still otherwise, and re-reads the recorder's total every minute.

## Intervals

Per thread, signals no further apart than the gap setting (5 minutes by
default) join into one interval; a longer gap counts as nothing. Intervals of
different threads overlap freely. A lone typed message is a zero-length
interval. The derivation happens at read time, so changing the gap changes
history.

## Reading hours off

```sh
bb activity-recorder summary                      # today
bb activity-recorder summary --date 2026-10-17
bb activity-recorder summary --week               # Monday to Sunday of this week
bb activity-recorder summary --week --week-start sunday   # Sunday to Saturday, as billing counts it
bb activity-recorder summary --from 2026-10-13 --to 2026-10-17
```

Each thread's attention, most time first, with its title, and a total that
counts parallel threads in full. Dates are the bb server's local days, and
a week runs Monday to Sunday unless `--week-start sunday` says otherwise.
Threads touched by a message but with no measured time are counted
separately; `--touched` lists them, `--json` gives the same as data.

## RPC

See `ActivityRecorderRpc` in `activity-core` for the contract: `intervals`,
`signals`, and `status`, under plugin id `activity-recorder`, registered as
discoverable. `ActivityJson` decodes the responses.

## Layout

- `activity-core/` — shared, cross-built: signal and interval types, the
  gap rule, and the JSON shapes.
- `activity-core-test/` — munit tests for the gap rule (JVM).
- `activity/` — plugin backend (Scala.js): store, ingestion, RPCs.
- `activity-app/` — frontend core (Scala.js): the view tracker.
- `app.tsx` — thin React shell mounting one probe per thread pane.

## Build and install

```sh
npm install
npm run build          # bleep-links server.js and app-core.cjs
bb plugin install .    # from this directory
```

## Settings

- **gapMs** — the gap threshold in milliseconds (default `300000`).
- **staleAfterMs** — `status` reports the converse sync as stale when its
  newest span is older than this (default `86400000`, one day).
