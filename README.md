# bb-tools

Tools around [bb](https://github.com/get-bb/bb).

- `bb-plugin-activity-recorder/` — bb plugin (written in Scala.js) that records
  when you give each thread attention (speech, replies read aloud, typed
  messages, a focused pane), serves per-thread activity intervals over RPC, and
  prints per-thread totals with `bb activity-recorder summary`.
- `bb-plugin-agentmail/` — bb plugin (written in Scala.js) that lets agent
  threads send email via [AgentMail](https://agentmail.to) and routes replies
  back into the thread that sent them.
- `bb-plugin-converse/` — bb plugin (written in Scala.js) for two-way voice
  conversations with agent threads: continuous listening with VAD,
  transcription into the thread, and spoken replies with barge-in.
- `bb-plugin-wbs/` — bb plugin (written in Scala.js) that renders `*.wbs.yaml`
  work-breakdown estimates as a tree table with totals, in the file panel and
  via `bb wbs html`.
- `bb-plugin-facades/` — shared Scala.js facades for the bb plugin API, and
  small helpers every plugin backend needs (reading loosely typed values,
  Standard Schema values for RPC contracts, a one-in-flight coalescer), for
  this repo's plugins to depend on.

All Scala projects build from the single Bleep build at the repo root.
