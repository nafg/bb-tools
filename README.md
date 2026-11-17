# bb-tools

Tools around [bb](https://github.com/get-bb/bb).

- `bb-plugin-activity-recorder/` — bb plugin (written in Scala.js) that records
  when you give each thread attention (speech, replies read aloud, typed
  messages, a focused pane), serves per-thread activity intervals over RPC, and
  prints per-thread totals with `bb activity-recorder summary`.
- `bb-plugin-agentmail/` — bb plugin (written in Scala.js) that lets agent
  threads send email via [AgentMail](https://agentmail.to) and routes replies
  back into the thread that sent them.
- `bb-plugin-billing/` — bb plugin (written in Scala.js) that bills recorded
  thread attention to Zoho Books: map threads to projects and tasks, review
  each week's time and overlaps, push time entries, and report per client.
- `bb-plugin-converse/` — bb plugin (written in Scala.js) for two-way voice
  conversations with agent threads: continuous listening with VAD,
  transcription into the thread, and spoken replies with barge-in.
- `bb-plugin-subscriptions/` — bb plugin (written in Scala.js) that lets a
  thread subscribe to something and get a message when it changes. It stores and
  delivers; source plugins check the targets. Threads are its own source.
- `bb-plugin-forge-github/`, `bb-plugin-forge-gitlab/` — subscription sources
  for GitHub pull requests and GitLab merge requests, branches and tags, read
  through `gh` and `glab`; their shared code is in `bb-plugin-forge-shared/`.
- `bb-plugin-maven-central/` — subscription source for Maven Central versions.
- `bb-plugin-wbs/` — bb plugin (written in Scala.js) that renders `*.wbs.yaml`
  work-breakdown estimates as a tree table with totals, in the file panel and
  via `bb wbs html`.
- `bb-plugin-facades/` — shared Scala.js facades for the bb plugin API, and
  small helpers every plugin backend needs (reading loosely typed values,
  Standard Schema values for RPC contracts, a one-in-flight coalescer), for
  this repo's plugins to depend on.

All Scala projects build from the single Bleep build at the repo root.
