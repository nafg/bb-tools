# bb-tools

Tools around [bb](https://github.com/get-bb/bb).

- `bb-plugin-agentmail/` — bb plugin (written in Scala.js) that lets agent
  threads send email via [AgentMail](https://agentmail.to) and routes replies
  back into the thread that sent them.
- `bb-plugin-converse/` — bb plugin (written in Scala.js) for two-way voice
  conversations with agent threads: continuous listening with VAD,
  transcription into the thread, and spoken replies with barge-in.
- `bb-plugin-facades/` — shared Scala.js facades for the bb plugin API, for
  this repo's plugins to depend on.

All Scala projects build from the single Bleep build at the repo root.
