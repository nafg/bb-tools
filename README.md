# bb-tools

Tools around [bb](https://github.com/get-bb/bb).

- `linux-window/` — unofficial Linux desktop window for bb: a launcher script
  that starts `bb-app` if needed, plus a minimal WebKitGTK window written in
  Scala Native (Bleep build) and a desktop entry. Stopgap until the official
  Linux desktop build
  ([get-bb/bb#1064](https://github.com/get-bb/bb/issues/1064)) lands.
- `bb-plugin-agentmail/` — bb plugin (written in Scala.js) that lets agent
  threads send email via [AgentMail](https://agentmail.to) and routes replies
  back into the thread that sent them.
- `bb-plugin-facades/` — shared Scala.js facades for the bb plugin API, for
  this repo's plugins to depend on.

All Scala projects (the Scala Native window and the Scala.js plugins) build
from the single Bleep build at the repo root.
