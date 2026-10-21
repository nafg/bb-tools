# bb-plugin-subscriptions

A [bb](https://github.com/get-bb/bb) plugin that lets a thread subscribe to a GitHub pull request or GitLab merge
request and get a message when it merges, closes, or finishes CI. It replaces agents' own polling loops, which ran in
sandboxes, hit background-command time limits, and could mistake a failed check for "not merged yet".

Written in Scala.js, like the other plugins in this repo. `subscriptions-core` holds the platform-independent logic
(URL parsing, state and CI interpretation, change detection) and is tested on the JVM.

## Build and install

```sh
npm run build          # bleep link + copy the linked JS to ./server.js
npm test               # core tests
bb plugin install .    # register this directory as a path plugin
```

After changing sources: `npm run reload`.

## How it works

- A bb schedule runs every minute in the bb server. For each active subscription it reads the PR or MR through `gh`
  or `glab`, using their existing logins, and compares it with the state last reported to the thread.
- Changes are delivered with `bb.sdk.threads.send`. The new state is recorded only after delivery succeeds, so a
  failed delivery is retried on the next run.
- A failed check is reported to the thread once, and again when checking recovers.
- Subscriptions live in the plugin's SQLite database.

See `skills/subscriptions/SKILL.md` for the agent-facing usage.
