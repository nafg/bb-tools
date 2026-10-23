# bb-plugin-subscriptions

A [bb](https://github.com/get-bb/bb) plugin that lets a thread subscribe, by URL, to a GitHub or GitLab pull/merge
request, branch or repository's tags, and get a message when it merges, closes, finishes CI, gets a new commit, review
or comment, when the branch gets new commits, or when a new tag appears. It replaces agents' own polling loops, which ran in
sandboxes, hit background-command time limits, and could mistake a failed check for "not merged yet".

Written in Scala.js, like the other plugins in this repo. `subscriptions-core` holds the platform-independent logic
(URL parsing, state per kind of target, CI interpretation, change detection) and is tested on the JVM.

## Build and install

```sh
npm run build          # bleep link + copy the linked JS to ./server.js
npm test               # core tests
bb plugin install .    # register this directory as a path plugin
```

After changing sources: `npm run reload`.

## How it works

- A bb schedule runs every minute in the bb server. For each active subscription it reads the target through `gh` or
  `glab`, using their existing logins, and compares it with the state last reported to the thread. Each check of a
  GitHub target costs about one point of GitHub's 5,000-point hourly allowance, which every other `gh` use shares.
- Reviews, comments and reactions cost four more GitHub calls (two on GitLab), so they are fetched only when the request's
  updated time changed, and for every subscription once every 15 runs as a safety net.
- Changes are delivered with `bb.sdk.threads.send`. The new state is recorded only after delivery succeeds, so a
  failed delivery is retried on the next run.
- A failed check is reported to the thread once, and again when checking recovers.
- Subscriptions live in the plugin's SQLite database.

## Checking it in production

`scripts/verify.sh [days]` checks that the plugin is running and logging no errors, compares each active
subscription's stored state with GitHub and GitLab (worked out independently of the plugin's code), and lists the
messages each thread received, flagging duplicates and missing "ended" messages. It needs network access and the
`bb` CLI, so run it outside any sandbox.

See `skills/subscriptions/SKILL.md` for the agent-facing usage.
