# bb-plugin-subscriptions

A [bb](https://github.com/get-bb/bb) plugin that lets a thread subscribe to something and get a message when it
changes. It replaces agents' own polling loops, which ran in sandboxes, hit background-command time limits, and could
mistake a failed check for "not merged yet".

This plugin stores subscriptions, polls them, retries and delivers messages. The checking is done by **source
plugins**:

| Source plugin | Targets |
|---|---|
| this plugin | another bb thread being archived (`bb:thread/thr_…/archived`) |
| `forge-github` | GitHub pull requests, branches and tags |
| `forge-gitlab` | GitLab merge requests, branches and tags |
| `maven-central` | `maven:group:artifact[:version]` |

A source registers two discoverable RPC methods (see `source/` and `source-rpc/`): one recognizes a target string
and gives its canonical URL, the other checks a target against the state it returned last time and returns the new
state, the messages to send, and whether the subscription is finished. The state is opaque to this plugin. Any plugin
can become a source by registering them.

Written in Scala.js, like the other plugins in this repo. `source` holds the contract and text helpers,
`subscriptions-core` the thread source's logic; both are tested on the JVM.

## Build and install

```sh
npm run build          # bleep link + copy the linked JS to ./server.js
npm test               # core tests
bb plugin install .    # register this directory as a path plugin
```

After changing sources: `npm run reload`.

## How it works

- A bb schedule runs every minute in the bb server. For each active subscription it asks the subscription's source
  plugin to check the target against the state recorded last time. Every 15th run asks sources to also read what they
  normally read only when something changed.
- A failed check is retried once, five seconds later, before the thread hears about it.
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
