---
name: subscriptions
description: Wait for a GitHub pull request or GitLab merge request to merge, close or finish CI without polling. Use whenever a thread needs to know when a PR or MR merges, closes, or its CI passes or fails.
---

# Subscriptions

Subscribe the thread to a pull or merge request and end the turn. bb checks it every minute from the bb server and
sends the thread a message when something happens. Do not write `gh` or `glab` polling loops or background watchers
for this.

## Tools and commands

| Native tool | Command | What it does |
|---|---|---|
| `subscriptions_add` | `bb subscriptions add <url> [--thread <id>]` | Subscribe; returns the current state at once |
| `subscriptions_list` | `bb subscriptions list [--thread <id> \| --all]` | Show active subscriptions and what was last seen |
| `subscriptions_cancel` | `bb subscriptions cancel <id>` | Cancel one |

`--thread` subscribes another thread, for example a subthread that should hear about its own MR.

## What the thread hears

Messages start with `[bb subscription #<id>]` and end with the URL.

- The PR or MR was merged, closed without merging, or reopened.
- CI finished on a commit: passed, failed or canceled. Each result is reported once per commit. Pending CI is not
  reported.
- Checking failed, with the error, and later that checking works again. A failed check is never treated as "nothing
  changed".

The subscription ends by itself when the PR or MR is merged or closed. If it is already merged or closed when you
subscribe, no subscription is made and the reply says so.

## Limits

- Checks run every minute, so a message can arrive up to about a minute after the event.
- Archived threads are skipped until they are unarchived; they then hear what changed. Deleting a thread ends its
  subscriptions.
- It reads through the `gh` and `glab` logins of the user running bb, so it sees what they can see.
- Reviews, comments, individual CI jobs, releases and whole-repo subscriptions are not covered yet.
