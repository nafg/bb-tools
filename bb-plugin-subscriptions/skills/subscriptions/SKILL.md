---
name: subscriptions
description: Wait for a GitHub or GitLab pull/merge request to merge, close or finish CI, for new commits on a branch, or for a new tag, without polling. Use whenever a thread needs to know when one of those happens.
---

# Subscriptions

Subscribe the thread by URL and end the turn. bb checks every minute from the bb server and sends the thread a message
when something happens. Do not write `gh` or `glab` polling loops, background watchers or `gitlab-watch` monitors for
this.

| Subscribe to | URL | The thread hears |
|---|---|---|
| A pull or merge request | its page | merged, closed or reopened; CI passed, failed or canceled on a commit |
| A branch | `…/tree/<branch>` (GitLab `…/-/tree/<branch>`) | each time it moves, with the new commits' subjects |
| A repository's tags | `…/tags` (GitLab `…/-/tags`) | each new tag |

## Tools and commands

| Native tool | Command | What it does |
|---|---|---|
| `subscriptions_add` | `bb subscriptions add <url> [--thread <id>]` | Subscribe; returns the current state at once |
| `subscriptions_list` | `bb subscriptions list [--thread <id> \| --all]` | Show active subscriptions and what was last seen |
| `subscriptions_cancel` | `bb subscriptions cancel <id>` | Cancel one |
| `subscriptions_who` | `bb subscriptions who <url>` | List threads that subscribed to a URL, now or before |

`who` answers "which thread is working on this MR?". Subscribing suggests, but doesn't prove, that a thread works on
it: a coordinator may subscribe to MRs it doesn't own. The list links each thread, current subscribers first.

`--thread` subscribes another thread, for example a subthread that should hear about its own MR.

## What the thread hears

Messages start with `[bb subscription #<id>]` and end with the URL.

- A PR or MR was merged, closed without merging, or reopened.
- CI on a PR or MR finished on a commit: passed, failed or canceled. Each result is reported once per commit. Pending
  CI is not reported.
- A branch moved: the old and new commit, how many commits are new, and the subjects of up to ten.
- New tags appeared. Deleted tags are not reported. Only the newest 100 tags are compared.
- Checking failed, with the error, and later that checking works again. A failed check is never treated as "nothing
  changed".

A PR or MR subscription ends by itself when it is merged or closed. If it is already merged or closed when you
subscribe, no subscription is made and the reply says so. Branch and tag subscriptions last until canceled. If a
subscribed branch is deleted, the thread is told that checking failed.

## Limits

- Checks run every minute, so a message can arrive up to about a minute after the event.
- Archived threads are skipped until they are unarchived; they then hear what changed. Deleting a thread ends its
  subscriptions.
- It reads through the `gh` and `glab` logins of the user running bb, so it sees what they can see.
- Reviews, comments, individual CI jobs, releases and whole-repo subscriptions are not covered yet.
- Branch moves are reported as the new commits since the last check, not one message per push.
