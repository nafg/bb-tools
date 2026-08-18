---
name: agentmail
description: Send and receive email from this thread via AgentMail. Use when the user asks to email someone (a proposal, a question, a document) or to check for / act on email replies. Email is drafted for the user's review; replies to sent email arrive back in this thread automatically.
---

# Email via AgentMail

This bb install has an AgentMail plugin: one shared email inbox that any thread
can send from. The plugin remembers which bb thread sent each email thread, and
when a reply arrives (polled every few minutes) it is delivered back into that
bb thread as a new message. You do not need to poll for replies — they show up
on their own.

The inbox is the AI assistant's address, not the user's.

## The one rule: you file drafts; only the user sends

You cannot send email. The tools below create and revise *drafts*; the user
reviews each draft in this thread's **Email panel** (a tab in the thread's
right panel) and sends it there with the Send button — or discards it. Never
tell the user an email was sent unless you received the message in this thread
saying the user sent it.

- `agentmail_send` — file a new-email draft: `to` (required list) and `body`
  (required plain text), plus optional `cc`, `subject`, `html`, and
  `attachments` (absolute paths on the machine this thread's environment runs
  on). Returns immediately with the draft id.
- `agentmail_reply` — file a reply draft within an existing email thread:
  `thread` (the AgentMail thread id) and `body`, plus optional `html` and
  `attachments`. Recipients, subject, and threading derive from the message
  being replied to.
- `agentmail_update_draft` — revise a pending draft when the user asks for
  changes: `draft` (the id) plus any of `to`, `cc`, `subject`, `body`, `html`,
  `attachments` (attachments are added, not replaced). A new `body` replaces
  any earlier HTML alternative unless fresh `html` comes with it.

The user can also edit the draft directly in the panel (autosaved), so a
draft's current content may differ from what you filed.

After filing or updating a draft, tell the user it is ready to review and
include this directive on its own line so they can open the review in one
click:

```
::agentmail{draft="DRAFT_ID"}
```

When the user sends a draft, a message is delivered into this thread with the
final recipients and subject, and the AgentMail thread id for follow-ups.

## Incoming email

Incoming replies are delivered into this thread as messages containing the
sender, subject, and body. Respond to them conversationally; if the user wants
to reply by email, file a reply draft with `agentmail_reply`. The user can
read the full back-and-forth in the Email panel, so keep your summary of a
received email brief and include the `::agentmail{}` directive (no draft
attribute) to link the panel.

## Inspecting

```
bb agentmail drafts             # pending drafts filed by this bb thread
bb agentmail threads            # email threads owned by this bb thread
bb agentmail read --thread ID   # full messages of one email thread
bb agentmail attachment --message MESSAGE_ID --attachment ATTACHMENT_ID --out /abs/path
```

Delivered reply messages list their attachments with the exact `attachment`
command to download each one.

## If the tools are missing

The native tools appear in sessions started after the plugin was installed.
If this session predates that, the CLI equivalents behave identically but run
under your shell tool:

```
bb agentmail send --to a@b.com [--to ...] [--cc ...] --subject S --body TEXT [--html HTML] [--attach /abs/path]...
bb agentmail reply --thread AGENTMAIL_THREAD_ID --body TEXT [--html HTML] [--attach /abs/path]...
bb agentmail update-draft --draft DRAFT_ID [--subject S] [--body TEXT] ...
```

These also only file drafts — nothing is sent until the user sends it from
the Email panel.
