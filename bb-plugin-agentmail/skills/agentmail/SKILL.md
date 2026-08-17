---
name: agentmail
description: Send and receive email from this thread via AgentMail. Use when the user asks to email someone (a proposal, a question, a document) or to check for / act on email replies. Replies to email you send arrive back in this thread automatically.
---

# Email via AgentMail

This bb install has an AgentMail plugin: one shared email inbox that any thread
can send from. The plugin remembers which bb thread sent each email thread, and
when a reply arrives (polled every few minutes) it is delivered back into that
bb thread as a new message. You do not need to poll for replies — they show up
on their own.

The inbox is the AI assistant's address, not the user's. Outgoing email
normally pauses in the bb thread for the user to review, edit, and approve
before anything is sent, so the user has final say over the text. Compose the
draft the way the user asked; if they gave no direction on voice, write as
their assistant and let them adjust it in review.

## Sending

Use the `agentmail_send` tool: `to` (required list of addresses) and `body`
(required plain text), plus optional `cc`, `subject`, `html` (an HTML
alternative body), and `attachments` (absolute paths on the machine this
thread's environment runs on).

- The tool call waits while the user reviews the draft in a form that
  replaces the thread's composer (unless review is disabled in the plugin
  settings). It is a native tool call, not a shell command — there is no
  timeout to configure; it simply waits, up to bb's one-hour cap on the
  review form.
- The email is NOT sent until the tool result says so. Never tell the user an
  email was sent before then. If the result says the review was cancelled or
  timed out, nothing was sent; tell the user and do not retry unless they ask.
- When the user edited the draft before approving, the result includes the
  final version that was actually sent — treat that, not your draft, as what
  the recipient received.
- The result includes the AgentMail thread id — mention it when telling the
  user the email was sent, so follow-ups are easy to correlate.

## Replying within an existing email thread

When an email reply is delivered to this thread it names its AgentMail thread
id. To respond by email, use the `agentmail_reply` tool: `thread` (the
AgentMail thread id) and `body`, plus optional `html` and `attachments`.
Replies wait for user review exactly the way `agentmail_send` does.

## Inspecting

```
bb agentmail threads            # email threads owned by this bb thread
bb agentmail read --thread ID   # full messages of one email thread
bb agentmail attachment --message MESSAGE_ID --attachment ATTACHMENT_ID --out /abs/path
```

Delivered reply messages list their attachments with the exact `attachment`
command to download each one.

## If the tools are missing

The `agentmail_send` / `agentmail_reply` tools appear in sessions started
after the plugin was installed. If this session predates that, the CLI
equivalents behave identically but run under your shell tool:

```
bb agentmail send --to a@b.com [--to ...] [--cc ...] --subject S --body TEXT [--html HTML] [--attach /abs/path]...
bb agentmail reply --thread AGENTMAIL_THREAD_ID --body TEXT [--html HTML] [--attach /abs/path]...
```

Run them with a generous tool timeout (10 minutes) since they block on the
user's review; if the command times out, nothing was sent.
