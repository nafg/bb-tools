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

## Sending

```
bb agentmail send --to alice@example.com --subject "Project proposal" --body "Hi Alice, ..." \
  [--to bob@example.com] [--cc carol@example.com] [--html "<p>...</p>"] [--attach /abs/path/file.pdf]...
```

- `--body` is the plain-text body (required). `--html` optionally adds an HTML body.
- `--attach` may be repeated; paths are read from the machine this thread's
  environment runs on and must be absolute.
- The output includes the AgentMail thread id — mention it when telling the user
  the email was sent, so follow-ups are easy to correlate.

## Replying within an existing email thread

When an email reply is delivered to this thread it names its AgentMail thread id.
To respond by email:

```
bb agentmail reply --thread <AGENTMAIL_THREAD_ID> --body "..." [--attach /abs/path]...
```

## Inspecting

```
bb agentmail threads            # email threads owned by this bb thread
bb agentmail read --thread ID   # full messages of one email thread
bb agentmail attachment --message MESSAGE_ID --attachment ATTACHMENT_ID --out /abs/path
```

Delivered reply messages list their attachments with the exact `attachment`
command to download each one.
