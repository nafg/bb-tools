# bb-plugin-agentmail

A [bb](https://github.com/get-bb/bb) plugin that gives agent threads email via
[AgentMail](https://agentmail.to): any thread can send email with
`bb agentmail send`, the plugin records which bb thread owns which email
thread, and when a reply arrives it is delivered back into that thread. Inbound
email with no known thread spawns a new thread in the personal project.

Written in Scala.js: bb loads the linked ES module (`server.js`) directly as
the plugin backend — no TypeScript shim. `facades.scala` declares the small
slice of the bb plugin API the plugin uses; regenerate the authoritative
declarations with `bb plugin types` (`types/bb-plugin-sdk.d.ts`, gitignored).

## Build and install

Requires [bleep](https://bleep.build) (only for building; the built plugin is
plain JavaScript).

```sh
npm run build          # bleep link + copy the linked JS to ./server.js
bb plugin install .    # register this directory as a path plugin
```

Then set the **AgentMail API key** and **inbox address** in bb Settings →
Plugins → AgentMail and reload the plugin. Mail arriving from configuration
time onward is processed; older mail is ignored.

After changing Scala sources: `npm run build && bb plugin reload agentmail`.

## Commands

```
bb agentmail send --to a@b.com [--to ...] [--cc ...] --subject S --body TEXT [--html HTML] [--attach /abs/path]...
bb agentmail reply --thread AGENTMAIL_THREAD_ID --body TEXT [--html HTML] [--attach /abs/path]...
bb agentmail threads [--all]
bb agentmail read --thread AGENTMAIL_THREAD_ID
bb agentmail attachment --message MESSAGE_ID --attachment ATTACHMENT_ID --out /abs/path
bb agentmail poll
```

Attachment paths are read from (and downloads written to) the machine the
invoking thread's environment runs on, via bb's files API.

## How replies come back

A background schedule polls the inbox (interval configurable in settings,
default 5 minutes). New inbound messages are matched by AgentMail thread id to
the bb thread that started the email thread and injected there as a message;
if that bb thread is archived it is unarchived first. Delivery is idempotent
(delivered message ids are recorded in the plugin's SQLite database).
