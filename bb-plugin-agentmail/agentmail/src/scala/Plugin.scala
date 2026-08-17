package agentmail

import bbplugin.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

private def str(value: js.Dynamic): String =
  value.asInstanceOf[js.UndefOr[Any]].fold("")(v => if v == null then "" else v.toString)

private def toBase64(content: String, encoding: String): String =
  if encoding == "base64" then content
  else js.Dynamic.global.Buffer.from(content, "utf8").applyDynamic("toString")("base64").asInstanceOf[String]

private def basename(path: String): String = path.split('/').last

@JSExportTopLevel("default")
def plugin(bb: BbApi): js.Promise[Unit] =
  val db = bb.storage.database()
  bb.storage.migrate(
    db,
    js.Array(
      """CREATE TABLE IF NOT EXISTS email_threads (
        |  agentmail_thread_id TEXT PRIMARY KEY,
        |  bb_thread_id TEXT NOT NULL,
        |  last_message_id TEXT,
        |  subject TEXT,
        |  counterparty TEXT,
        |  created_at INTEGER NOT NULL DEFAULT (unixepoch())
        |)""".stripMargin,
      """CREATE TABLE IF NOT EXISTS delivered_messages (
        |  message_id TEXT PRIMARY KEY,
        |  delivered_at INTEGER NOT NULL DEFAULT (unixepoch())
        |)""".stripMargin,
      "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
    )
  )

  val settings = bb.settings.define(
    js.Dynamic.literal(
      "apiKey" -> js.Dynamic.literal(
        "type"   -> "string",
        "label"  -> "AgentMail API key",
        "secret" -> true
      ),
      "inbox" -> js.Dynamic.literal(
        "type"    -> "string",
        "label"   -> "Inbox address (e.g. you@agentmail.to)",
        "default" -> ""
      ),
      "reviewBeforeSend" -> js.Dynamic.literal(
        "type"    -> "boolean",
        "label"   -> "Review outgoing email in the thread before it is sent",
        "default" -> true
      ),
      "pollMinutes" -> js.Dynamic.literal(
        "type"    -> "select",
        "label"   -> "Poll interval in minutes (reload plugin after changing)",
        "options" -> js.Array("1", "2", "5", "10", "15"),
        "default" -> "5"
      )
    )
  )

  Cli.register(bb, settings, db)

  settings.get().toFuture.map { s =>
    if str(s.apiKey).isEmpty || str(s.inbox).isEmpty then
      bb.status.needsConfiguration(
        "Set the AgentMail API key and inbox address in the plugin settings, then reload the plugin."
      )
    else
      // Mail arriving from configuration time onward is delivered; older mail is
      // ignored so configuring against a full inbox doesn't flood bb with threads.
      Poller.ensureCursor(db)
    val minutes = str(s.pollMinutes).toIntOption.filter(_ > 0).getOrElse(5)
    val cron    = if minutes == 1 then "* * * * *" else s"*/$minutes * * * *"
    bb.background.schedule("poll", cron, () => Poller.poll(bb, settings, db).toJSPromise)
  }.toJSPromise

private object Mapping:
  def lookup(db: SqliteDb, agentmailThreadId: String): Option[js.Dynamic] =
    db.prepare("SELECT * FROM email_threads WHERE agentmail_thread_id = ?")
      .get(agentmailThreadId)
      .toOption

  def record(
      db: SqliteDb,
      agentmailThreadId: String,
      bbThreadId: String,
      lastMessageId: String,
      subject: String,
      counterparty: String
  ): Unit =
    val _ = db
      .prepare(
        """INSERT INTO email_threads (agentmail_thread_id, bb_thread_id, last_message_id, subject, counterparty)
          |VALUES (?, ?, ?, ?, ?)
          |ON CONFLICT (agentmail_thread_id)
          |DO UPDATE SET last_message_id = excluded.last_message_id""".stripMargin
      )
      .run(agentmailThreadId, bbThreadId, lastMessageId, subject, counterparty)

private object Review:
  enum Outcome:
    case Skipped
    case Approved(value: js.Dynamic)
    case Cancelled(reason: String)

  /** Replaces the thread's composer with the plugin's review form and waits for
    * the user; Skipped when review is disabled or there is no invoking thread.
    */
  def request(
      bb: BbApi,
      settings: BbSettingsHandle,
      ctx: js.Dynamic,
      title: String,
      payload: js.Dynamic
  )(using ExecutionContext): Future[Outcome] =
    settings.get().toFuture.flatMap { s =>
      val enabled  = s.reviewBeforeSend.asInstanceOf[js.UndefOr[Boolean]].getOrElse(true)
      val bbThread = ctx.threadId.asInstanceOf[js.UndefOr[String]]
      bbThread.toOption.filter(_ => enabled) match
        case None => Future.successful(Outcome.Skipped)
        case Some(tid) =>
          val request = js.Dynamic.literal(
            "threadId"   -> tid,
            "rendererId" -> "email-review",
            "title"      -> title,
            "payload"    -> payload
          )
          val options = js.Dynamic.literal()
          ctx.signal.asInstanceOf[js.UndefOr[js.Any]].foreach(sig => options.updateDynamic("signal")(sig))
          bb.ui.requestInput(request, options).toFuture.map { result =>
            str(result.outcome) match
              case "submitted" => Outcome.Approved(result.value.asInstanceOf[js.Dynamic])
              case _           => Outcome.Cancelled(str(result.reason))
          }
    }

  def cancelledMessage(reason: String): String =
    reason match
      case "user"    => "the user dismissed the review form without sending"
      case "timeout" => "the review form timed out before the user acted"
      case other     => s"the review was cancelled ($other)"

object Cli:
  private def ok(stdout: String): js.Dynamic =
    js.Dynamic.literal("exitCode" -> 0, "stdout" -> stdout)
  private def err(message: String): js.Dynamic =
    js.Dynamic.literal("exitCode" -> 1, "stderr" -> message)

  /** `--flag value` pairs; repeated flags accumulate. Returns flag -> values plus positionals. */
  private def parseArgs(argv: List[String]): (Map[String, List[String]], List[String]) =
    @annotation.tailrec
    def loop(
        rest: List[String],
        flags: Map[String, List[String]],
        positional: List[String]
    ): (Map[String, List[String]], List[String]) =
      rest match
        case flag :: value :: tail if flag.startsWith("--") && !value.startsWith("--") =>
          val name = flag.drop(2)
          loop(tail, flags.updated(name, flags.getOrElse(name, Nil) :+ value), positional)
        case flag :: tail if flag.startsWith("--") =>
          // Bare flag (followed by another flag or nothing) is boolean true.
          val name = flag.drop(2)
          loop(tail, flags.updated(name, flags.getOrElse(name, Nil) :+ "true"), positional)
        case value :: tail => loop(tail, flags, positional :+ value)
        case Nil           => (flags, positional)
    loop(argv, Map.empty, Nil)

  private def clientFromSettings(settings: BbSettingsHandle)(using ExecutionContext): Future[AgentMailClient] =
    settings.get().toFuture.flatMap { s =>
      val apiKey = str(s.apiKey)
      val inbox  = str(s.inbox)
      if apiKey.isEmpty || inbox.isEmpty then
        Future.failed(AgentMailException("plugin not configured: set apiKey and inbox in the AgentMail plugin settings"))
      else Future.successful(AgentMailClient(apiKey, inbox))
    }

  /** The host the invoking thread's environment lives on; undefined = the server's own host. */
  private def invokingHostId(bb: BbApi, ctx: js.Dynamic)(using ExecutionContext): Future[js.UndefOr[String]] =
    ctx.threadId.asInstanceOf[js.UndefOr[String]].fold(Future.successful(js.undefined: js.UndefOr[String])) { tid =>
      bb.sdk.threads.get(js.Dynamic.literal("threadId" -> tid)).toFuture.flatMap { thread =>
        val envId = thread.environmentId.asInstanceOf[String | Null]
        if envId == null then Future.successful(js.undefined)
        else
          bb.sdk.environments
            .get(js.Dynamic.literal("environmentId" -> envId))
            .toFuture
            .map(env => env.hostId.asInstanceOf[String]: js.UndefOr[String])
      }
    }

  private def readAttachments(bb: BbApi, hostId: js.UndefOr[String], paths: List[String])(using
      ExecutionContext
  ): Future[js.Array[js.Any]] =
    paths.foldLeft(Future.successful(js.Array[js.Any]())) { (acc, path) =>
      acc.flatMap { collected =>
        val args = js.Dynamic.literal("path" -> path)
        hostId.foreach(h => args.updateDynamic("hostId")(h))
        bb.sdk.files.read(args).toFuture.map { file =>
          val attachment = js.Dynamic.literal(
            "filename" -> basename(path),
            "content"  -> toBase64(file.content.asInstanceOf[String], str(file.contentEncoding))
          )
          file.mimeType.asInstanceOf[js.UndefOr[String]].foreach(m => attachment.updateDynamic("content_type")(m))
          val _ = collected.push(attachment)
          collected
        }
      }
    }

  private def outboundPayload(body: String, html: Option[String], attachments: js.Array[js.Any]): js.Dynamic =
    val payload = js.Dynamic.literal("text" -> body)
    html.foreach(h => payload.updateDynamic("html")(h))
    if attachments.nonEmpty then payload.updateDynamic("attachments")(attachments)
    payload

  private def strings(value: js.Dynamic, fallback: List[String]): List[String] =
    value.asInstanceOf[js.UndefOr[js.Array[String]]].fold(fallback)(_.toList)

  def register(bb: BbApi, settings: BbSettingsHandle, db: SqliteDb): Unit =
    bb.cli.register(
      js.Dynamic.literal(
        "name"    -> "agentmail",
        "summary" -> "Send and receive email via AgentMail; replies come back to the sending thread",
        "commands" -> js.Array(
          js.Dynamic.literal(
            "name"    -> "send",
            "summary" -> "Send an email from this thread; replies will be delivered back here",
            "usage"   -> "bb agentmail send --to a@b.com [--to ...] [--cc ...] --subject S --body TEXT [--html HTML] [--attach /abs/path]..."
          ),
          js.Dynamic.literal(
            "name"    -> "reply",
            "summary" -> "Reply within an existing email thread",
            "usage"   -> "bb agentmail reply --thread AGENTMAIL_THREAD_ID --body TEXT [--html HTML] [--attach /abs/path]..."
          ),
          js.Dynamic.literal(
            "name"    -> "threads",
            "summary" -> "List email threads owned by this bb thread (all with --all)",
            "usage"   -> "bb agentmail threads [--all]"
          ),
          js.Dynamic.literal(
            "name"    -> "read",
            "summary" -> "Show the messages of an email thread",
            "usage"   -> "bb agentmail read --thread AGENTMAIL_THREAD_ID"
          ),
          js.Dynamic.literal(
            "name"    -> "attachment",
            "summary" -> "Download an attachment from a received email",
            "usage"   -> "bb agentmail attachment --message MESSAGE_ID --attachment ATTACHMENT_ID --out /abs/path"
          ),
          js.Dynamic.literal(
            "name"    -> "poll",
            "summary" -> "Check for new inbound email now instead of waiting for the schedule",
            "usage"   -> "bb agentmail poll"
          )
        ),
        "run" -> ((argv: js.Array[String], ctx: js.Dynamic) => run(bb, settings, db, argv.toList, ctx).toJSPromise)
      )
    )

  private def run(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      argv: List[String],
      ctx: js.Dynamic
  ): Future[js.Dynamic] =
    val result = argv match
      case "send" :: rest      => send(bb, settings, db, rest, ctx)
      case "reply" :: rest     => reply(bb, settings, db, rest, ctx)
      case "poll" :: _         => Poller.poll(bb, settings, db).map(_ => ok("poll completed (see bb plugin logs agentmail)"))
      case "threads" :: rest   => Future.successful(threads(db, rest, ctx))
      case "read" :: rest      => read(settings, rest)
      case "attachment" :: rest => attachment(bb, settings, rest, ctx)
      case other =>
        Future.successful(
          err(s"unknown command: ${other.mkString(" ")}\nCommands: send, reply, threads, read, attachment, poll")
        )
    result.recover { case e => err(e.getMessage) }

  private def send(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      argv: List[String],
      ctx: js.Dynamic
  ): Future[js.Dynamic] =
    val (flags, _) = parseArgs(argv)
    val to         = flags.getOrElse("to", Nil)
    if to.isEmpty then Future.successful(err("--to is required"))
    else if !flags.contains("body") then Future.successful(err("--body is required"))
    else
      val cc      = flags.getOrElse("cc", Nil)
      val subject = flags.getOrElse("subject", Nil).mkString(" ")
      val body    = flags.getOrElse("body", Nil).mkString("\n")
      val html    = flags.get("html").map(_.mkString("\n"))
      val attach  = flags.getOrElse("attach", Nil)
      val reviewPayload = js.Dynamic.literal(
        "kind"        -> "send",
        "to"          -> to.toJSArray,
        "cc"          -> cc.toJSArray,
        "subject"     -> subject,
        "body"        -> body,
        "hasHtml"     -> html.isDefined,
        "attachments" -> attach.toJSArray
      )
      Review.request(bb, settings, ctx, "Review outgoing email", reviewPayload).flatMap {
        case Review.Outcome.Cancelled(reason) =>
          Future.successful(err(s"Email not sent: ${Review.cancelledMessage(reason)}."))
        case outcome =>
          val (finalTo, finalCc, finalSubject, finalBody) = outcome match
            case Review.Outcome.Approved(value) =>
              (strings(value.to, to), strings(value.cc, cc), str(value.subject), str(value.body))
            case _ => (to, cc, subject, body)
          val edited = (finalTo, finalCc, finalSubject, finalBody) != (to, cc, subject, body)
          if finalTo.isEmpty then Future.successful(err("Email not sent: the reviewed draft has no recipients"))
          else
            // The HTML variant is not editable in the review form, so an
            // edited plain-text body invalidates it.
            val finalHtml = if finalBody == body then html else None
            for
              client      <- clientFromSettings(settings)
              hostId      <- invokingHostId(bb, ctx)
              attachments <- readAttachments(bb, hostId, attach)
              payload = outboundPayload(finalBody, finalHtml, attachments)
              _       = payload.updateDynamic("to")(finalTo.toJSArray)
              _       = if finalCc.nonEmpty then payload.updateDynamic("cc")(finalCc.toJSArray)
              _       = if finalSubject.nonEmpty then payload.updateDynamic("subject")(finalSubject)
              sent <- client.sendMessage(payload)
            yield
              val threadId  = sent.thread_id.asInstanceOf[String]
              val messageId = sent.message_id.asInstanceOf[String]
              val bbThread  = ctx.threadId.asInstanceOf[js.UndefOr[String]]
              bbThread.foreach { tid =>
                Mapping.record(db, threadId, tid, messageId, finalSubject, finalTo.head)
              }
              val note =
                if bbThread.isDefined then "Replies will be delivered back to this thread."
                else "Not invoked from a bb thread: replies will spawn a new thread."
              val editNote =
                if edited then
                  s"\nThe user edited the draft before sending; final version:\nTo: ${finalTo.mkString(", ")}\nSubject: $finalSubject\n\n$finalBody"
                else ""
              ok(s"Sent. AgentMail thread: $threadId, message: $messageId. $note$editNote")
      }

  private def reply(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      argv: List[String],
      ctx: js.Dynamic
  ): Future[js.Dynamic] =
    val (flags, _) = parseArgs(argv)
    (flags.get("thread").flatMap(_.headOption), flags.contains("body")) match
      case (None, _) => Future.successful(err("--thread is required (see `bb agentmail threads`)"))
      case (_, false) => Future.successful(err("--body is required"))
      case (Some(threadId), _) =>
        Mapping.lookup(db, threadId) match
          case None => Future.successful(err(s"unknown email thread: $threadId (see `bb agentmail threads`)"))
          case Some(mapping) =>
            val lastMessageId = str(mapping.last_message_id)
            if lastMessageId.isEmpty then Future.successful(err(s"no message to reply to in thread $threadId"))
            else
              val body   = flags.getOrElse("body", Nil).mkString("\n")
              val html   = flags.get("html").map(_.mkString("\n"))
              val attach = flags.getOrElse("attach", Nil)
              val reviewPayload = js.Dynamic.literal(
                "kind"        -> "reply",
                "to"          -> js.Array(str(mapping.counterparty)),
                "subject"     -> str(mapping.subject),
                "body"        -> body,
                "hasHtml"     -> html.isDefined,
                "attachments" -> attach.toJSArray
              )
              Review.request(bb, settings, ctx, "Review outgoing email reply", reviewPayload).flatMap {
                case Review.Outcome.Cancelled(reason) =>
                  Future.successful(err(s"Reply not sent: ${Review.cancelledMessage(reason)}."))
                case outcome =>
                  val finalBody = outcome match
                    case Review.Outcome.Approved(value) => str(value.body)
                    case _                              => body
                  val finalHtml = if finalBody == body then html else None
                  for
                    client      <- clientFromSettings(settings)
                    hostId      <- invokingHostId(bb, ctx)
                    attachments <- readAttachments(bb, hostId, attach)
                    payload = outboundPayload(finalBody, finalHtml, attachments)
                    sent <- client.replyToMessage(lastMessageId, payload)
                  yield
                    val editNote =
                      if finalBody != body then
                        s"\nThe user edited the draft before sending; final version:\n$finalBody"
                      else ""
                    ok(s"Replied. message: ${sent.message_id.asInstanceOf[String]}$editNote")
              }

  private def threads(db: SqliteDb, argv: List[String], ctx: js.Dynamic): js.Dynamic =
    val (flags, _) = parseArgs(argv)
    val all        = flags.get("all").exists(_.lastOption.exists(_ != "false"))
    val bbThread   = ctx.threadId.asInstanceOf[js.UndefOr[String]]
    val rows =
      if all || bbThread.isEmpty then db.prepare("SELECT * FROM email_threads ORDER BY created_at DESC").all()
      else
        db.prepare("SELECT * FROM email_threads WHERE bb_thread_id = ? ORDER BY created_at DESC")
          .all(bbThread.get)
    if rows.isEmpty then ok("No email threads.")
    else
      val lines = rows.map { row =>
        s"${str(row.agentmail_thread_id)}  subject=${str(row.subject)}  with=${str(row.counterparty)}  bbThread=${str(row.bb_thread_id)}"
      }
      ok(lines.mkString("\n"))

  private def read(settings: BbSettingsHandle, argv: List[String]): Future[js.Dynamic] =
    val (flags, _) = parseArgs(argv)
    flags.get("thread").flatMap(_.headOption) match
      case None => Future.successful(err("--thread is required"))
      case Some(threadId) =>
        clientFromSettings(settings).flatMap { client =>
          client.getThread(threadId).map { thread =>
            val messages = thread.messages.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]]
            messages.fold(ok(js.JSON.stringify(thread.asInstanceOf[js.Object], null, 2))) { msgs =>
              val rendered = msgs.map { m =>
                val attachments = m.attachments
                  .asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]]
                  .map { list =>
                    if list.isEmpty then ""
                    else
                      list
                        .map(a => s"  [attachment] ${str(a.filename)} (id: ${str(a.attachment_id)})")
                        .mkString("\n", "\n", "")
                  }
                  .getOrElse("")
                val body = Seq(str(m.text), str(m.preview)).find(_.nonEmpty).getOrElse("(no text)")
                s"--- ${str(m.timestamp)}  from ${str(m.selectDynamic("from"))}  (message ${str(m.message_id)})\n$body$attachments"
              }
              ok(rendered.mkString("\n\n"))
            }
          }
        }

  private def attachment(
      bb: BbApi,
      settings: BbSettingsHandle,
      argv: List[String],
      ctx: js.Dynamic
  ): Future[js.Dynamic] =
    val (flags, _) = parseArgs(argv)
    (
      flags.get("message").flatMap(_.headOption),
      flags.get("attachment").flatMap(_.headOption),
      flags.get("out").flatMap(_.headOption)
    ) match
      case (Some(messageId), Some(attachmentId), Some(out)) =>
        for
          client          <- clientFromSettings(settings)
          hostId          <- invokingHostId(bb, ctx)
          (meta, base64)  <- client.downloadAttachment(messageId, attachmentId)
          writeArgs = js.Dynamic.literal(
            "path"            -> out,
            "content"         -> base64,
            "contentEncoding" -> "base64",
            "createParents"   -> true
          )
          _ = hostId.foreach(h => writeArgs.updateDynamic("hostId")(h))
          written <- bb.sdk.files.write(writeArgs).toFuture
        yield ok(s"Wrote ${str(written.sizeBytes)} bytes to $out (${str(meta.filename)})")
      case _ => Future.successful(err("--message, --attachment, and --out are all required"))

object Poller:
  private def metaGet(db: SqliteDb, key: String): Option[String] =
    db.prepare("SELECT value FROM meta WHERE key = ?").get(key).toOption.map(row => str(row.value))

  private def metaSet(db: SqliteDb, key: String, value: String): Unit =
    val _ = db
      .prepare("INSERT INTO meta (key, value) VALUES (?, ?) ON CONFLICT (key) DO UPDATE SET value = excluded.value")
      .run(key, value)

  def ensureCursor(db: SqliteDb): Unit =
    if metaGet(db, "cursor").isEmpty then metaSet(db, "cursor", new js.Date().toISOString())

  private def isDelivered(db: SqliteDb, messageId: String): Boolean =
    db.prepare("SELECT 1 FROM delivered_messages WHERE message_id = ?").get(messageId).isDefined

  private def markDelivered(db: SqliteDb, messageId: String): Unit =
    val _ = db.prepare("INSERT OR IGNORE INTO delivered_messages (message_id) VALUES (?)").run(messageId)

  private def formatInbound(full: js.Dynamic, agentmailThreadId: String): String =
    val from    = str(full.selectDynamic("from"))
    val subject = str(full.subject)
    val body    = Seq(str(full.text), str(full.preview)).find(_.nonEmpty).getOrElse("(no text body)")
    val attachments = full.attachments
      .asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]]
      .map { list =>
        if list.isEmpty then ""
        else
          list
            .map(a =>
              s"- ${str(a.filename)}: bb agentmail attachment --message ${str(full.message_id)} --attachment ${str(a.attachment_id)} --out /path/to/save"
            )
            .mkString("\nAttachments:\n", "\n", "")
      }
      .getOrElse("")
    s"""Email received via AgentMail.
       |From: $from
       |Subject: $subject
       |
       |$body$attachments
       |
       |To reply by email: bb agentmail reply --thread $agentmailThreadId --body "..."""".stripMargin

  private def deliverToThread(bb: BbApi, bbThreadId: String, text: String)(using ExecutionContext): Future[Unit] =
    bb.sdk.threads.get(js.Dynamic.literal("threadId" -> bbThreadId)).toFuture.flatMap { thread =>
      val archived = thread.archivedAt.asInstanceOf[Any] != null
      val unarchive =
        if archived then bb.sdk.threads.unarchive(js.Dynamic.literal("threadId" -> bbThreadId)).toFuture.map(_ => ())
        else Future.unit
      unarchive.flatMap { _ =>
        bb.sdk.threads
          .send(
            js.Dynamic.literal(
              "threadId" -> bbThreadId,
              "mode"     -> "auto",
              "input"    -> js.Array(js.Dynamic.literal("type" -> "text", "text" -> text))
            )
          )
          .toFuture
          .map(_ => ())
      }
    }

  private def spawnForUnmatched(bb: BbApi, db: SqliteDb, full: js.Dynamic, agentmailThreadId: String, text: String)(
      using ExecutionContext
  ): Future[Unit] =
    bb.sdk.projects.list(js.Dynamic.literal("includePersonal" -> true)).toFuture.flatMap { projects =>
      projects.find(p => str(p.kind) == "personal") match
        case None => Future.failed(AgentMailException("no personal project found for unmatched inbound email"))
        case Some(personal) =>
          val subject = str(full.subject)
          val title   = if subject.nonEmpty then s"Email: $subject" else s"Email from ${str(full.selectDynamic("from"))}"
          bb.sdk.threads
            .spawn(
              js.Dynamic.literal(
                "projectId"   -> str(personal.id),
                "environment" -> js.Dynamic.literal("type" -> "project-default"),
                "prompt"      -> text,
                "title"       -> title
              )
            )
            .toFuture
            .map { spawned =>
              Mapping.record(
                db,
                agentmailThreadId,
                str(spawned.id),
                str(full.message_id),
                subject,
                str(full.selectDynamic("from"))
              )
            }
    }

  private def processMessage(bb: BbApi, db: SqliteDb, client: AgentMailClient, inbox: String, item: js.Dynamic)(using
      ExecutionContext
  ): Future[Unit] =
    val messageId = str(item.message_id)
    val labels    = item.labels.asInstanceOf[js.UndefOr[js.Array[String]]].map(_.toList).getOrElse(Nil)
    val from      = str(item.selectDynamic("from"))
    if isDelivered(db, messageId) then Future.unit
    else if labels.contains("sent") || from.contains(inbox) then
      markDelivered(db, messageId)
      Future.unit
    else
      client.getMessage(messageId).flatMap { full =>
        val agentmailThreadId = str(full.thread_id)
        val text              = formatInbound(full, agentmailThreadId)
        val deliver = Mapping.lookup(db, agentmailThreadId) match
          case Some(mapping) =>
            deliverToThread(bb, str(mapping.bb_thread_id), text).map { _ =>
              Mapping.record(
                db,
                agentmailThreadId,
                str(mapping.bb_thread_id),
                messageId,
                str(mapping.subject),
                from
              )
            }
          case None => spawnForUnmatched(bb, db, full, agentmailThreadId, text)
        deliver.map { _ =>
          markDelivered(db, messageId)
          bb.log.info(s"delivered email message $messageId (thread $agentmailThreadId)")
        }
      }

  def poll(bb: BbApi, settings: BbSettingsHandle, db: SqliteDb)(using ExecutionContext): Future[Unit] =
    settings.get().toFuture.flatMap { s =>
      val apiKey = str(s.apiKey)
      val inbox  = str(s.inbox)
      if apiKey.isEmpty || inbox.isEmpty then Future.unit
      else
        val client = AgentMailClient(apiKey, inbox)
        // Normally set at load time; covers configuration changing without a reload.
        ensureCursor(db)
        metaGet(db, "cursor") match
          case None => Future.unit
          case Some(cursor) =>
            client.listMessages(limit = 100, after = Some(cursor)).flatMap { result =>
              val messages = result.messages.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].map(_.toList).getOrElse(Nil)
              bb.log.debug(s"poll: cursor=$cursor count=${str(result.count)} got=${messages.size}")
              // The cursor only advances over messages processed successfully; after the
              // first failure it stays put so the failed message is retried next poll
              // (later successes are re-listed then but deduped via delivered_messages).
              messages
                .foldLeft(Future.successful((Option.empty[String], false))) { (acc, item) =>
                  acc.flatMap { case (cursor, failed) =>
                    processMessage(bb, db, client, inbox, item)
                      .map { _ =>
                        val ts = str(item.timestamp)
                        if failed || ts.isEmpty then (cursor, failed) else (Some(ts), false)
                      }
                      .recover { case e =>
                        bb.log.warn(
                          s"failed to process message ${str(item.message_id)}: ${e.getMessage} (will retry next poll)"
                        )
                        (cursor, true)
                      }
                  }
                }
                .map { case (cursor, _) => cursor.foreach(metaSet(db, "cursor", _)) }
            }
    }
