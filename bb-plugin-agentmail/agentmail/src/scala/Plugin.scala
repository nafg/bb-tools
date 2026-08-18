package agentmail

import bbplugin.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

private def str(value: js.Dynamic): String =
  value.asInstanceOf[js.UndefOr[Any]].fold("")(v => if v == null then "" else v.toString)

private def strArray(value: js.Dynamic): List[String] =
  value.asInstanceOf[js.UndefOr[js.Array[String]]].fold(Nil)(_.toList)

private def toBase64(content: String, encoding: String): String =
  if encoding == "base64" then content
  else js.Dynamic.global.Buffer.from(content, "utf8").applyDynamic("toString")("base64").asInstanceOf[String]

private def basename(path: String): String = path.split('/').last

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

/** All open panels and header icons listen on this channel; any change to a
  * thread's email state (draft filed/updated/sent/discarded, mail delivered)
  * publishes { type: "changed", threadId } so they refetch.
  */
private def publishChanged(bb: BbApi, bbThreadId: String): Unit =
  bb.realtime.publish("agentmail", js.Dynamic.literal("type" -> "changed", "threadId" -> bbThreadId))

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
      "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)",
      """CREATE TABLE IF NOT EXISTS drafts (
        |  draft_id TEXT PRIMARY KEY,
        |  bb_thread_id TEXT NOT NULL,
        |  created_at INTEGER NOT NULL DEFAULT (unixepoch())
        |)""".stripMargin
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
      "pollMinutes" -> js.Dynamic.literal(
        "type"    -> "select",
        "label"   -> "Poll interval in minutes (reload plugin after changing)",
        "options" -> js.Array("1", "2", "5", "10", "15"),
        "default" -> "5"
      )
    )
  )

  Cli.register(bb, settings, db)
  Tools.register(bb, settings, db)
  Rpc.register(bb, settings, db)

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

private object Drafts:
  def lookup(db: SqliteDb, draftId: String): Option[js.Dynamic] =
    db.prepare("SELECT * FROM drafts WHERE draft_id = ?").get(draftId).toOption

  def rowsFor(db: SqliteDb, bbThreadId: String): List[js.Dynamic] =
    db.prepare("SELECT * FROM drafts WHERE bb_thread_id = ? ORDER BY created_at").all(bbThreadId).toList

  def all(db: SqliteDb): List[js.Dynamic] =
    db.prepare("SELECT * FROM drafts ORDER BY created_at").all().toList

  def record(db: SqliteDb, draftId: String, bbThreadId: String): Unit =
    val _ = db.prepare("INSERT OR IGNORE INTO drafts (draft_id, bb_thread_id) VALUES (?, ?)").run(draftId, bbThreadId)

  def remove(db: SqliteDb, draftId: String): Unit =
    val _ = db.prepare("DELETE FROM drafts WHERE draft_id = ?").run(draftId)

/** Draft-filing flows shared by the native tools and the CLI. Nothing here
  * sends email: tools and CLI can only create and revise AgentMail drafts,
  * and only the user's Send button in the Email panel (the sendDraft rpc)
  * actually sends. Left is an error explanation; Right is a report for the
  * agent.
  */
private object Outbound:
  private def draftBase(body: String, html: Option[String], attachments: js.Array[js.Any]): js.Dynamic =
    val payload = js.Dynamic.literal("text" -> body)
    html.foreach(h => payload.updateDynamic("html")(h))
    if attachments.nonEmpty then payload.updateDynamic("attachments")(attachments)
    payload

  private def filedReport(draftId: String, inThread: Boolean): String =
    if inThread then
      s"""Filed email draft $draftId for the user's review. NOT SENT — only the user can send it, from this thread's Email panel; never tell the user it was sent.
         |To revise it, call agentmail_update_draft with draft $draftId.
         |To give the user a one-click way to open the review, include this directive on its own line in your reply: ::agentmail{draft="$draftId"}
         |You will receive a message in this thread if the user sends the draft.""".stripMargin
    else
      s"Filed email draft $draftId. NOT SENT — not invoked from a bb thread, so the draft can only be reviewed and sent from the AgentMail console."

  def fileSend(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      to: List[String],
      cc: List[String],
      subject: String,
      body: String,
      html: Option[String],
      attach: List[String],
      ctx: js.Dynamic
  )(using ExecutionContext): Future[Either[String, String]] =
    if to.isEmpty then Future.successful(Left("to must contain at least one recipient address"))
    else
      for
        client      <- clientFromSettings(settings)
        hostId      <- invokingHostId(bb, ctx)
        attachments <- readAttachments(bb, hostId, attach)
        payload = draftBase(body, html, attachments)
        _       = payload.updateDynamic("to")(to.toJSArray)
        _       = if cc.nonEmpty then payload.updateDynamic("cc")(cc.toJSArray)
        _       = if subject.nonEmpty then payload.updateDynamic("subject")(subject)
        draft <- client.createDraft(payload)
      yield
        val draftId  = str(draft.draft_id)
        val bbThread = ctx.threadId.asInstanceOf[js.UndefOr[String]]
        bbThread.foreach { tid =>
          Drafts.record(db, draftId, tid)
          publishChanged(bb, tid)
        }
        Right(filedReport(draftId, bbThread.isDefined))

  def fileReply(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      emailThreadId: String,
      body: String,
      html: Option[String],
      attach: List[String],
      ctx: js.Dynamic
  )(using ExecutionContext): Future[Either[String, String]] =
    Mapping.lookup(db, emailThreadId) match
      case None =>
        Future.successful(Left(s"unknown email thread: $emailThreadId (see `bb agentmail threads`)"))
      case Some(mapping) =>
        val lastMessageId = str(mapping.last_message_id)
        if lastMessageId.isEmpty then Future.successful(Left(s"no message to reply to in thread $emailThreadId"))
        else
          for
            client      <- clientFromSettings(settings)
            hostId      <- invokingHostId(bb, ctx)
            attachments <- readAttachments(bb, hostId, attach)
            payload = draftBase(body, html, attachments)
            _       = payload.updateDynamic("in_reply_to")(lastMessageId)
            draft <- client.createDraft(payload)
          yield
            val draftId  = str(draft.draft_id)
            val bbThread = ctx.threadId.asInstanceOf[js.UndefOr[String]]
            bbThread.foreach { tid =>
              Drafts.record(db, draftId, tid)
              publishChanged(bb, tid)
            }
            Right(filedReport(draftId, bbThread.isDefined))

  /** Revises a pending draft in place. `body` invalidates a previously supplied
    * HTML alternative unless a fresh `html` comes with it; `attach` adds files.
    */
  def update(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      draftId: String,
      to: List[String],
      cc: List[String],
      subject: Option[String],
      body: Option[String],
      html: Option[String],
      attach: List[String],
      ctx: js.Dynamic
  )(using ExecutionContext): Future[Either[String, String]] =
    Drafts.lookup(db, draftId) match
      case None => Future.successful(Left(s"unknown draft: $draftId (see `bb agentmail drafts`)"))
      case Some(row) =>
        for
          client      <- clientFromSettings(settings)
          hostId      <- invokingHostId(bb, ctx)
          attachments <- readAttachments(bb, hostId, attach)
          payload = js.Dynamic.literal()
          _       = if to.nonEmpty then payload.updateDynamic("to")(to.toJSArray)
          _       = if cc.nonEmpty then payload.updateDynamic("cc")(cc.toJSArray)
          _       = subject.foreach(s => payload.updateDynamic("subject")(s))
          _       = body.foreach(b => payload.updateDynamic("text")(b))
          _       = html match
            case Some(h)               => payload.updateDynamic("html")(h)
            case None if body.nonEmpty => payload.updateDynamic("html")(null)
            case None                  => ()
          _ = if attachments.nonEmpty then payload.updateDynamic("add_attachments")(attachments)
          _ <- client.updateDraft(draftId, payload)
        yield
          publishChanged(bb, str(row.bb_thread_id))
          Right(s"Draft $draftId updated. Still pending the user's review — NOT SENT.")

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

  def register(bb: BbApi, settings: BbSettingsHandle, db: SqliteDb): Unit =
    bb.cli.register(
      js.Dynamic.literal(
        "name"    -> "agentmail",
        "summary" -> "Email via AgentMail: file drafts the user reviews and sends; replies come back to the sending thread",
        "commands" -> js.Array(
          js.Dynamic.literal(
            "name"    -> "send",
            "summary" -> "File a new-email draft for the user to review and send (nothing is sent directly)",
            "usage"   -> "bb agentmail send --to a@b.com [--to ...] [--cc ...] --subject S --body TEXT [--html HTML] [--attach /abs/path]..."
          ),
          js.Dynamic.literal(
            "name"    -> "reply",
            "summary" -> "File a reply draft within an existing email thread",
            "usage"   -> "bb agentmail reply --thread AGENTMAIL_THREAD_ID --body TEXT [--html HTML] [--attach /abs/path]..."
          ),
          js.Dynamic.literal(
            "name"    -> "update-draft",
            "summary" -> "Revise a pending draft in place",
            "usage"   -> "bb agentmail update-draft --draft DRAFT_ID [--to ...] [--cc ...] [--subject S] [--body TEXT] [--html HTML] [--attach /abs/path]..."
          ),
          js.Dynamic.literal(
            "name"    -> "drafts",
            "summary" -> "List pending drafts filed by this bb thread (all with --all)",
            "usage"   -> "bb agentmail drafts [--all]"
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
      case "send" :: rest         => send(bb, settings, db, rest, ctx)
      case "reply" :: rest        => reply(bb, settings, db, rest, ctx)
      case "update-draft" :: rest => updateDraft(bb, settings, db, rest, ctx)
      case "drafts" :: rest       => Future.successful(drafts(db, rest, ctx))
      case "poll" :: _            => Poller.poll(bb, settings, db).map(_ => ok("poll completed (see bb plugin logs agentmail)"))
      case "threads" :: rest      => Future.successful(threads(db, rest, ctx))
      case "read" :: rest         => read(settings, rest)
      case "attachment" :: rest   => attachment(bb, settings, rest, ctx)
      case other =>
        Future.successful(
          err(
            s"unknown command: ${other.mkString(" ")}\nCommands: send, reply, update-draft, drafts, threads, read, attachment, poll"
          )
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
      Outbound
        .fileSend(
          bb,
          settings,
          db,
          to = to,
          cc = flags.getOrElse("cc", Nil),
          subject = flags.getOrElse("subject", Nil).mkString(" "),
          body = flags.getOrElse("body", Nil).mkString("\n"),
          html = flags.get("html").map(_.mkString("\n")),
          attach = flags.getOrElse("attach", Nil),
          ctx = ctx
        )
        .map(_.fold(err, ok))

  private def reply(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      argv: List[String],
      ctx: js.Dynamic
  ): Future[js.Dynamic] =
    val (flags, _) = parseArgs(argv)
    (flags.get("thread").flatMap(_.headOption), flags.contains("body")) match
      case (None, _)  => Future.successful(err("--thread is required (see `bb agentmail threads`)"))
      case (_, false) => Future.successful(err("--body is required"))
      case (Some(threadId), _) =>
        Outbound
          .fileReply(
            bb,
            settings,
            db,
            threadId,
            body = flags.getOrElse("body", Nil).mkString("\n"),
            html = flags.get("html").map(_.mkString("\n")),
            attach = flags.getOrElse("attach", Nil),
            ctx = ctx
          )
          .map(_.fold(err, ok))

  private def updateDraft(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      argv: List[String],
      ctx: js.Dynamic
  ): Future[js.Dynamic] =
    val (flags, _) = parseArgs(argv)
    flags.get("draft").flatMap(_.headOption) match
      case None => Future.successful(err("--draft is required (see `bb agentmail drafts`)"))
      case Some(draftId) =>
        Outbound
          .update(
            bb,
            settings,
            db,
            draftId,
            to = flags.getOrElse("to", Nil),
            cc = flags.getOrElse("cc", Nil),
            subject = flags.get("subject").map(_.mkString(" ")),
            body = flags.get("body").map(_.mkString("\n")),
            html = flags.get("html").map(_.mkString("\n")),
            attach = flags.getOrElse("attach", Nil),
            ctx = ctx
          )
          .map(_.fold(err, ok))

  private def drafts(db: SqliteDb, argv: List[String], ctx: js.Dynamic): js.Dynamic =
    val (flags, _) = parseArgs(argv)
    val all        = flags.get("all").exists(_.lastOption.exists(_ != "false"))
    val bbThread   = ctx.threadId.asInstanceOf[js.UndefOr[String]]
    val rows =
      if all || bbThread.isEmpty then Drafts.all(db)
      else Drafts.rowsFor(db, bbThread.get)
    if rows.isEmpty then ok("No pending drafts.")
    else
      val lines = rows.map(row => s"${str(row.draft_id)}  bbThread=${str(row.bb_thread_id)}")
      ok(lines.mkString("\n"))

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

/** Native agent tools. All of them only file or revise drafts — none of them
  * can send email; sending is exclusively the user's Send button in the Email
  * panel (the sendDraft rpc).
  */
object Tools:
  private val AttachmentsDescription =
    "Absolute paths of files to attach, on the machine this thread's environment runs on"

  private def textError(message: String): js.Any =
    js.Dynamic.literal(
      "content" -> js.Array(js.Dynamic.literal("type" -> "text", "text" -> message)),
      "isError" -> true
    )

  private def optionalStr(value: js.Dynamic): Option[String] =
    Some(str(value)).filter(_.nonEmpty)

  private def stringSchema(description: String): js.Dynamic =
    js.Dynamic.literal("type" -> "string", "description" -> description)

  private def stringArraySchema(description: String): js.Dynamic =
    js.Dynamic.literal(
      "type"        -> "array",
      "items"       -> js.Dynamic.literal("type" -> "string"),
      "description" -> description
    )

  def register(bb: BbApi, settings: BbSettingsHandle, db: SqliteDb): Unit =
    bb.agents.registerTool(
      js.Dynamic.literal(
        "name" -> "agentmail_send",
        "description" ->
          ("File a new-email draft via AgentMail for the user to review. This does NOT send anything: " +
            "the draft appears in this thread's Email panel, where only the user can send (or discard) it. " +
            "Returns the draft id; revise the draft with agentmail_update_draft. If the user sends it, a " +
            "message is delivered into this thread, and replies to the email arrive here too."),
        "experimental_statusLabels" -> js.Dynamic.literal(
          "pending"   -> "Filing email draft",
          "completed" -> "Filed email draft"
        ),
        "parameters" -> js.Dynamic.literal(
          "type" -> "object",
          "properties" -> js.Dynamic.literal(
            "to"          -> stringArraySchema("Recipient email addresses"),
            "cc"          -> stringArraySchema("Cc email addresses"),
            "subject"     -> stringSchema("Subject line"),
            "body"        -> stringSchema("Plain-text body"),
            "html"        -> stringSchema("Optional HTML alternative body"),
            "attachments" -> stringArraySchema(AttachmentsDescription)
          ),
          "required"             -> js.Array("to", "body"),
          "additionalProperties" -> false
        ),
        "execute" -> ((input: js.Dynamic, toolCtx: js.Dynamic) =>
          executeSend(bb, settings, db, input, toolCtx).toJSPromise
        )
      )
    )
    bb.agents.registerTool(
      js.Dynamic.literal(
        "name" -> "agentmail_reply",
        "description" ->
          ("File a reply draft within an existing AgentMail email thread owned by this bb thread. " +
            "Like agentmail_send, this does NOT send anything — the user reviews and sends the draft " +
            "from the Email panel."),
        "experimental_statusLabels" -> js.Dynamic.literal(
          "pending"   -> "Filing email reply draft",
          "completed" -> "Filed email reply draft"
        ),
        "parameters" -> js.Dynamic.literal(
          "type" -> "object",
          "properties" -> js.Dynamic.literal(
            "thread"      -> stringSchema("AgentMail thread id (from `bb agentmail threads` or a delivered reply)"),
            "body"        -> stringSchema("Plain-text body"),
            "html"        -> stringSchema("Optional HTML alternative body"),
            "attachments" -> stringArraySchema(AttachmentsDescription)
          ),
          "required"             -> js.Array("thread", "body"),
          "additionalProperties" -> false
        ),
        "execute" -> ((input: js.Dynamic, toolCtx: js.Dynamic) =>
          executeReply(bb, settings, db, input, toolCtx).toJSPromise
        )
      )
    )
    bb.agents.registerTool(
      js.Dynamic.literal(
        "name" -> "agentmail_update_draft",
        "description" ->
          ("Revise a pending email draft in place (e.g. after the user asks for changes). Only the " +
            "supplied fields change; a new body replaces any earlier HTML alternative unless fresh html " +
            "is supplied too; attachments are added, not replaced. The draft stays pending until the " +
            "user sends it from the Email panel."),
        "experimental_statusLabels" -> js.Dynamic.literal(
          "pending"   -> "Updating email draft",
          "completed" -> "Updated email draft"
        ),
        "parameters" -> js.Dynamic.literal(
          "type" -> "object",
          "properties" -> js.Dynamic.literal(
            "draft"       -> stringSchema("Draft id returned by agentmail_send / agentmail_reply"),
            "to"          -> stringArraySchema("Replacement recipient addresses"),
            "cc"          -> stringArraySchema("Replacement Cc addresses"),
            "subject"     -> stringSchema("Replacement subject line"),
            "body"        -> stringSchema("Replacement plain-text body"),
            "html"        -> stringSchema("Replacement HTML alternative body"),
            "attachments" -> stringArraySchema(AttachmentsDescription + " (added to the draft)")
          ),
          "required"             -> js.Array("draft"),
          "additionalProperties" -> false
        ),
        "execute" -> ((input: js.Dynamic, toolCtx: js.Dynamic) =>
          executeUpdate(bb, settings, db, input, toolCtx).toJSPromise
        )
      )
    )

  private def executeSend(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      input: js.Dynamic,
      toolCtx: js.Dynamic
  ): Future[js.Any] =
    val to   = strArray(input.to)
    val body = str(input.body)
    if to.isEmpty then Future.successful(textError("to must contain at least one recipient address"))
    else if body.isEmpty then Future.successful(textError("body is required"))
    else
      Outbound
        .fileSend(
          bb,
          settings,
          db,
          to = to,
          cc = strArray(input.cc),
          subject = str(input.subject),
          body = body,
          html = optionalStr(input.html),
          attach = strArray(input.attachments),
          ctx = toolCtx
        )
        .map(_.fold(textError, message => message: js.Any))
        .recover { case e => textError(e.getMessage) }

  private def executeReply(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      input: js.Dynamic,
      toolCtx: js.Dynamic
  ): Future[js.Any] =
    val emailThreadId = str(input.thread)
    val body          = str(input.body)
    if emailThreadId.isEmpty then Future.successful(textError("thread is required"))
    else if body.isEmpty then Future.successful(textError("body is required"))
    else
      Outbound
        .fileReply(
          bb,
          settings,
          db,
          emailThreadId,
          body = body,
          html = optionalStr(input.html),
          attach = strArray(input.attachments),
          ctx = toolCtx
        )
        .map(_.fold(textError, message => message: js.Any))
        .recover { case e => textError(e.getMessage) }

  private def executeUpdate(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      input: js.Dynamic,
      toolCtx: js.Dynamic
  ): Future[js.Any] =
    val draftId = str(input.draft)
    if draftId.isEmpty then Future.successful(textError("draft is required"))
    else
      Outbound
        .update(
          bb,
          settings,
          db,
          draftId,
          to = strArray(input.to),
          cc = strArray(input.cc),
          subject = optionalStr(input.subject),
          body = optionalStr(input.body),
          html = optionalStr(input.html),
          attach = strArray(input.attachments),
          ctx = toolCtx
        )
        .map(_.fold(textError, message => message: js.Any))
        .recover { case e => textError(e.getMessage) }

/** The frontend data plane: the Email panel, the thread-header badge, and the
  * user-only send/discard/edit operations.
  */
object Rpc:
  /** Minimal Standard Schema v1 value: bb's RPC layer only needs `~standard.validate`. */
  private def make(validate: js.Function1[js.Any, js.Any]): js.Any =
    js.Dynamic.literal(
      "~standard" -> js.Dynamic.literal("version" -> 1, "vendor" -> "bb-plugin-agentmail", "validate" -> validate)
    )

  private def invalid(message: String): js.Any =
    js.Dynamic.literal("issues" -> js.Array(js.Dynamic.literal("message" -> message)))

  private val passthrough: js.Any =
    make(js.Any.fromFunction1(value => js.Dynamic.literal("value" -> value.asInstanceOf[js.Any])))

  /** An object with exactly these required non-empty string fields (extra fields pass through). */
  private def strings(fields: String*): js.Any =
    make(js.Any.fromFunction1 { value =>
      if value == null || js.typeOf(value) != "object" then invalid("expected an object")
      else
        val d = value.asInstanceOf[js.Dynamic]
        fields
          .collectFirst {
            case name
                if {
                  val v = d.selectDynamic(name)
                  js.typeOf(v) != "string" || v.asInstanceOf[String].isEmpty
                } =>
              invalid(s"$name must be a non-empty string")
          }
          .getOrElse(js.Dynamic.literal("value" -> value.asInstanceOf[js.Any]))
    })

  private def method(input: js.Any): js.Any =
    js.Dynamic.literal("input" -> input, "output" -> passthrough)

  private def draftSummary(d: js.Dynamic): js.Dynamic =
    val attachments = d.attachments
      .asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]]
      .fold(js.Array[String]())(_.map(a => str(a.filename)))
    js.Dynamic.literal(
      "draftId"     -> str(d.draft_id),
      "to"          -> strArray(d.to).toJSArray,
      "cc"          -> strArray(d.cc).toJSArray,
      "subject"     -> str(d.subject),
      "body"        -> str(d.text),
      "hasHtml"     -> str(d.html).nonEmpty,
      "isReply"     -> str(d.in_reply_to).nonEmpty,
      "updatedAt"   -> str(d.updated_at),
      "attachments" -> attachments
    )

  def register(bb: BbApi, settings: BbSettingsHandle, db: SqliteDb): Unit =
    bb.rpc.register(
      js.Dynamic.literal(
        "emailBadge"     -> method(strings("threadId")),
        "emailState"     -> method(strings("threadId")),
        "threadMessages" -> method(strings("emailThreadId")),
        "saveDraft"      -> method(strings("draftId")),
        "sendDraft"      -> method(strings("draftId")),
        "discardDraft"   -> method(strings("draftId"))
      ),
      js.Dynamic.literal(
        "emailBadge" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
          val tid = str(input.threadId)
          js.Dynamic.literal(
            "pendingDrafts" -> Drafts.rowsFor(db, tid).size,
            "emailThreads"  -> db
              .prepare("SELECT COUNT(*) AS n FROM email_threads WHERE bb_thread_id = ?")
              .get(tid)
              .fold(0)(row => str(row.n).toIntOption.getOrElse(0))
          )
        },
        "emailState" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
          emailState(bb, settings, db, str(input.threadId)).toJSPromise
        },
        "threadMessages" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
          threadMessages(settings, str(input.emailThreadId)).toJSPromise
        },
        "saveDraft" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
          saveDraft(bb, settings, db, input).toJSPromise
        },
        "sendDraft" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
          sendDraft(bb, settings, db, str(input.draftId)).toJSPromise
        },
        "discardDraft" -> js.Any.fromFunction1 { (input: js.Dynamic) =>
          discardDraft(bb, settings, db, str(input.draftId)).toJSPromise
        }
      )
    )

  private def emailState(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      bbThreadId: String
  ): Future[js.Dynamic] =
    clientFromSettings(settings).flatMap { client =>
      val rows = Drafts.rowsFor(db, bbThreadId)
      rows
        .foldLeft(Future.successful(js.Array[js.Any]())) { (acc, row) =>
          acc.flatMap { collected =>
            val draftId = str(row.draft_id)
            client
              .getDraft(draftId)
              .map { d =>
                val _ = collected.push(draftSummary(d))
                collected
              }
              .recover { case e =>
                // A draft deleted outside bb (e.g. AgentMail console) self-heals here.
                bb.log.warn(s"dropping stale draft $draftId: ${e.getMessage}")
                Drafts.remove(db, draftId)
                collected
              }
          }
        }
        .map { drafts =>
          val threads = db
            .prepare("SELECT * FROM email_threads WHERE bb_thread_id = ? ORDER BY created_at DESC")
            .all(bbThreadId)
            .map(row =>
              js.Dynamic.literal(
                "emailThreadId" -> str(row.agentmail_thread_id),
                "subject"       -> str(row.subject),
                "counterparty"  -> str(row.counterparty)
              ): js.Any
            )
          js.Dynamic.literal("drafts" -> drafts, "threads" -> threads)
        }
    }

  private def threadMessages(settings: BbSettingsHandle, emailThreadId: String): Future[js.Dynamic] =
    settings.get().toFuture.flatMap { s =>
      val inbox = str(s.inbox)
      clientFromSettings(settings).flatMap { client =>
        client.getThread(emailThreadId).map { thread =>
          val messages = thread.messages
            .asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]]
            .fold(js.Array[js.Any]())(_.map { m =>
              val from = str(m.selectDynamic("from"))
              val attachments = m.attachments
                .asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]]
                .fold(js.Array[js.Any]())(_.map(a =>
                  js.Dynamic.literal(
                    "filename"     -> str(a.filename),
                    "attachmentId" -> str(a.attachment_id)
                  ): js.Any
                ))
              js.Dynamic.literal(
                "messageId"   -> str(m.message_id),
                "from"        -> from,
                "timestamp"   -> str(m.timestamp),
                "body"        -> Seq(str(m.text), str(m.preview)).find(_.nonEmpty).getOrElse("(no text)"),
                "direction"   -> (if inbox.nonEmpty && from.contains(inbox) then "sent" else "received"),
                "attachments" -> attachments
              ): js.Any
            })
          js.Dynamic.literal("messages" -> messages)
        }
      }
    }

  private def rejected(message: String): Future[js.Dynamic] =
    Future.failed(new js.JavaScriptException(new js.Error(message)))

  private def saveDraft(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      input: js.Dynamic
  ): Future[js.Dynamic] =
    val draftId = str(input.draftId)
    Drafts.lookup(db, draftId) match
      case None => rejected(s"unknown draft: $draftId")
      case Some(row) =>
        clientFromSettings(settings).flatMap { client =>
          val payload = js.Dynamic.literal()
          input.to.asInstanceOf[js.UndefOr[js.Array[String]]].foreach(v => payload.updateDynamic("to")(v))
          input.cc.asInstanceOf[js.UndefOr[js.Array[String]]].foreach(v => payload.updateDynamic("cc")(v))
          input.subject.asInstanceOf[js.UndefOr[String]].foreach(v => payload.updateDynamic("subject")(v))
          input.body.asInstanceOf[js.UndefOr[String]].foreach { v =>
            payload.updateDynamic("text")(v)
            // The panel edits plain text only, so an edited body invalidates
            // a previously composed HTML alternative.
            payload.updateDynamic("html")(null)
          }
          client.updateDraft(draftId, payload).map { d =>
            publishChanged(bb, str(row.bb_thread_id))
            draftSummary(d)
          }
        }

  private def sendDraft(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      draftId: String
  ): Future[js.Dynamic] =
    Drafts.lookup(db, draftId) match
      case None => rejected(s"unknown draft: $draftId")
      case Some(row) =>
        val bbThreadId = str(row.bb_thread_id)
        clientFromSettings(settings).flatMap { client =>
          for
            draft <- client.getDraft(draftId)
            sent  <- client.sendDraft(draftId)
          yield
            val emailThreadId = str(sent.thread_id)
            val messageId     = str(sent.message_id)
            val to            = strArray(draft.to)
            val subject       = str(draft.subject)
            Mapping.record(db, emailThreadId, bbThreadId, messageId, subject, to.headOption.getOrElse(""))
            Drafts.remove(db, draftId)
            publishChanged(bb, bbThreadId)
            val notice =
              s"""The user reviewed and sent email draft $draftId from the Email panel.
                 |To: ${to.mkString(", ")}
                 |Subject: $subject
                 |AgentMail thread: $emailThreadId, message: $messageId. Replies will be delivered back to this thread.""".stripMargin
            Poller.deliverToThread(bb, bbThreadId, notice).failed.foreach { e =>
              bb.log.warn(s"failed to notify thread $bbThreadId of sent draft: ${e.getMessage}")
            }
            js.Dynamic.literal("messageId" -> messageId, "emailThreadId" -> emailThreadId)
        }

  private def discardDraft(
      bb: BbApi,
      settings: BbSettingsHandle,
      db: SqliteDb,
      draftId: String
  ): Future[js.Dynamic] =
    Drafts.lookup(db, draftId) match
      case None => rejected(s"unknown draft: $draftId")
      case Some(row) =>
        clientFromSettings(settings).flatMap { client =>
          client.deleteDraft(draftId).map { _ =>
            Drafts.remove(db, draftId)
            publishChanged(bb, str(row.bb_thread_id))
            js.Dynamic.literal("ok" -> true)
          }
        }

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
       |To reply by email: call the agentmail_reply tool with thread $agentmailThreadId (it files a draft the user reviews and sends from the Email panel).""".stripMargin

  def deliverToThread(bb: BbApi, bbThreadId: String, text: String)(using ExecutionContext): Future[Unit] =
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
  ): Future[String] =
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
              val spawnedId = str(spawned.id)
              Mapping.record(
                db,
                agentmailThreadId,
                spawnedId,
                str(full.message_id),
                subject,
                str(full.selectDynamic("from"))
              )
              spawnedId
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
            val bbThreadId = str(mapping.bb_thread_id)
            deliverToThread(bb, bbThreadId, text).map { _ =>
              Mapping.record(
                db,
                agentmailThreadId,
                bbThreadId,
                messageId,
                str(mapping.subject),
                from
              )
              bbThreadId
            }
          case None => spawnForUnmatched(bb, db, full, agentmailThreadId, text)
        deliver.map { bbThreadId =>
          markDelivered(db, messageId)
          publishChanged(bb, bbThreadId)
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
