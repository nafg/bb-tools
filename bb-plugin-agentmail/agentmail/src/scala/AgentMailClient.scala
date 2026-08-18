package agentmail

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import java.net.URLEncoder

class AgentMailException(message: String) extends Exception(message)

/** Minimal AgentMail REST client (https://docs.agentmail.to). Retries once on 429
  * honoring Retry-After.
  */
class AgentMailClient(apiKey: String, inboxId: String)(using ExecutionContext):
  private val baseUrl = "https://api.agentmail.to/v0"
  private val inboxPath = s"$baseUrl/inboxes/${URLEncoder.encode(inboxId, "UTF-8")}"

  private def delay(ms: Double): Future[Unit] =
    val p = Promise[Unit]()
    val _ = js.timers.setTimeout(ms)(p.success(()))
    p.future

  private def request(method: String, url: String, body: Option[js.Any], retriesLeft: Int = 1): Future[js.Dynamic] =
    val init = js.Dynamic.literal(
      method = method,
      headers = js.Dynamic.literal(
        "Authorization" -> s"Bearer $apiKey",
        "Content-Type"  -> "application/json"
      )
    )
    body.foreach(b => init.updateDynamic("body")(js.JSON.stringify(b.asInstanceOf[js.Object])))
    js.Dynamic.global.fetch(url, init).asInstanceOf[js.Promise[js.Dynamic]].toFuture.flatMap { resp =>
      val status = resp.status.asInstanceOf[Int]
      if status == 429 && retriesLeft > 0 then
        val retryAfter = resp.headers.get("retry-after").asInstanceOf[String | Null]
        val waitMs = Option(retryAfter).flatMap(_.toDoubleOption).map(_ * 1000).getOrElse(5000.0)
        delay(waitMs).flatMap(_ => request(method, url, body, retriesLeft - 1))
      else if status >= 400 then
        resp.text().asInstanceOf[js.Promise[String]].toFuture.flatMap { text =>
          Future.failed(AgentMailException(s"AgentMail $method $url failed: $status $text"))
        }
      else
        resp.text().asInstanceOf[js.Promise[String]].toFuture.map { text =>
          if text.isEmpty then js.Dynamic.literal() else js.JSON.parse(text)
        }
    }

  /** Returns { message_id, thread_id }. */
  def sendMessage(payload: js.Any): Future[js.Dynamic] =
    request("POST", s"$inboxPath/messages/send", Some(payload))

  /** Creates a draft; `in_reply_to` in the payload makes it a reply draft whose
    * recipients, subject, and threading derive from the referenced message.
    * Returns the draft object including draft_id.
    */
  def createDraft(payload: js.Any): Future[js.Dynamic] =
    request("POST", s"$inboxPath/drafts", Some(payload))

  /** Full draft object. */
  def getDraft(draftId: String): Future[js.Dynamic] =
    request("GET", s"$inboxPath/drafts/${URLEncoder.encode(draftId, "UTF-8")}", None)

  /** Partial update; null clears a field. Returns the updated draft. */
  def updateDraft(draftId: String, payload: js.Any): Future[js.Dynamic] =
    request("PATCH", s"$inboxPath/drafts/${URLEncoder.encode(draftId, "UTF-8")}", Some(payload))

  def deleteDraft(draftId: String): Future[js.Dynamic] =
    request("DELETE", s"$inboxPath/drafts/${URLEncoder.encode(draftId, "UTF-8")}", None)

  /** Sends an existing draft as-is. Returns { message_id, thread_id }. */
  def sendDraft(draftId: String): Future[js.Dynamic] =
    request("POST", s"$inboxPath/drafts/${URLEncoder.encode(draftId, "UTF-8")}/send", Some(js.Dynamic.literal()))

  /** Returns { message_id, thread_id }. */
  def replyToMessage(messageId: String, payload: js.Any): Future[js.Dynamic] =
    request("POST", s"$inboxPath/messages/${URLEncoder.encode(messageId, "UTF-8")}/reply", Some(payload))

  /** Returns { count, messages, next_page_token }. Message items carry ids, labels,
    * timestamp, from, subject, preview — not full bodies.
    */
  def listMessages(limit: Int, after: Option[String]): Future[js.Dynamic] =
    val params = new scala.collection.mutable.StringBuilder(s"limit=$limit&ascending=true")
    after.foreach(a => params.append("&after=").append(URLEncoder.encode(a, "UTF-8")))
    request("GET", s"$inboxPath/messages?$params", None)

  /** Full message including text/html bodies and attachment metadata. */
  def getMessage(messageId: String): Future[js.Dynamic] =
    request("GET", s"$inboxPath/messages/${URLEncoder.encode(messageId, "UTF-8")}", None)

  /** Thread with its messages. */
  def getThread(threadId: String): Future[js.Dynamic] =
    request("GET", s"$inboxPath/threads/${URLEncoder.encode(threadId, "UTF-8")}", None)

  /** Attachment metadata including a temporary download_url. */
  def getAttachment(messageId: String, attachmentId: String): Future[js.Dynamic] =
    request(
      "GET",
      s"$inboxPath/messages/${URLEncoder.encode(messageId, "UTF-8")}/attachments/${URLEncoder.encode(attachmentId, "UTF-8")}",
      None
    )

  /** Downloads attachment bytes as base64. */
  def downloadAttachment(messageId: String, attachmentId: String): Future[(js.Dynamic, String)] =
    getAttachment(messageId, attachmentId).flatMap { meta =>
      val url = meta.download_url.asInstanceOf[String]
      js.Dynamic.global.fetch(url).asInstanceOf[js.Promise[js.Dynamic]].toFuture.flatMap { resp =>
        if resp.ok.asInstanceOf[Boolean] then
          resp.arrayBuffer().asInstanceOf[js.Promise[js.typedarray.ArrayBuffer]].toFuture.map { buf =>
            val base64 =
              js.Dynamic.global.Buffer.from(buf).applyDynamic("toString")("base64").asInstanceOf[String]
            (meta, base64)
          }
        else Future.failed(AgentMailException(s"attachment download failed: ${resp.status}"))
      }
    }
