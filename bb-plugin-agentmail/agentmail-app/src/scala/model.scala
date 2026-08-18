package agentmailapp

import scala.scalajs.js

// Facades for the plain-JS values that cross the app.tsx boundary: rpc result
// shapes (produced by the backend's Rpc object) and the props each exported
// component receives. Non-native traits suffice — these are read-only views
// of plain objects.

/** `(method, input) => Promise<result>` — the bb rpc call, bound in app.tsx. */
type RpcCall = js.Function2[String, js.Any, js.Promise[js.Any]]

trait DraftSummary extends js.Object:
  val draftId: String
  val to: js.Array[String]
  val cc: js.Array[String]
  val subject: String
  val body: String
  val hasHtml: Boolean
  val isReply: Boolean
  val updatedAt: String
  val attachments: js.Array[String]

trait EmailThreadSummary extends js.Object:
  val emailThreadId: String
  val subject: String
  val counterparty: String

trait EmailStateResult extends js.Object:
  val drafts: js.Array[DraftSummary]
  val threads: js.Array[EmailThreadSummary]

trait EmailAttachment extends js.Object:
  val filename: String
  val attachmentId: String

trait EmailMessage extends js.Object:
  val messageId: String
  val from: String
  val timestamp: String
  val body: String
  val direction: String // "sent" | "received"
  val attachments: js.Array[EmailAttachment]

trait ThreadMessagesResult extends js.Object:
  val messages: js.Array[EmailMessage]

trait EmailBadgeResult extends js.Object:
  val pendingDrafts: Int
  val emailThreads: Int

/** signal increments when a realtime change lands for this thread; components
  * refetch when it moves.
  */
trait PanelProps extends js.Object:
  val threadId: String
  val call: RpcCall
  val signal: Int

trait HeaderButtonProps extends js.Object:
  val threadId: String
  val call: RpcCall
  val signal: Int
  val openPanel: js.Function0[Unit]

trait DirectiveProps extends js.Object:
  val draft: js.UndefOr[String]
  val openPanel: js.Function0[Unit]

def errorMessage(e: Throwable): String =
  e match
    case js.JavaScriptException(err) => err.toString
    case other                       => Option(other.getMessage).getOrElse(other.toString)

def splitAddresses(text: String): List[String] =
  text.split("[,\\s]+").iterator.map(_.trim).filter(_.nonEmpty).toList
