package billing

import bbplugin.BbApi
import bbplugin.Dyn.str
import billingcore.*

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

/** Sending a thread from an agent: the plan is shown to the user in the thread's composer and nothing is written to
  * Zoho until they confirm it. The confirmation is checked against a fresh plan, so a change under the user's feet (a
  * new decision, a changed choice, a window left out) refuses rather than sends something other than what was shown.
  */
object SendConfirmation:
  val RendererId = "send-thread"

  private def fingerprint(p: PlanResult): String =
    (p.entries.map(e => s"${e.day}:${e.billedMinutes}:${e.notes}") :+ p.target.fold("")(t =>
      s"${t.projectId}/${t.taskId}"
    )).mkString("|")

  def run(
    bb: BbApi,
    service: Service,
    commands: Commands,
    threadId: String,
    root: String,
    pick: DraftPick,
    subthreads: Boolean,
    signal: js.Any
  )(using ExecutionContext): Future[Either[String, String]] =
    commands.run(BillingRpc.Summarize(root, subthreads, pick))
      .flatMap(_ => commands.run(BillingRpc.Draft(root, subthreads, pick))).map(Right(_))
      .recover { case e => Left(errorMessage(e)) }
      .flatMap {
        case Left(error)                              => Future.successful(Left(error))
        case Right(p) if service.blockers(p).nonEmpty =>
          Future.successful(Left(s"Cannot send yet: ${service.blockers(p).mkString("; ")}"))
        case Right(p)                                 =>
          val shown   = fingerprint(p)
          val request = js.Dynamic.literal(
            "threadId"     -> threadId,
            "rendererId"   -> RendererId,
            "title"        -> s"Send ${p.root.title} to Zoho Books",
            "payload"      -> Json.sendConfirmation(p),
            "presentation" -> js.Dynamic.literal(
              "label" -> js.Dynamic.literal(
                "pending"   -> "Waiting for you to confirm sending time to Zoho Books",
                "completed" -> "Answered about sending time to Zoho Books"
              )
            )
          )
          val options = js.Dynamic.literal()
          if signal != null && !js.isUndefined(signal) then options.updateDynamic("signal")(signal)
          bb.ui.requestInput(request, options).toFuture.flatMap { answer =>
            str(answer.outcome) match
              case "submitted"
                  if answer.value.asInstanceOf[js.Dynamic].confirm.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false) =>
                commands.run(BillingRpc.Draft(root, subthreads, pick)).flatMap { again =>
                  if fingerprint(again) != shown then
                    Future.successful(
                      Left("The plan changed after the user reviewed it; nothing was sent. Run the plan again.")
                    )
                  else
                    commands.run(BillingRpc.Submit(root, subthreads, pick, billed = true)).map(s =>
                      Right(
                        s"Sent ${p.root.title} to Zoho Books: " +
                          s.entries.map(e => s"${e.day} ${ZohoEntries.logTime(e.minutes)} (entry ${e.timeEntryId})")
                            .mkString(", ")
                      )
                    ).recover { case e => Left(errorMessage(e)) }
                }
              case "submitted" => Future.successful(Left("The user declined to send. Nothing was sent."))
              case other       => Future.successful(Left(
                  s"The user did not confirm (${Option(str(answer.reason)).filter(_.nonEmpty).getOrElse(other)}). Nothing was sent."
                ))
          }
      }
