// Frontend entry: a thin React shell over the Scala.js core (app-core.cjs).
// The week review, the report, and the per-thread mapping panel are rendered
// by the billing-app Bleep project; this file binds bb's hooks (rpc,
// navigation) and registers the surfaces.
import { definePluginApp, useBbNavigate, useRealtime, useRpc } from "@get-bb/plugin-sdk/app";
import { useState } from "react";
import { AssignPanel, ReviewPage, SendThreadForm, type RpcCall } from "./app-core.cjs";

const PANEL_PATH = "billing";

function useBillingRpc(): RpcCall {
  const rpc = useRpc();
  return (method, input) => rpc.call(method as never, input as never);
}

type RealtimeEvent = { seq: number; payload: unknown };

/** The latest message the backend published on the billing channel; the Scala core decodes it. */
function useBillingEvent(): RealtimeEvent | null {
  const [event, setEvent] = useState<RealtimeEvent | null>(null);
  useRealtime("billing", (payload) => setEvent((prev) => ({ seq: (prev?.seq ?? 0) + 1, payload })));
  return event;
}

function PageSlot({ subPath }: { subPath: string }) {
  const call = useBillingRpc();
  const navigate = useBbNavigate();
  const event = useBillingEvent();
  return (
    <ReviewPage
      call={call}
      subPath={subPath}
      navigate={(next) => navigate.toPluginPanel(PANEL_PATH, { subPath: next })}
      openThread={(threadId) => navigate.toThread(threadId)}
      event={event}
    />
  );
}

// An agent's send opens this in the thread composer; the backend waits for the answer.
function SendThreadSlot({
  interaction,
  submit,
  cancel,
}: {
  interaction: { payload: unknown };
  submit: (value: unknown) => void;
  cancel: () => void;
}) {
  return <SendThreadForm plan={interaction.payload} submit={submit} cancel={cancel} />;
}

function ThreadPanelSlot({ threadId }: { threadId: string; params: unknown }) {
  const call = useBillingRpc();
  return <AssignPanel threadId={threadId} call={call} />;
}

export default definePluginApp((app) => {
  app.slots.navPanel({
    id: "billing",
    title: "Billing",
    icon: "Receipt",
    path: PANEL_PATH,
    component: PageSlot,
  });
  app.slots.pendingInteraction({
    id: "send-thread",
    component: SendThreadSlot,
  });
  app.slots.threadPanelAction({
    id: "billing",
    title: "Billing",
    icon: "Receipt",
    component: ThreadPanelSlot,
  });
});
