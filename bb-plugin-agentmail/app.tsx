// Frontend entry: thin React shell over the Scala.js core (app-core.cjs).
// All rendering and state (draft cards with autosave/stale handling, the
// email panel, the header badge, the message directive card) live in the
// agentmail-app Bleep project; this file only binds bb's hooks (rpc,
// realtime, navigation) and passes them in as plain functions.
import { useState } from "react";
import {
  definePluginApp,
  useBbNavigate,
  useRealtime,
  useRpc,
} from "@get-bb/plugin-sdk/app";
import {
  AgentmailDirective,
  EmailHeaderButton,
  EmailPanel,
  type RpcCall,
} from "./app-core.cjs";

function useAgentmailRpc(): RpcCall {
  const rpc = useRpc();
  return (method, input) => rpc.call(method as never, input as never);
}

/** Increments when a realtime change lands for this thread; the Scala
 * components refetch when the value moves. */
function useChangeSignal(threadId: string): number {
  const [signal, setSignal] = useState(0);
  useRealtime("agentmail", (payload) => {
    const p = payload as { threadId?: string };
    if (p.threadId === threadId) setSignal((s) => s + 1);
  });
  return signal;
}

function useOpenEmailPanel(): () => void {
  const navigate = useBbNavigate();
  return () => {
    navigate.openThreadPanel({ actionId: "email", title: "Email" });
  };
}

function PanelSlot({ threadId }: { threadId: string; params: unknown }) {
  const call = useAgentmailRpc();
  const signal = useChangeSignal(threadId);
  return <EmailPanel threadId={threadId} call={call} signal={signal} />;
}

function HeaderSlot({ threadId }: { threadId: string }) {
  const call = useAgentmailRpc();
  const signal = useChangeSignal(threadId);
  const openPanel = useOpenEmailPanel();
  return (
    <EmailHeaderButton threadId={threadId} call={call} signal={signal} openPanel={openPanel} />
  );
}

function DirectiveSlot({ attributes }: { attributes: Readonly<Record<string, string>> }) {
  const openPanel = useOpenEmailPanel();
  const draft = typeof attributes.draft === "string" ? attributes.draft : undefined;
  return <AgentmailDirective draft={draft} openPanel={openPanel} />;
}

export default definePluginApp((app) => {
  app.slots.threadPanelAction({
    id: "email",
    title: "Email",
    icon: "Mail",
    component: PanelSlot,
  });
  app.slots.experimental_threadHeaderAction({
    id: "email",
    title: "Email",
    component: HeaderSlot,
  });
  app.slots.messageDirective({
    id: "agentmail",
    component: DirectiveSlot,
  });
});
