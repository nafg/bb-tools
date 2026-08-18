// Frontend entry. The visible voice UI is application chrome: a floating
// widget at the bottom of the window, rendered by the Scala.js core from a
// content script, outside any thread surface. React's only remaining job is
// an invisible bridge that forwards what content scripts cannot reach —
// realtime signals, the routed thread, and settings — to the global
// controller.
import { useEffect } from "react";
import {
  definePluginApp,
  useBbContext,
  useRealtime,
  useSettings,
} from "@get-bb/plugin-sdk/app";
import { createController, type ConverseController } from "./app-core.cjs";

// Plain-fetch RPC so the controller exists independently of React.
async function rpcCall(method: string, input: unknown): Promise<unknown> {
  const response = await fetch(`/api/v1/plugins/converse/rpc/${method}`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(input),
  });
  const envelope = (await response.json()) as
    | { ok: true; result: unknown }
    | { ok: false; error?: { message?: string } };
  if (!envelope.ok) throw new Error(envelope.error?.message ?? `rpc ${method} failed`);
  return envelope.result;
}

// One controller per browser tab: it owns the microphone and the widget.
let controller: ConverseController | null = null;
function getController(): ConverseController {
  if (!controller) controller = createController({ rpcCall });
  return controller;
}

// Invisible: forwards route, settings, and realtime signals to the controller.
function VoiceBridge() {
  const bbContext = useBbContext();
  const settings = useSettings();
  const ctl = getController();
  useRealtime("converse", (payload) => ctl.handleSignal(payload));
  useEffect(() => {
    if (bbContext.threadId) ctl.noteViewed(bbContext.threadId);
  }, [ctl, bbContext.threadId]);
  useEffect(() => {
    const values = settings.values ?? {};
    const vadThreshold = Number.parseFloat(
      typeof values.vadThreshold === "string" ? values.vadThreshold : "0.01",
    );
    ctl.configure({
      ttsMode: values.ttsProvider === "browser" || values.ttsProvider === undefined ? "browser" : "server",
      vadThreshold: Number.isFinite(vadThreshold) && vadThreshold > 0 ? vadThreshold : 0.01,
    });
  }, [ctl, settings.values]);
  return null;
}

export default definePluginApp((app) => {
  app.contentScripts.register({
    id: "voice-widget",
    mount({ experimental_setThreadRowStatus }) {
      const ctl = getController();
      const unmountWidget = ctl.mountWidget();
      // Badge the routing target's sidebar row while a session is active
      // (feature-detected: older bb clients lack the setter).
      // Two independent badges: a mic on the routing target, and a speaker on
      // the thread whose reply is currently playing (they can differ).
      const marked = new Map<string, string>();
      let unsubscribe = () => {};
      if (experimental_setThreadRowStatus) {
        unsubscribe = ctl.subscribe(() => {
          const snap = ctl.getSnapshot();
          const desired = new Map<string, { icon: string; label: string }>();
          if (snap.phase !== "idle" && snap.threadId)
            desired.set(snap.threadId, { icon: "Mic", label: "Voice conversation target" });
          if (snap.speakingThread)
            desired.set(snap.speakingThread, { icon: "AudioLines", label: "Speaking reply aloud" });
          for (const id of [...marked.keys()])
            if (!desired.has(id)) {
              experimental_setThreadRowStatus(id, null);
              marked.delete(id);
            }
          for (const [id, status] of desired) {
            const key = `${id}:${status.icon}`;
            if (marked.get(id) !== key) {
              experimental_setThreadRowStatus(id, { ...status, tone: "running" });
              marked.set(id, key);
            }
          }
        });
      }
      return () => {
        unsubscribe();
        if (experimental_setThreadRowStatus)
          for (const id of marked.keys()) experimental_setThreadRowStatus(id, null);
        unmountWidget();
        controller?.stop();
        controller = null;
      };
    },
  });
  app.composer.customize({
    id: "voice",
    scopes: ["thread"],
    banners: [{ id: "bridge", chrome: "bare", component: VoiceBridge }],
  });
  // Native fallback toggle: always registered, but Widget.scala hides it
  // (matching this title) while the injected widget is anchored, so it only
  // shows when the widget cannot anchor (e.g. bb's sidebar DOM changes).
  app.slots.sidebarFooterAction({
    id: "voice-toggle",
    title: "Toggle voice conversation",
    icon: "Mic",
    run: () => {
      const ctl = getController();
      if (ctl.getSnapshot().phase !== "idle") ctl.stop();
      else ctl.startViewed();
    },
  });
});
