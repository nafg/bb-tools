// Frontend entry: an invisible probe in each thread pane's header reports
// when that pane is being viewed. The tracking itself lives in the Scala.js
// core (app-core.cjs).
import { useEffect, useRef } from "react";
import { definePluginApp, useSidebarSplitLayout } from "@get-bb/plugin-sdk/app";
import { createViewTracker, type PaneHandle, type ViewTracker } from "./app-core.cjs";

// Plain fetch so a final report can use `keepalive` and finish after the
// page goes away.
async function report(input: unknown, keepalive: boolean): Promise<unknown> {
  const response = await fetch("/api/v1/plugins/activity-recorder/rpc/reportView", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(input),
    keepalive,
  });
  const envelope = (await response.json()) as
    | { ok: true; result: unknown }
    | { ok: false; error?: { message?: string } };
  if (!envelope.ok) throw new Error(envelope.error?.message ?? "reportView failed");
  return envelope.result;
}

// One tracker per browser tab: it listens for input and visibility once.
let tracker: ViewTracker | null = null;
function getTracker(): ViewTracker {
  if (!tracker) tracker = createViewTracker({ report });
  return tracker;
}

function ViewProbe({ threadId }: { threadId: string }) {
  const layout = useSidebarSplitLayout();
  const focused =
    layout === null || layout.panes.some((pane) => pane.threadId === threadId && pane.isFocused);
  const handle = useRef<PaneHandle | null>(null);
  useEffect(() => {
    handle.current = getTracker().attach(threadId);
    return () => {
      handle.current?.dispose();
      handle.current = null;
    };
  }, [threadId]);
  useEffect(() => {
    handle.current?.setFocused(focused);
  }, [threadId, focused]);
  return null;
}

export default definePluginApp((app) => {
  app.slots.experimental_threadHeaderAction({
    id: "view-probe",
    title: "Activity recorder",
    component: ViewProbe,
  });
  app.contentScripts.register({
    id: "view-tracker",
    mount() {
      return () => {
        tracker?.dispose();
        tracker = null;
      };
    },
  });
});
