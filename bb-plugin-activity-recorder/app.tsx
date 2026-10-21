// Frontend entry: a probe in each thread pane's header reports when that
// pane is being viewed, and shows the thread's attention today as a timer
// that runs while the pane counts. The tracking itself lives in the
// Scala.js core (app-core.cjs).
import { useEffect, useRef, useState } from "react";
import { definePluginApp, useRpc, useSidebarSplitLayout } from "@get-bb/plugin-sdk/app";
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

// The static total is re-read this often, and whenever recording flips, to
// pick up attention recorded elsewhere (speech, a reply read aloud).
const RefreshMs = 60_000;

function startOfToday(): number {
  const now = new Date();
  return new Date(now.getFullYear(), now.getMonth(), now.getDate()).getTime();
}

function formatElapsed(ms: number): string {
  const seconds = Math.floor(ms / 1000);
  const h = Math.floor(seconds / 3600);
  const m = Math.floor((seconds % 3600) / 60);
  const s = seconds % 60;
  const mm = m.toString().padStart(2, "0");
  const ss = s.toString().padStart(2, "0");
  return h > 0 ? `${h}:${mm}:${ss}` : `${m}:${ss}`;
}

/** The thread's attention today, as the recorder has it. */
function useTodayTotal(threadId: string, recording: boolean): { ms: number; at: number } | null {
  const rpc = useRpc();
  const [total, setTotal] = useState<{ ms: number; at: number } | null>(null);
  useEffect(() => {
    let cancelled = false;
    const load = async () => {
      const from = startOfToday();
      const at = Date.now();
      try {
        const result = (await rpc.call("intervals", {
          from,
          to: from + 86_400_000,
          threadIds: [threadId],
        })) as { intervals: { start: number; end: number }[] };
        if (cancelled) return;
        const ms = result.intervals.reduce((sum, i) => sum + (i.end - i.start), 0);
        setTotal({ ms, at });
      } catch {
        // Leave the last total standing; the next refresh retries.
      }
    };
    void load();
    const timer = setInterval(() => void load(), RefreshMs);
    return () => {
      cancelled = true;
      clearInterval(timer);
    };
  }, [rpc, threadId, recording]);
  return total;
}

function ViewProbe({ threadId }: { threadId: string }) {
  const layout = useSidebarSplitLayout();
  const focused =
    layout === null || layout.panes.some((pane) => pane.threadId === threadId && pane.isFocused);
  const handle = useRef<PaneHandle | null>(null);
  const [recording, setRecording] = useState(false);
  useEffect(() => {
    const pane = getTracker().attach(threadId);
    handle.current = pane;
    setRecording(pane.recording());
    const unsubscribe = pane.subscribe(() => setRecording(pane.recording()));
    return () => {
      unsubscribe();
      pane.dispose();
      handle.current = null;
    };
  }, [threadId]);
  useEffect(() => {
    handle.current?.setFocused(focused);
  }, [threadId, focused]);

  const total = useTodayTotal(threadId, recording);
  // Re-render each second while the timer runs.
  const [, setTick] = useState(0);
  useEffect(() => {
    if (!recording) return;
    const timer = setInterval(() => setTick((n) => n + 1), 1000);
    return () => clearInterval(timer);
  }, [recording]);

  if (total === null) return null;
  const elapsed = total.ms + (recording ? Date.now() - total.at : 0);
  return (
    <span
      className={`flex items-center gap-1 font-mono text-xs tabular-nums ${recording ? "text-foreground" : "text-muted-foreground"}`}
      title={recording ? "Attention on this thread today; counting while this pane is viewed" : "Attention on this thread today; not counting now"}
      aria-live="off"
    >
      <span aria-hidden="true" className={recording ? "text-destructive" : "opacity-40"}>
        ●
      </span>
      {formatElapsed(elapsed)}
    </span>
  );
}

export default definePluginApp((app) => {
  app.slots.experimental_threadHeaderAction({
    id: "view-probe",
    title: "Attention today",
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
