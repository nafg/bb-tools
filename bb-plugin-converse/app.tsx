// Frontend entry: a voice toggle (composer action) plus a status-line banner
// above the composer. All conversation logic (VAD, recording, upload,
// playback, barge-in) lives in the Scala.js core (app-core.cjs); this file is
// only the React view over it.
import { useEffect, useSyncExternalStore } from "react";
import {
  definePluginApp,
  useComposerView,
  useRealtime,
  useRpc,
  useSettings,
} from "@get-bb/plugin-sdk/app";
import { createController, type ConverseController } from "./app-core.cjs";

// One controller per browser tab: it owns the microphone and outlives any
// single composer. The session keeps running while you navigate; every mounted
// converse component relays realtime signals to it (deduped inside), and each
// utterance is routed to the most recently mounted thread composer — i.e. the
// thread you are looking at.
let controller: ConverseController | null = null;
function getController(rpcCall: (method: string, input: unknown) => Promise<unknown>): ConverseController {
  if (!controller) controller = createController({ rpcCall });
  return controller;
}

const phaseLabels: Record<string, string> = {
  starting: "Starting…",
  listening: "Listening",
  recording: "Hearing you…",
  transcribing: "Transcribing…",
  waiting: "Thinking…",
  speaking: "Speaking",
  error: "Voice error",
};

const phaseColors: Record<string, string> = {
  starting: "text-muted-foreground",
  listening: "text-success",
  recording: "text-destructive",
  transcribing: "text-primary",
  waiting: "text-primary",
  speaking: "text-primary",
  error: "text-destructive",
};

function MicIcon({ className }: { className?: string }) {
  // Lucide "mic" glyph (ISC licensed).
  return (
    <svg
      className={className}
      width="14"
      height="14"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="2"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
    >
      <path d="M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z" />
      <path d="M19 10v2a7 7 0 0 1-14 0v-2" />
      <line x1="12" x2="12" y1="19" y2="22" />
    </svg>
  );
}

// Live input meter: fills with the current mic RMS, turns hot past the VAD
// threshold. If this never moves while "Listening", capture is broken.
function LevelMeter({ level, threshold }: { level: number; threshold: number }) {
  const pct = Math.min(100, (level / (threshold * 8)) * 100);
  const hot = level > threshold;
  return (
    <span className="inline-block h-1 w-8 shrink-0 overflow-hidden rounded-full bg-border" aria-hidden="true">
      <span
        className={"block h-full rounded-full " + (hot ? "bg-destructive" : "bg-success")}
        style={{ width: `${pct}%` }}
      />
    </span>
  );
}

function useConverse() {
  const rpc = useRpc();
  const ctl = getController((method, input) => rpc.call(method as never, input as never));
  const state = useSyncExternalStore(ctl.subscribe, ctl.getSnapshot);
  useRealtime("converse", (payload) => ctl.handleSignal(payload));
  return { ctl, state };
}

function VoiceAction() {
  const settings = useSettings();
  const view = useComposerView();
  const { ctl, state } = useConverse();
  const threadId = view.scope.kind === "thread" ? view.scope.threadId : null;
  useEffect(() => {
    if (threadId !== null) return ctl.registerView(threadId);
  }, [ctl, threadId]);

  if (threadId === null) return null;
  const active = state.phase !== "idle";

  function toggle() {
    if (active) {
      ctl.stop();
      return;
    }
    const values = settings.values ?? {};
    const vadThreshold = Number.parseFloat(
      typeof values.vadThreshold === "string" ? values.vadThreshold : "0.01",
    );
    ctl.start(threadId as string, {
      ttsMode: values.ttsMode === "server" ? "server" : "browser",
      vadThreshold: Number.isFinite(vadThreshold) && vadThreshold > 0 ? vadThreshold : 0.01,
    });
  }

  const color = active ? (phaseColors[state.phase] ?? "text-foreground") : "text-muted-foreground";
  const pulse = active && (state.phase === "recording" || state.phase === "speaking");
  const title = active
    ? "Voice conversation is on — click to stop"
    : "Start a voice conversation (utterances go to the thread you are viewing)";

  return (
    <button
      type="button"
      onClick={toggle}
      title={title}
      aria-label={title}
      aria-pressed={active}
      className={
        "flex h-7 items-center rounded-md border px-2 text-xs hover:bg-accent " +
        (active ? "border-primary/50 bg-accent " : "border-transparent ") +
        color
      }
    >
      <MicIcon className={pulse ? "animate-pulse" : undefined} />
    </button>
  );
}

// The status line above the composer while a session is running: phase, live
// level, interim transcription while you are still talking, and the last
// utterance actually sent.
function VoiceBanner() {
  const view = useComposerView();
  const { state } = useConverse();

  if (view.scope.kind !== "thread") return null;
  if (state.phase === "idle") return null;
  const elsewhere = state.threadId !== view.scope.threadId;

  const label = phaseLabels[state.phase] ?? state.phase;
  const color = phaseColors[state.phase] ?? "text-foreground";
  const text = state.error ?? state.interim ?? state.heard;
  const textClass = state.error
    ? "text-destructive"
    : state.interim
      ? "italic text-foreground"
      : "text-muted-foreground";

  return (
    <div className="w-full min-w-0 space-y-0.5 px-2 py-1 text-xs">
      <div className="flex items-center gap-2">
        <span className={"flex shrink-0 items-center gap-1.5 " + color}>
          <MicIcon />
          <span>{label}</span>
        </span>
        <LevelMeter level={state.level} threshold={state.threshold} />
        {elsewhere && <span className="text-muted-foreground">→ another thread</span>}
      </div>
      {text && <div className={"break-words " + textClass}>{text}</div>}
    </div>
  );
}

export default definePluginApp((app) => {
  app.contentScripts.register({
    id: "lifecycle",
    mount() {
      return () => {
        controller?.stop();
        controller = null;
      };
    },
  });
  app.composer.customize({
    id: "voice",
    scopes: ["thread"],
    actions: [{ id: "voice", component: VoiceAction }],
    banners: [{ id: "voice-status", chrome: "bare", component: VoiceBanner }],
  });
});
