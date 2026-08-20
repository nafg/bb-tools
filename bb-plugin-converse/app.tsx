// Frontend entry. Persistent voice controls live in application chrome while
// the pending transcript appears beside the composer receiving that utterance.
import { useEffect, useState, useSyncExternalStore } from "react";
import {
  definePluginApp,
  experimental_useSidebarThreads,
  useBbContext,
  useComposerView,
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
  const sidebar = experimental_useSidebarThreads();
  const ctl = getController();
  useRealtime("converse", (payload) => ctl.handleSignal(payload));
  useEffect(() => {
    if (bbContext.threadId) ctl.noteViewed(bbContext.threadId);
  }, [ctl, bbContext.threadId]);
  useEffect(() => {
    ctl.setThreadDisplayNames(
      Object.fromEntries(
        sidebar.threads.map((thread) => [
          thread.id,
          thread.title ?? thread.titleFallback ?? thread.id,
        ]),
      ),
    );
  }, [ctl, sidebar.threads]);
  useEffect(() => {
    const values = settings.values ?? {};
    const vadThreshold = Number.parseFloat(
      typeof values.vadThreshold === "string" ? values.vadThreshold : "0.015",
    );
    const endOfSpeechSilenceMs = Number.parseFloat(
      typeof values.endOfSpeechSilenceMs === "string" ? values.endOfSpeechSilenceMs : "3000",
    );
    ctl.configure({
      ttsMode: values.ttsProvider === "browser" || values.ttsProvider === undefined ? "browser" : "server",
      vadThreshold: Number.isFinite(vadThreshold) && vadThreshold > 0 ? vadThreshold : 0.015,
      endOfSpeechSilenceMs:
        Number.isFinite(endOfSpeechSilenceMs) && endOfSpeechSilenceMs >= 30
          ? endOfSpeechSilenceMs
          : 3000,
    });
  }, [ctl, settings.values]);
  return null;
}

// The live transcription belongs to the thread that will receive it, not to
// the global microphone control. It grows with its contents instead of
// clipping inside the narrow sidebar widget. Once submitted, the ordinary
// timeline message replaces it.
function PendingVoiceTranscript() {
  const view = useComposerView();
  const ctl = getController();
  const state = useSyncExternalStore(ctl.subscribe, ctl.getSnapshot);

  if (view.scope.kind !== "thread" || state.threadId !== view.scope.threadId) return null;
  const text = state.error ?? state.interim;
  if (!text) return null;

  return (
    <div
      aria-live="polite"
      className={
        "w-full min-w-0 break-words px-2 py-1 text-xs " +
        (state.error ? "text-destructive" : "italic text-foreground")
      }
    >
      {text}
    </div>
  );
}

type VoiceChoice = { id: string; enabled: boolean };
type VoiceSettingsState = {
  enabled: boolean;
  provider: string;
  voices: VoiceChoice[];
};

async function playVoicePreview(voice: string): Promise<void> {
  const AudioContextClass = window.AudioContext;
  const context = new AudioContextClass();
  await context.resume();
  const response = await fetch("/api/v1/plugins/converse/http/tts", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ text: `This thread uses the ${voice.replaceAll("_", " ")} voice.`, voice }),
  });
  if (!response.ok) throw new Error(`Voice preview failed (${response.status})`);
  const decoded = await context.decodeAudioData(await response.arrayBuffer());
  const source = context.createBufferSource();
  source.buffer = decoded;
  source.connect(context.destination);
  source.onended = () => void context.close();
  source.start();
}

function VoiceSettingsSection() {
  const settings = useSettings();
  const [state, setState] = useState<VoiceSettingsState | null>(null);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const isPocket = settings.values?.ttsProvider === "pocket-tts";

  const load = () =>
    rpcCall("voiceSettings", {})
      .then((result) => setState(result as VoiceSettingsState))
      .catch((cause) => setError(cause instanceof Error ? cause.message : String(cause)));

  useEffect(() => {
    void load();
  }, []);
  useRealtime("converse-voices", () => void load());

  const save = async (next: VoiceSettingsState) => {
    setState(next);
    setSaving(true);
    setError(null);
    try {
      await rpcCall("updateVoiceSettings", {
        enabled: next.enabled,
        voices: next.voices.filter((voice) => voice.enabled).map((voice) => voice.id),
      });
    } catch (cause) {
      setError(cause instanceof Error ? cause.message : String(cause));
      await load();
    } finally {
      setSaving(false);
    }
  };

  if (!state) return <div className="text-xs text-muted-foreground">Loading voices…</div>;

  return (
    <div className="space-y-3">
      <label className="flex items-center gap-2 text-sm">
        <input
          type="checkbox"
          checked={state.enabled}
          disabled={saving}
          onChange={(event) => void save({ ...state, enabled: event.target.checked })}
        />
        Different voices by thread
      </label>
      {!isPocket && (
        <p className="text-xs text-muted-foreground">
          Voice assignment applies when Pocket TTS is the selected text-to-speech provider.
        </p>
      )}
      <div className="grid gap-1 sm:grid-cols-2">
        {state.voices.map((voice) => (
          <div key={voice.id} className="flex items-center justify-between gap-2 rounded border px-2 py-1">
            <label className="flex min-w-0 items-center gap-2 text-sm">
              <input
                type="checkbox"
                checked={voice.enabled}
                disabled={saving}
                onChange={(event) =>
                  void save({
                    ...state,
                    voices: state.voices.map((item) =>
                      item.id === voice.id ? { ...item, enabled: event.target.checked } : item,
                    ),
                  })
                }
              />
              <span className="truncate">{voice.id.replaceAll("_", " ")}</span>
            </label>
            <button
              type="button"
              className="rounded px-2 py-1 text-xs hover:bg-muted disabled:opacity-50"
              disabled={!isPocket}
              onClick={() => void playVoicePreview(voice.id).catch((cause) => setError(String(cause)))}
            >
              Preview
            </button>
          </div>
        ))}
      </div>
      {error && <p className="text-xs text-destructive">{error}</p>}
    </div>
  );
}

type ThreadVoiceState = { enabled: boolean; voice: string | null; voices: VoiceChoice[] };

function ThreadVoicePicker({ threadId }: { threadId: string }) {
  const [state, setState] = useState<ThreadVoiceState | null>(null);
  const [saving, setSaving] = useState(false);

  const load = () =>
    rpcCall("threadVoice", { threadId }).then((result) => setState(result as ThreadVoiceState));
  useEffect(() => {
    void load();
  }, [threadId]);
  useRealtime("converse-voices", (payload) => {
    const changed = payload as { threadId?: string };
    if (!changed.threadId || changed.threadId === threadId) void load();
  });

  if (!state) return null;
  const enabledVoices = state.voices.filter((voice) => voice.enabled);
  return (
    <select
      aria-label="Voice for this thread"
      title={state.enabled ? "Voice for this thread" : "Voice for this thread (different voices are off)"}
      className="h-7 max-w-32 rounded border bg-background px-1 text-xs text-foreground"
      value={state.voice ?? ""}
      disabled={saving}
      onChange={async (event) => {
        const voice = event.target.value;
        setState({ ...state, voice: voice || null });
        setSaving(true);
        try {
          await rpcCall("setThreadVoice", { threadId, voice });
        } finally {
          setSaving(false);
          await load();
        }
      }}
    >
      <option value="">Not assigned</option>
      {enabledVoices.map((voice) => (
        <option key={voice.id} value={voice.id}>
          {voice.id.replaceAll("_", " ")}
        </option>
      ))}
    </select>
  );
}

export default definePluginApp((app) => {
  app.slots.settingsSection({
    id: "thread-voices",
    title: "Thread voices",
    description: "Choose which Pocket TTS voices Converse may assign, and listen before enabling them.",
    component: VoiceSettingsSection,
  });
  app.slots.experimental_threadHeaderAction({
    id: "thread-voice",
    title: "Thread voice",
    component: ThreadVoicePicker,
  });
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
    banners: [
      { id: "bridge", chrome: "bare", component: VoiceBridge },
      { id: "pending-transcript", chrome: "bare", component: PendingVoiceTranscript },
    ],
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
  app.slots.messageAction({
    id: "speak-message",
    title: "Speak this message",
    icon: "Play",
    run: ({ threadId, message, selectedText }) =>
      getController().replay(selectedText ?? message.text, threadId),
  });
});
