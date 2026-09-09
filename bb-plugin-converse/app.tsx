// Frontend entry. Persistent voice controls live in application chrome while
// the pending transcript appears beside the composer receiving that utterance.
import { useEffect, useState, useSyncExternalStore } from "react";
import {
  definePluginApp,
  experimental_useSidebarThreads,
  useBbContext,
  useBbNavigate,
  useComposerView,
  useRealtime,
  useSettings,
} from "@get-bb/plugin-sdk/app";
import { MicVAD, utils } from "@ricky0123/vad-web";
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
  if (!controller)
    controller = createController({
      rpcCall,
      createMicVad: (options) => MicVAD.new(options),
      encodeWav: (audio) => utils.encodeWAV(audio, 1, 16_000, 1, 16),
    });
  return controller;
}

// Invisible: forwards route, settings, and realtime signals to the controller.
function VoiceBridge() {
  const bbContext = useBbContext();
  const settings = useSettings();
  const sidebar = experimental_useSidebarThreads();
  const navigate = useBbNavigate();
  const ctl = getController();
  useRealtime("converse", (payload) => ctl.handleSignal(payload));
  useEffect(() => {
    ctl.setNavigateToThread(navigate.toThread);
  }, [ctl, navigate]);
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
    const speechProbabilityThreshold = Number.parseFloat(
      typeof values.speechProbabilityThreshold === "string"
        ? values.speechProbabilityThreshold
        : "0.3",
    );
    const endOfSpeechSilenceMs = Number.parseFloat(
      typeof values.endOfSpeechSilenceMs === "string" ? values.endOfSpeechSilenceMs : "3000",
    );
    ctl.configure({
      ttsMode: values.ttsProvider === "browser" || values.ttsProvider === undefined ? "browser" : "server",
      speechProbabilityThreshold:
        Number.isFinite(speechProbabilityThreshold) &&
        speechProbabilityThreshold > 0.05 &&
        speechProbabilityThreshold <= 1
          ? speechProbabilityThreshold
          : 0.3,
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

type VoiceChoice = { id: string; enabled: boolean; label?: string };
type VoiceSettingsState = {
  enabled: boolean;
  provider: string;
  url: string;
  model: string;
  defaultVoice: string;
  apiKeyConfigured: boolean;
  voices: VoiceChoice[];
};

const TTS_PROVIDERS = [
  { id: "browser", label: "Browser speech" },
  { id: "pocket-tts", label: "Pocket TTS" },
  { id: "kokoro", label: "Kokoro" },
  { id: "openai-compatible", label: "OpenAI-compatible TTS" },
] as const;

async function playVoicePreview(provider: string, voice: string): Promise<void> {
  const previewText = `This thread uses the ${voice.replaceAll("_", " ")} voice.`;
  if (provider === "browser") {
    const utterance = new SpeechSynthesisUtterance(previewText);
    const selected = speechSynthesis.getVoices().find((item) => item.voiceURI === voice || item.name === voice);
    if (selected) utterance.voice = selected;
    speechSynthesis.speak(utterance);
    return;
  }
  const AudioContextClass = window.AudioContext;
  const context = new AudioContextClass();
  await context.resume();
  const response = await fetch("/api/v1/plugins/converse/http/tts", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ provider, text: previewText, voice }),
  });
  if (!response.ok) throw new Error(`Voice preview failed (${response.status})`);
  const decoded = await context.decodeAudioData(await response.arrayBuffer());
  const source = context.createBufferSource();
  source.buffer = decoded;
  source.connect(context.destination);
  source.onended = () => void context.close();
  source.start();
}

function TtsProviderCard({
  provider,
  label,
  active,
}: {
  provider: string;
  label: string;
  active: boolean;
}) {
  const [state, setState] = useState<VoiceSettingsState | null>(null);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [apiKey, setApiKey] = useState("");
  const [newVoice, setNewVoice] = useState("");
  const [open, setOpen] = useState(active);

  const browserVoices = () =>
    speechSynthesis.getVoices().map((voice) => ({ id: voice.voiceURI || voice.name, label: voice.name }));

  const load = () =>
    rpcCall("voiceSettings", { provider })
      .then((result) => {
        const loaded = result as VoiceSettingsState;
        if (provider === "browser") {
          const prior = new Map(loaded.voices.map((voice) => [voice.id, voice.enabled]));
          loaded.voices = browserVoices().map((voice) => ({ ...voice, enabled: prior.get(voice.id) ?? true }));
        }
        setState(loaded);
        setApiKey("");
      })
      .catch((cause) => setError(cause instanceof Error ? cause.message : String(cause)));

  useEffect(() => {
    void load();
    if (provider === "browser") {
      const onVoicesChanged = () => void load();
      speechSynthesis.addEventListener("voiceschanged", onVoicesChanged);
      return () => speechSynthesis.removeEventListener("voiceschanged", onVoicesChanged);
    }
  }, [provider]);
  useRealtime("converse-voices", () => void load());

  const save = async () => {
    if (!state) return;
    setSaving(true);
    setError(null);
    try {
      const input: Record<string, unknown> = {
        provider,
        url: state.url,
        model: state.model,
        defaultVoice: state.defaultVoice,
        catalog: state.voices.map((voice) => voice.id),
        voices: state.voices.filter((voice) => voice.enabled).map((voice) => voice.id),
      };
      if (apiKey) input.apiKey = apiKey;
      await rpcCall("updateVoiceSettings", input);
      await load();
    } catch (cause) {
      setError(cause instanceof Error ? cause.message : String(cause));
    } finally {
      setSaving(false);
    }
  };

  return (
    <details className="rounded border" open={open} onToggle={(event) => setOpen(event.currentTarget.open)}>
      <summary className="flex cursor-pointer items-center justify-between gap-3 px-3 py-2 text-sm font-medium">
        <span>{label}</span>
        {active && <span className="rounded bg-primary/10 px-2 py-0.5 text-xs text-primary">Active</span>}
      </summary>
      <div className="space-y-4 border-t p-3">
        {provider === "pocket-tts" && (
          <p className="text-xs text-muted-foreground">Converse starts and stops Pocket TTS automatically.</p>
        )}
        {!state ? (
          <div className="text-xs text-muted-foreground">Loading voices…</div>
        ) : (
          <>
      {(provider === "kokoro" || provider === "openai-compatible") && (
        <label className="block space-y-1 text-sm">
          <span>Endpoint</span>
          <input className="w-full rounded border bg-background px-2 py-1" value={state.url} onChange={(event) => setState({ ...state, url: event.target.value })} />
        </label>
      )}
      {provider === "openai-compatible" && (
        <>
          <label className="block space-y-1 text-sm">
            <span>Model</span>
            <input className="w-full rounded border bg-background px-2 py-1" value={state.model} onChange={(event) => setState({ ...state, model: event.target.value })} />
          </label>
          <label className="block space-y-1 text-sm">
            <span>API key {state.apiKeyConfigured && !apiKey ? "(configured)" : ""}</span>
            <input type="password" className="w-full rounded border bg-background px-2 py-1" value={apiKey} placeholder={state.apiKeyConfigured ? "Leave blank to keep existing key" : "Optional for local services"} onChange={(event) => setApiKey(event.target.value)} />
          </label>
        </>
      )}
      {provider !== "pocket-tts" && provider !== "browser" && (
        <div className="flex gap-2">
          <input className="min-w-0 flex-1 rounded border bg-background px-2 py-1 text-sm" value={newVoice} placeholder="Add a provider voice ID" onChange={(event) => setNewVoice(event.target.value)} />
          <button type="button" className="rounded border px-3 text-sm" onClick={() => {
            const id = newVoice.trim();
            if (id && !state.voices.some((voice) => voice.id === id)) setState({ ...state, voices: [...state.voices, { id, enabled: true }] });
            setNewVoice("");
          }}>Add voice</button>
        </div>
      )}
      <label className="block space-y-1 text-sm">
        <span>Default voice</span>
        <select className="w-full rounded border bg-background px-2 py-1" value={state.defaultVoice} onChange={(event) => setState({ ...state, defaultVoice: event.target.value })}>
          <option value="">Provider default</option>
          {state.voices.map((voice) => <option key={voice.id} value={voice.id}>{voice.label ?? voice.id.replaceAll("_", " ")}</option>)}
        </select>
      </label>
      <div className="space-y-1">
        <div className="text-sm font-medium">Voice pool</div>
        <div className="grid gap-1 sm:grid-cols-2">
        {state.voices.map((voice) => (
          <div key={voice.id} className="flex items-center justify-between gap-2 rounded border px-2 py-1">
            <label className="flex min-w-0 items-center gap-2 text-sm">
              <input
                type="checkbox"
                checked={voice.enabled}
                disabled={saving}
                onChange={(event) =>
                  setState({
                    ...state,
                    voices: state.voices.map((item) =>
                      item.id === voice.id ? { ...item, enabled: event.target.checked } : item,
                    ),
                  })
                }
              />
              <span className="truncate">{voice.label ?? voice.id.replaceAll("_", " ")}</span>
            </label>
            <button
              type="button"
              className="rounded px-2 py-1 text-xs hover:bg-muted disabled:opacity-50"
              onClick={() => void playVoicePreview(provider, voice.id).catch((cause) => setError(String(cause)))}
            >
              Preview
            </button>
          </div>
        ))}
        </div>
      </div>
      <button type="button" disabled={saving} className="rounded bg-primary px-3 py-1.5 text-sm text-primary-foreground disabled:opacity-50" onClick={() => void save()}>
        {saving ? "Saving…" : `Save ${label}`}
      </button>
          </>
        )}
        {error && <p className="text-xs text-destructive">{error}</p>}
      </div>
    </details>
  );
}

function TtsProviderSettingsSection() {
  const settings = useSettings();
  const activeProvider =
    typeof settings.values?.ttsProvider === "string" ? settings.values.ttsProvider : "browser";
  const [threadVoicesEnabled, setThreadVoicesEnabled] = useState<boolean | null>(null);
  const [savingThreadVoices, setSavingThreadVoices] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const loadThreadVoiceSettings = () =>
    rpcCall("threadVoiceSettings", {}).then((result) => {
      setThreadVoicesEnabled((result as { enabled: boolean }).enabled);
    });

  useEffect(() => {
    void loadThreadVoiceSettings().catch((cause) => setError(String(cause)));
  }, []);
  useRealtime("converse-voices", () => void loadThreadVoiceSettings());

  const updateThreadVoicesEnabled = async (enabled: boolean) => {
    setThreadVoicesEnabled(enabled);
    setSavingThreadVoices(true);
    setError(null);
    try {
      await rpcCall("updateThreadVoiceSettings", { enabled });
    } catch (cause) {
      setThreadVoicesEnabled(!enabled);
      setError(cause instanceof Error ? cause.message : String(cause));
    } finally {
      setSavingThreadVoices(false);
    }
  };

  return (
    <div className="space-y-4">
      <div className="space-y-1">
        <label className="flex items-center gap-2 text-sm">
          <input
            type="checkbox"
            checked={threadVoicesEnabled ?? false}
            disabled={threadVoicesEnabled === null || savingThreadVoices}
            onChange={(event) => void updateThreadVoicesEnabled(event.target.checked)}
          />
          Assign a different voice to each thread
        </label>
        <p className="text-xs text-muted-foreground">
          Each provider draws from its own enabled voice pool below.
        </p>
      </div>
      <div className="space-y-2">
        {TTS_PROVIDERS.map((provider) => (
          <TtsProviderCard
            key={provider.id}
            provider={provider.id}
            label={provider.label}
            active={provider.id === activeProvider}
          />
        ))}
      </div>
      {error && <p className="text-xs text-destructive">{error}</p>}
    </div>
  );
}

type ThreadVoiceState = { enabled: boolean; provider: string; voice: string | null; voices: VoiceChoice[] };

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
          {voice.label ?? voice.id.replaceAll("_", " ")}
        </option>
      ))}
    </select>
  );
}

export default definePluginApp((app) => {
  app.slots.settingsSection({
    id: "tts-provider",
    title: "Text-to-speech providers",
    description: "Configure every provider, its default voice, and the voices available across threads.",
    component: TtsProviderSettingsSection,
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
