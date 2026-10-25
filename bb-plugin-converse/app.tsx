// Frontend entry. Persistent voice controls live in application chrome while
// the pending transcript appears beside the composer receiving that utterance.
import { useEffect, useRef, useState, useSyncExternalStore } from "react";
import * as Dialog from "@radix-ui/react-dialog";
import {
  definePluginApp,
  experimental_THREAD_ACTION_GROUPS,
  experimental_useSidebarThreads,
  useBbContext,
  useBbNavigate,
  useComposer,
  useRealtime,
  useSettings,
} from "@get-bb/plugin-sdk/app";
import { MicVAD, utils } from "@ricky0123/vad-web";
import * as RadixSlider from "@radix-ui/react-slider";
import {
  createController,
  type ConverseController,
  type SoundCueConfig,
  type SoundCueNoteConfig,
  type SoundCueSettings,
} from "./app-core.cjs";

// Plain-fetch RPC so the controller exists independently of React.
// `keepalive` lets a small call finish after the page goes away; the browser
// limits such a body to 64 KB, so it is not for uploads.
async function rpcCall(method: string, input: unknown, options?: { keepalive?: boolean }): Promise<unknown> {
  const response = await fetch(`/api/v1/plugins/converse/rpc/${method}`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(input),
    keepalive: options?.keepalive ?? false,
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

// What every composer surface hands the controller: host navigation,
// realtime signals, and sidebar names for the threads and projects it shows.
function useHostBridge(ctl: ConverseController) {
  const sidebar = experimental_useSidebarThreads();
  const navigate = useBbNavigate();
  useRealtime("converse", (payload) => ctl.handleSignal(payload));
  useEffect(() => {
    ctl.setNavigateToThread(navigate.toThread);
  }, [ctl, navigate]);
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
    ctl.setProjectDisplayNames(
      Object.fromEntries(sidebar.projects.map((project) => [project.id, project.name])),
    );
  }, [ctl, sidebar.projects]);
  return sidebar;
}

// The stored sound-cue settings, refreshed whenever any tab saves them.
function useSoundCueSettings() {
  const [state, setState] = useState<SoundCueSettings | null>(null);
  const [error, setError] = useState<string | null>(null);
  const load = () =>
    rpcCall("soundCueSettings", {})
      .then((result) => setState(result as SoundCueSettings))
      .catch((cause) => setError(cause instanceof Error ? cause.message : String(cause)));
  useEffect(() => {
    void load();
  }, []);
  useRealtime("converse-sound-cues", () => void load());
  return { state, setState, error, setError };
}

// Invisible: forwards the viewed thread, settings, and realtime signals to the
// controller. Mounted in every thread composer, including a ThreadChat
// embedded in a plugin page, where the route names no thread: there the
// composer on screen names it. bb keeps previously visited thread pages
// mounted but hidden, so only a composer actually on screen counts.
function VoiceBridge() {
  const bbContext = useBbContext();
  const view = useComposer();
  const composerThread = view.scope.kind === "thread" ? view.scope.threadId : null;
  const settings = useSettings();
  const soundCues = useSoundCueSettings();
  const ctl = getController();
  useHostBridge(ctl);
  const marker = useRef<HTMLSpanElement>(null);
  const [onScreen, setOnScreen] = useState(false);
  useEffect(() => {
    const element = marker.current;
    if (!element) return;
    const observer = new IntersectionObserver(([entry]) => setOnScreen(entry?.isIntersecting ?? false));
    observer.observe(element);
    return () => observer.disconnect();
  }, []);
  useEffect(() => {
    const viewed = bbContext.threadId ?? (onScreen ? composerThread : null);
    if (viewed) ctl.noteViewed(viewed);
  }, [ctl, bbContext.threadId, onScreen, composerThread]);
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
      callAudioOutput: values.callAudioOutput === "webrtc" ? "webrtc" : "direct",
      soundCues: soundCues.state ?? undefined,
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
  }, [ctl, settings.values, soundCues.state]);
  return <span ref={marker} aria-hidden="true" style={{ display: "block", height: 0 }} />;
}

// The live transcription belongs to the thread that will receive it, not to
// the global microphone control. It grows with its contents instead of
// clipping inside the narrow sidebar widget. Once submitted, the ordinary
// timeline message replaces it.
function PendingVoiceTranscript() {
  const view = useComposer();
  const ctl = getController();
  const state = useSyncExternalStore(ctl.subscribe, ctl.getSnapshot);

  if (view.scope.kind !== "thread" || state.threadId !== view.scope.threadId) return null;
  const text = state.notice ?? state.interim ?? state.progress;
  if (!text) return null;

  return (
    <div
      aria-live="polite"
      className={
        "w-full min-w-0 break-words px-2 py-1 text-xs " +
        (state.notice ? "text-destructive" : "italic text-foreground")
      }
    >
      {text}
    </div>
  );
}

// Banner on the New thread composer, where there is no thread to route to.
// While mounted it tells the controller to start a new thread in the
// composer's selected project from the next utterance, and it hosts the live
// transcript the thread banner would otherwise show.
function NewThreadVoiceBanner() {
  const view = useComposer();
  const projectId = view.scope.kind === "new-thread" ? view.scope.projectId : null;
  const ctl = getController();
  const sidebar = useHostBridge(ctl);
  const state = useSyncExternalStore(ctl.subscribe, ctl.getSnapshot);
  useEffect(() => {
    ctl.noteComposeSurface(projectId);
    return () => ctl.leaveComposeSurface();
  }, [ctl, projectId]);

  const projectName = sidebar.projects.find((project) => project.id === projectId)?.name ?? projectId;
  const guidance =
    projectId === null
      ? "Voice: pick a project, then speaking starts a new thread in it."
      : !state.on
        ? `Voice: turn on the microphone and speak to start a new thread in ${projectName}.`
        : `Voice: speaking starts a new thread in ${projectName}.`;
  const live = state.notice ?? state.interim ?? state.progress;
  return (
    <div className="w-full min-w-0 space-y-1 px-2 py-1 text-xs">
      <p className="text-muted-foreground">{guidance}</p>
      {live && (
        <p aria-live="polite" className={"break-words " + (state.notice ? "text-destructive" : "italic text-foreground")}>
          {live}
        </p>
      )}
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
  { id: "qwen3-tts", label: "Qwen3-TTS" },
  { id: "chatterbox-nano", label: "Chatterbox Nano" },
  { id: "kokoro", label: "Kokoro" },
  { id: "openai-compatible", label: "OpenAI-compatible TTS" },
] as const;

async function playVoicePreview(provider: string, voice: string): Promise<void> {
  // A Chatterbox voice is a clip's path, which would read oddly in the sentence.
  const previewText =
    provider === "chatterbox-nano"
      ? "This is how this thread sounds."
      : `This thread uses the ${voice.replaceAll("_", " ")} voice.`;
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
        {provider === "qwen3-tts" && (
          <p className="text-xs text-muted-foreground">
            Converse starts and stops Qwen3-TTS automatically; it runs on the GPU, so Parakeet moves to the CPU
            while it is the provider. The voices are the model's built-in speakers, and a word the agent
            writes between *asterisks* is stressed.
          </p>
        )}
        {provider === "chatterbox-nano" && (
          <p className="text-xs text-muted-foreground">
            Converse starts and stops Chatterbox Nano automatically; it runs on the CPU and leaves the GPU alone.
            Besides its built-in voice, a voice is the path on this host of a WAV clip, over five seconds long,
            of the voice to clone.
          </p>
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
      {provider !== "pocket-tts" && provider !== "qwen3-tts" && provider !== "browser" && (
        <div className="flex gap-2">
          <input className="min-w-0 flex-1 rounded border bg-background px-2 py-1 text-sm" value={newVoice} placeholder={provider === "chatterbox-nano" ? "Path of a voice clip to add" : "Add a provider voice ID"} onChange={(event) => setNewVoice(event.target.value)} />
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

// One or more thumbs on a Radix track in the host's slider style. Radix keeps
// the thumbs from crossing. Class names are host utilities only: the designer
// renders in a portal that the plugin's scoped stylesheet does not reach.
const thumbClass = "block h-4 w-4 rounded-full border border-primary/50 bg-background shadow transition-colors focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring";

// One or more thumbs on a Radix track in the host's slider style. Radix keeps
// the thumbs from crossing. Class names are host utilities only: the designer
// renders in a portal that the plugin's scoped stylesheet does not reach.
type TrackFill = "before" | "after" | "none";

function Thumbs({ label, min, max, step, values, onChange, show, thumbLabels, track, fill = "before" }: {
  label: string;
  min: number;
  max: number;
  step: number;
  values: number[];
  onChange: (values: number[]) => void;
  show: (values: number[]) => string;
  thumbLabels?: string[];
  /** A CSS background for the whole track, replacing the filled range. */
  track?: string;
  /** Which side of a single thumb is filled: the amount before it, the span after it, or neither for a position. */
  fill?: TrackFill;
}) {
  const background = track ?? (fill === "after" ? afterTrack((values[0] - min) / (max - min)) : undefined);
  return (
    <div className="space-y-1 text-xs">
      <div className="flex justify-between gap-2">
        <span className="whitespace-nowrap">{label}</span>
        <output className="tabular-nums text-muted-foreground">{show(values)}</output>
      </div>
      <RadixSlider.Root aria-label={label} min={min} max={max} step={step} value={values} onValueChange={onChange} className="relative flex w-full touch-none select-none items-center">
        <RadixSlider.Track className="relative h-1.5 w-full grow overflow-hidden rounded-full bg-primary/20" style={background ? { backgroundImage: background, backgroundColor: "transparent" } : undefined}>
          {!background && fill === "before" && <RadixSlider.Range className="absolute h-full bg-primary" />}
        </RadixSlider.Track>
        {values.map((_, i) => (
          <RadixSlider.Thumb key={i} aria-label={thumbLabels?.[i]} title={thumbLabels?.[i]} className={thumbClass} />
        ))}
      </RadixSlider.Root>
    </div>
  );
}

// A level, standing up beside the axis it belongs to.
function Level({ label, min, max, step, value, onChange }: { label: string; min: number; max: number; step: number; value: number; onChange: (value: number) => void }) {
  return (
    <div className="flex flex-col items-center gap-1 text-xs">
      <RadixSlider.Root orientation="vertical" aria-label={label} min={min} max={max} step={step} value={[value]} onValueChange={([v]) => onChange(v)} className="relative flex h-12 w-4 touch-none select-none flex-col items-center justify-center">
        <RadixSlider.Track className="relative h-full grow overflow-hidden rounded-full bg-primary/20" style={{ width: 6 }}>
          <RadixSlider.Range className="absolute w-full bg-primary" />
        </RadixSlider.Track>
        <RadixSlider.Thumb aria-label={label} title={label} className={thumbClass} />
      </RadixSlider.Root>
      <span className="whitespace-nowrap">
        {label} <output className="tabular-nums text-muted-foreground">{value.toFixed(decimalsOf(step))}</output>
      </span>
    </div>
  );
}

// A control group: an axis across, and its level standing to the right.
function Group({ legend, axis, level }: { legend: string; axis: React.ReactNode; level: React.ReactNode }) {
  return (
    <fieldset className="rounded border px-2 pb-1">
      <legend className="px-1 text-xs text-muted-foreground">{legend}</legend>
      <div className="grid items-center gap-4" style={{ gridTemplateColumns: "minmax(0,1fr) 6rem" }}>
        <div>{axis}</div>
        {level}
      </div>
    </fieldset>
  );
}

const decimalsOf = (step: number) => Math.max(0, -Math.floor(Math.log10(step)));
const hz = (v: number) => `${Math.round(v)} Hz`;
// Frequencies are dragged on a log scale, like a keyboard: the slider's value is log10(Hz).
const toLog = (hzValue: number) => Math.log10(hzValue);
const fromLog = (v: number) => Math.round(Math.pow(10, v));
const NoFilterHz = 8000;
const NoteMaxSec = 3;

const quiet = "color-mix(in srgb, var(--primary) 8%, transparent)";
const loud = "var(--primary)";

// The envelope painted onto the track: rising to the attack end, held to the release start, falling to the end.
function envelopeTrack(attackEnd: number, releaseStart: number, end: number): string {
  const pct = (t: number) => `${((t / NoteMaxSec) * 100).toFixed(2)}%`;
  return `linear-gradient(to right, ${quiet} 0%, ${loud} ${pct(attackEnd)}, ${loud} ${pct(releaseStart)}, ${quiet} ${pct(end)}, ${quiet} 100%)`;
}

// Filled from the thumb to the end: what starts once the thumb is passed.
function afterTrack(fraction: number): string {
  const pct = `${(fraction * 100).toFixed(2)}%`;
  return `linear-gradient(to right, ${quiet} 0%, ${quiet} ${pct}, ${loud} ${pct}, ${loud} 100%)`;
}

function NoteEditor({ note, onChange }: { note: SoundCueNoteConfig; onChange: (note: SoundCueNoteConfig) => void }) {
  const cut = note.lowpassHz ?? NoFilterHz;
  const end = note.durationSec;
  const attackEnd = Math.min(note.attackSec, end);
  const releaseStart = Math.min(end, Math.max(attackEnd, end - (note.releaseSec ?? end - attackEnd)));
  return (
    <div className="space-y-2">
      <Group
        legend="Tone"
        axis={<Slider label="Pitch" fill="none" min={toLog(60)} max={toLog(2000)} step={0.005} value={toLog(note.frequencyHz)} onChange={(v) => onChange({ ...note, frequencyHz: fromLog(v) })} show={(v) => hz(fromLog(v))} />}
        level={<Level label="Octave" min={0} max={1} step={0.01} value={note.harmonic2} onChange={(v) => onChange({ ...note, harmonic2: v })} />}
      />
      <Group
        legend="Air"
        axis={<Slider label="Brightness" fill="none" min={toLog(100)} max={toLog(NoFilterHz)} step={0.005} value={toLog(cut)} onChange={(v) => onChange({ ...note, lowpassHz: fromLog(v) >= NoFilterHz ? undefined : fromLog(v) })} show={(v) => (fromLog(v) >= NoFilterHz ? "full" : hz(fromLog(v)))} />}
        level={<Level label="Air" min={0} max={1} step={0.01} value={note.pinkNoise} onChange={(v) => onChange({ ...note, pinkNoise: v })} />}
      />
      <Group
        legend="Shape"
        axis={
          <Thumbs
            label="Attack · release · end"
            min={0}
            max={NoteMaxSec}
            step={0.005}
            values={[attackEnd, releaseStart, end]}
            thumbLabels={["Attack ends", "Release starts", "Note ends"]}
            track={envelopeTrack(attackEnd, releaseStart, end)}
            onChange={([a, r, e]) => {
              const noteEnd = Math.max(e, 0.02);
              onChange({ ...note, durationSec: noteEnd, attackSec: Math.min(a, noteEnd), releaseSec: Math.max(0.005, noteEnd - Math.min(r, noteEnd)) });
            }}
            show={([a, r, e]) => `${a.toFixed(2)} · ${r.toFixed(2)} · ${e.toFixed(2)} s`}
          />
        }
        level={<Level label="Volume" min={0} max={1} step={0.005} value={note.gain} onChange={(v) => onChange({ ...note, gain: v })} />}
      />
    </div>
  );
}

function Slider({ label, min, max, step, value, onChange, show, fill }: {
  label: string;
  min: number;
  max: number;
  step: number;
  value: number;
  onChange: (value: number) => void;
  show?: (value: number) => string;
  fill?: TrackFill;
}) {
  return <Thumbs label={label} min={min} max={max} step={step} values={[value]} onChange={([v]) => onChange(v)} show={([v]) => (show ? show(v) : v.toFixed(decimalsOf(step)))} fill={fill} />;
}

const newNote: SoundCueNoteConfig = { frequencyHz: 523, durationSec: 0.2, attackSec: 0.01, gain: 0.35, harmonic2: 0, pinkNoise: 0 };

// Edits one cue with live audition through the controller, so what is heard
// is exactly what a session plays.
function SoundCueDesigner({ cue, config, onSave, onClose }: {
  cue: { key: string; label: string; repeating: boolean };
  config: SoundCueConfig;
  onSave: (config: SoundCueConfig) => Promise<void>;
  onClose: () => void;
}) {
  const ctl = getController();
  const [draft, setDraft] = useState<SoundCueConfig>(config);
  const [auditioning, setAuditioning] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [saving, setSaving] = useState(false);
  const [openNotes, setOpenNotes] = useState<Set<number>>(() => new Set([0]));

  useEffect(() => {
    if (!auditioning) return;
    setError(ctl.auditionCue(cue.key, draft));
    if (!draft.repeat) setAuditioning(false);
  }, [ctl, cue.key, draft, auditioning]);
  useEffect(() => () => ctl.stopAudition(), [ctl]);

  const toggleAudition = () => {
    if (auditioning) {
      ctl.stopAudition();
      setAuditioning(false);
    } else setAuditioning(true);
  };
  const playNote = (note: SoundCueNoteConfig) => setError(ctl.auditionCue(cue.key, { ...draft, notes: [note], repeat: null }));
  const updateNote = (index: number, note: SoundCueNoteConfig) =>
    setDraft({ ...draft, notes: draft.notes.map((existing, i) => (i === index ? note : existing)) });
  const summary = (note: SoundCueNoteConfig) => `${note.frequencyHz} Hz · ${note.durationSec.toFixed(2)} s · vol ${note.gain}`;
  const save = async () => {
    setSaving(true);
    setError(null);
    try {
      ctl.stopAudition();
      await onSave(draft);
    } catch (cause) {
      setError(cause instanceof Error ? cause.message : String(cause));
    } finally {
      setSaving(false);
    }
  };
  const multi = draft.notes.length > 1;

  return (
    <Dialog.Root open onOpenChange={(open) => { if (!open) onClose(); }}>
      <Dialog.Portal>
        <Dialog.Overlay className="fixed inset-0 z-50 bg-black/50" />
        {/* Portaled outside the plugin root, so plugin-scoped Tailwind rules such as max-h-[90vh] do not reach it. */}
        <Dialog.Content
          style={{ maxHeight: "90vh" }}
          className="fixed left-1/2 top-1/2 z-50 w-full max-w-xl -translate-x-1/2 -translate-y-1/2 space-y-3 overflow-y-auto rounded-lg border bg-background p-4 text-foreground shadow-lg"
        >
          <Dialog.Title className="text-base font-medium">{cue.label}</Dialog.Title>
          <Dialog.Description className="text-xs text-muted-foreground">
                {cue.repeating
                  ? "One note, repeated while the session is in this phase. Audition repeats it at the interval, without the lead-in."
                  : "Up to three notes played back to back."}
          </Dialog.Description>
          {draft.notes.map((note, index) =>
            multi ? (
              <details key={index} open={openNotes.has(index)} onToggle={(event) => setOpenNotes((open) => { const next = new Set(open); if (event.currentTarget.open) next.add(index); else next.delete(index); return next; })} className="rounded border px-2">
                <summary className="flex cursor-pointer items-center justify-between gap-2 py-1.5 text-sm">
                  <span>
                    Note {index + 1}
                    {!openNotes.has(index) && <span className="text-xs text-muted-foreground"> · {summary(note)}</span>}
                  </span>
                  <span className="flex gap-1">
                    <button type="button" className="rounded border px-2 text-xs hover:bg-muted" onClick={(event) => { event.preventDefault(); playNote(note); }}>
                      Play
                    </button>
                    {!cue.repeating && (
                      <button type="button" className="rounded px-1 text-xs hover:bg-muted" onClick={(event) => { event.preventDefault(); setDraft({ ...draft, notes: draft.notes.filter((_, i) => i !== index) }); }}>
                        Remove
                      </button>
                    )}
                  </span>
                </summary>
                <div className="pb-2">
                  <NoteEditor note={note} onChange={(next) => updateNote(index, next)} />
                </div>
              </details>
            ) : (
              <NoteEditor key={index} note={note} onChange={(next) => updateNote(index, next)} />
            ),
          )}
          {!cue.repeating && draft.notes.length < 3 && (
            <button type="button" className="rounded border px-2 py-1 text-xs" onClick={() => setDraft({ ...draft, notes: [...draft.notes, { ...(draft.notes.at(-1) ?? newNote) }] })}>
              Add note
            </button>
          )}
          {draft.repeat && (
            <fieldset className="rounded border px-2 pb-2">
              <legend className="px-1 text-xs text-muted-foreground">Repeat</legend>
              <div className="grid grid-cols-2 gap-x-4">
                <Slider label="Lead-in (ms)" fill="after" min={0} max={10000} step={50} value={draft.repeat.leadInMs} onChange={(value) => setDraft({ ...draft, repeat: { ...draft.repeat!, leadInMs: value } })} />
                <Slider label="Interval (ms)" min={100} max={10000} step={10} value={draft.repeat.intervalMs} onChange={(value) => setDraft({ ...draft, repeat: { ...draft.repeat!, intervalMs: value } })} />
              </div>
            </fieldset>
          )}
          <div className="flex items-center gap-2">
            <button type="button" className={`rounded border px-3 py-1.5 text-sm ${auditioning ? "bg-primary/10 text-primary" : ""}`} onClick={toggleAudition}>
              {auditioning ? "Stop" : cue.repeating ? "Audition" : "Play"}
            </button>
            <button type="button" disabled={saving} className="rounded bg-primary px-3 py-1.5 text-sm text-primary-foreground disabled:opacity-50" onClick={() => void save()}>
              Save
            </button>
            <Dialog.Close className="rounded px-3 py-1.5 text-sm hover:bg-muted">Cancel</Dialog.Close>
            {error && <p className="text-xs text-destructive">{error}</p>}
          </div>
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog.Root>
  );
}

// Each cue: whether it sounds, a preview of its configured sound, the
// designer, and a reset; plus the master volume.
type RecentUtterance = {
  id: string;
  at: number;
  threadId: string;
  mimeType: string;
  bytes: number;
  text: string;
  transcriber: string | null;
  segments: string | null;
  detector: { frames: number; speechFrames: number; peakSpeechProbability: number; peakRms: number } | null;
  // Null for uploads stored before delivery was tracked.
  status: "received" | "sending" | "sent" | "empty" | "failed" | null;
  lastError: string | null;
};

const recordingStatusLabels = {
  received: "waiting to be transcribed and delivered",
  sending: "being delivered",
  sent: "delivered",
  empty: "nothing to deliver",
  failed: "not delivered",
};

function RecordingStatus({ u }: { u: RecentUtterance }) {
  const [retryError, setRetryError] = useState<string | null>(null);
  if (!u.status) return null;
  const retry = () =>
    rpcCall("retryRecording", { id: u.id })
      .then(() => setRetryError(null))
      .catch((cause) => setRetryError(cause instanceof Error ? cause.message : String(cause)));
  return (
    <div className="flex flex-wrap items-center gap-x-2 gap-y-1">
      <span className={u.status === "failed" ? "text-destructive" : "text-muted-foreground"}>{recordingStatusLabels[u.status]}</span>
      {u.status !== "sent" && u.status !== "empty" && u.lastError && <span className="text-muted-foreground">({u.lastError})</span>}
      {u.status === "failed" && (
        <>
          <button type="button" className="rounded border px-2 text-xs hover:bg-muted" onClick={() => void retry()}>
            Retry
          </button>
          <span className="text-muted-foreground">If you already said it again, retrying sends it twice. It fails again until whatever stopped it is fixed.</span>
        </>
      )}
      {retryError && <span className="text-destructive">{retryError}</span>}
    </div>
  );
}

function UtteranceCard({ u }: { u: RecentUtterance }) {
  return (
    <div className="space-y-1 rounded border p-2 text-xs">
      <div className="flex flex-wrap items-center gap-x-3 gap-y-1 text-muted-foreground">
        <span>{new Date(u.at).toLocaleString()}</span>
        <span>{u.threadId || "new thread"}</span>
        <span>{Math.round(u.bytes / 1024)} KB {u.mimeType}</span>
        {u.transcriber && <span>transcribed by {u.transcriber}</span>}
      </div>
      <RecordingStatus u={u} />
      <audio controls preload="none" className="h-8 w-full" src={`/api/v1/plugins/converse/http/recent-utterance?id=${encodeURIComponent(u.id)}`} />
      <p className="break-words">{u.text || <span className="text-muted-foreground">(nothing transcribed)</span>}</p>
      {u.detector && (
        <p className="font-mono text-muted-foreground">
          detector: {u.detector.speechFrames}/{u.detector.frames} speech frames ({(u.detector.speechFrames * 0.032).toFixed(2)}s), peak{" "}
          {u.detector.peakSpeechProbability.toFixed(2)}, loudness {u.detector.peakRms.toFixed(3)}
        </p>
      )}
      {u.segments && <p className="break-all font-mono text-muted-foreground">transcriber: {u.segments}</p>}
    </div>
  );
}

// Opened from a heard message's "Recording" action: that message's upload.
function UtterancePanel({ params }: { threadId: string; params: unknown }) {
  const id = params && typeof params === "object" && "id" in params ? String((params as { id: unknown }).id) : null;
  const [utterance, setUtterance] = useState<RecentUtterance | null | undefined>(undefined);
  useEffect(() => {
    if (!id) {
      setUtterance(null);
      return;
    }
    rpcCall("utterance", { id })
      .then((result) => setUtterance((result as { utterance: RecentUtterance | null }).utterance))
      .catch(() => setUtterance(null));
  }, [id]);
  if (utterance === undefined) return <div className="text-xs text-muted-foreground">Loading…</div>;
  if (!utterance)
    return (
      <p className="text-xs text-muted-foreground">
        No recording is kept for this message. Uploads are kept for a week; a message typed or sent before that has none.
      </p>
    );
  return <UtteranceCard u={utterance} />;
}

// The last uploads with what was heard in them: play one back to tell
// whether missing or invented words were in the audio or in the transcript.
function RecentUtterancesSection() {
  const [utterances, setUtterances] = useState<RecentUtterance[] | null>(null);
  const [error, setError] = useState<string | null>(null);
  const load = () =>
    rpcCall("recentUtterances", {})
      .then((result) => setUtterances((result as { utterances: RecentUtterance[] }).utterances))
      .catch((cause) => setError(cause instanceof Error ? cause.message : String(cause)));
  useEffect(() => {
    void load();
  }, []);
  useRealtime("converse-utterances", () => void load());
  if (error) return <p className="text-xs text-destructive">{error}</p>;
  if (!utterances) return <div className="text-xs text-muted-foreground">Loading…</div>;
  if (utterances.length === 0) return <div className="text-xs text-muted-foreground">Nothing uploaded in the last week.</div>;
  return (
    <div className="space-y-2">
      {utterances.map((u) => (
        <UtteranceCard key={u.id} u={u} />
      ))}
    </div>
  );
}

function SoundCueSettingsSection() {
  const ctl = getController();
  const { state, setState, error, setError } = useSoundCueSettings();
  const [designing, setDesigning] = useState<string | null>(null);
  const volumeSave = useRef<ReturnType<typeof setTimeout> | null>(null);

  const save = async (next: SoundCueSettings) => {
    setState(next);
    setError(null);
    try {
      setState((await rpcCall("updateSoundCueSettings", next)) as SoundCueSettings);
    } catch (cause) {
      setError(cause instanceof Error ? cause.message : String(cause));
      throw cause;
    }
  };
  const saveCue = (key: string, config: SoundCueConfig) =>
    state ? save({ ...state, cues: { ...state.cues, [key]: config } }) : Promise.resolve();
  const reset = async (key: string) => {
    setError(null);
    try {
      setState((await rpcCall("resetSoundCue", { key })) as SoundCueSettings);
    } catch (cause) {
      setError(cause instanceof Error ? cause.message : String(cause));
    }
  };

  if (!state) return <div className="text-xs text-muted-foreground">{error ?? "Loading…"}</div>;
  const designingCue = ctl.soundCues.find((cue) => cue.key === designing);
  return (
    <div className="space-y-3">
      <Slider
        label="Volume"
        min={0}
        max={1}
        step={0.01}
        value={state.volume}
        onChange={(volume) => {
          const next = { ...state, volume };
          setState(next);
          if (volumeSave.current) clearTimeout(volumeSave.current);
          volumeSave.current = setTimeout(() => void save(next).catch(() => undefined), 300);
        }}
      />
      <div className="grid gap-1 sm:grid-cols-2">
        {ctl.soundCues.map((cue) => {
          const config = state.cues[cue.key];
          if (!config) return null;
          return (
            <div key={cue.key} className="flex items-center justify-between gap-2 rounded border px-2 py-1 text-sm">
              <label className="flex min-w-0 items-center gap-2">
                <input
                  type="checkbox"
                  checked={config.enabled}
                  onChange={(event) => void saveCue(cue.key, { ...config, enabled: event.target.checked }).catch(() => undefined)}
                />
                <span className="truncate">{cue.label}</span>
              </label>
              <span className="flex shrink-0 gap-1">
                <button type="button" className="rounded px-2 py-1 text-xs hover:bg-muted" onClick={() => ctl.previewCue(cue.key)}>
                  Preview
                </button>
                <button type="button" className="rounded px-2 py-1 text-xs hover:bg-muted" onClick={() => setDesigning(cue.key)}>
                  Design
                </button>
                <button
                  type="button"
                  className="rounded px-2 py-1 text-xs hover:bg-muted disabled:opacity-50"
                  disabled={config.isDefault}
                  onClick={() => void reset(cue.key)}
                >
                  Reset
                </button>
              </span>
            </div>
          );
        })}
      </div>
      {error && <p className="text-xs text-destructive">{error}</p>}
      {designingCue && state.cues[designingCue.key] && (
        <SoundCueDesigner
          key={designingCue.key}
          cue={designingCue}
          config={state.cues[designingCue.key]}
          onSave={async (config) => {
            await saveCue(designingCue.key, config);
            setDesigning(null);
          }}
          onClose={() => setDesigning(null)}
        />
      )}
    </div>
  );
}

type ThreadVoices = {
  enabled: boolean;
  provider: string;
  voices: VoiceChoice[];
  /** The voice assigned to each requested thread that has one. */
  assigned: Record<string, string>;
};

// The voice catalog and the assignments of the threads whose menus are on
// screen. Each change of those threads, or of any voice setting, reloads them
// in one request; a response overtaken by a newer request is dropped.
function useThreadVoices({ threadIds }: { threadIds: readonly string[] }): ThreadVoices | undefined {
  const [voices, setVoices] = useState<ThreadVoices>();
  const [changes, setChanges] = useState(0);
  const latest = useRef(0);
  useRealtime("converse-voices", () => setChanges((n) => n + 1));
  useEffect(() => {
    const request = ++latest.current;
    void rpcCall("threadVoices", { threadIds }).then((result) => {
      if (request === latest.current) setVoices(result as ThreadVoices);
    });
  }, [threadIds, changes]);
  return voices;
}

const UnassignedVoice = "unassigned";

function voiceLabel(voices: VoiceChoice[], id: string): string {
  return voices.find((voice) => voice.id === id)?.label ?? id.replaceAll("_", " ");
}

export default definePluginApp((app) => {
  app.slots.settingsSection({
    id: "tts-provider",
    title: "Text-to-speech providers",
    description: "Configure every provider, its default voice, and the voices available across threads.",
    component: TtsProviderSettingsSection,
  });
  app.slots.settingsSection({
    id: "sound-cues",
    title: "Sound cues",
    description: "Tones that mark what the voice session is doing.",
    component: SoundCueSettingsSection,
  });
  app.slots.settingsSection({
    id: "recent-utterances",
    title: "Recent utterances",
    description: "A week of uploads, with their transcripts and the transcriber's per-segment silence and confidence figures.",
    component: RecentUtterancesSection,
  });
  app.slots.experimental_threadAction<ThreadVoices | undefined>({
    id: "thread-voice",
    title: "Voice",
    icon: "AudioLines",
    group: experimental_THREAD_ACTION_GROUPS.settings,
    useData: useThreadVoices,
    item({ thread, data }) {
      if (!data) return null;
      const current = data.assigned[thread.id] ?? null;
      const offered = data.voices.filter((voice) => voice.enabled || voice.id === current);
      return {
        label: "Voice",
        icon: "AudioLines",
        detail: current ? voiceLabel(data.voices, current) : "Not assigned",
        choices: {
          heading: "Voice for this thread",
          hint: data.enabled ? undefined : "Different voices for different threads are turned off in settings.",
          items: [
            { id: UnassignedVoice, label: "Not assigned", selected: current === null },
            ...offered.map((voice) => ({
              id: voice.id,
              label: voiceLabel(data.voices, voice.id),
              selected: voice.id === current,
            })),
          ],
        },
        async run({ value }) {
          if (value === undefined) return;
          await rpcCall("setThreadVoice", { threadId: thread.id, voice: value === UnassignedVoice ? "" : value });
        },
      };
    },
  });
  app.contentScripts.register({
    id: "voice-widget",
    mount({ experimental_setThreadRowStatus }) {
      const ctl = getController();
      const unmountWidget = ctl.mountWidget();
      ctl.resumeInterruptedSession();
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
          if (snap.on && snap.threadId)
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
        controller?.teardown();
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
  app.composer.customize({
    id: "new-thread-voice",
    scopes: ["new-thread"],
    banners: [{ id: "new-thread-voice", chrome: "bare", component: NewThreadVoiceBanner }],
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
      if (ctl.getSnapshot().on) ctl.stop();
      else ctl.startViewed();
    },
  });
  app.slots.threadPanelAction({
    id: "utterance",
    title: "Voice recording",
    icon: "Mic",
    component: UtterancePanel,
  });
  app.slots.messageAction({
    id: "recording",
    title: "Recording and transcription details",
    icon: "AudioLines",
    run: async ({ threadId, message, openPanel }) => {
      const result = (await rpcCall("utteranceForMessage", { threadId, text: message.text })) as { utterance: RecentUtterance | null };
      openPanel({ actionId: "utterance", title: "Voice recording", params: result.utterance ? { id: result.utterance.id } : {} });
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
