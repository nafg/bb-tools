// Hand-maintained declarations for the Scala.js frontend core (app-core.cjs,
// built from the converse-app Bleep project — see package.json "build").

export interface ConverseSnapshot {
  phase:
    | "idle"
    | "starting"
    | "listening"
    | "recording"
    | "transcribing"
    | "waiting"
    | "speaking"
    | "error";
  /** The thread utterances currently route to; null when idle. */
  threadId: string | null;
  /** The thread the app route currently shows. */
  viewed: string | null;
  /** The thread whose reply is currently being spoken aloud. */
  speakingThread: string | null;
  /** Human-readable name of the thread currently receiving speech. */
  targetThreadDisplayName: string | null;
  /** Human-readable name of the thread whose reply is being spoken aloud. */
  speakingThreadDisplayName: string | null;
  /** The New thread screen is in view, so speech starts a thread instead. */
  composeSurface: boolean;
  /** Project a spoken new thread would be created in; null when none is selected. */
  newThreadProjectId: string | null;
  newThreadProjectDisplayName: string | null;
  /** Per-thread voice selected for the reply currently being spoken. */
  speakingVoice: string | null;
  error: string | null;
  /** The last utterance that was transcribed and sent. */
  heard: string | null;
  /** Preview transcription of the utterance still being spoken. */
  interim: string | null;
  /** Silero V5 speech probability for the latest microphone frame. */
  speechProbability: number;
  /** The probability at which Silero considers a frame speech. */
  speechProbabilityThreshold: number;
}

/** One synthesized note as the settings store it. */
export interface SoundCueNoteConfig {
  frequencyHz: number;
  durationSec: number;
  attackSec: number;
  gain: number;
  /** Absent: the note decays from the end of its attack. */
  releaseSec?: number;
  harmonic2: number;
  pinkNoise: number;
  /** Absent: no low-pass filter. */
  lowpassHz?: number;
}

export interface SoundCueRepeatConfig {
  leadInMs: number;
  intervalMs: number;
}

export interface SoundCueConfig {
  enabled: boolean;
  notes: SoundCueNoteConfig[];
  /** Present exactly for repeating cues. */
  repeat: SoundCueRepeatConfig | null;
}

/** What the soundCueSettings rpc returns and updateSoundCueSettings accepts. */
export interface SoundCueSettings {
  /** Master gain, 0–1. */
  volume: number;
  /** By setting key. */
  cues: Record<string, SoundCueConfig & { isDefault?: boolean }>;
}

export interface ConverseStartOptions {
  ttsMode: "browser" | "server";
  speechProbabilityThreshold: number;
  endOfSpeechSilenceMs: number;
  soundCues?: SoundCueSettings;
}

export interface ConverseController {
  start(threadId: string, opts: ConverseStartOptions): void;
  /** Start a session targeting the thread currently in view. */
  startViewed(): void;
  stop(): void;
  /** Play one cue's configured sound on demand, ignoring whether it is enabled. */
  previewCue(settingKey: string): void;
  /**
   * Play an unsaved cue configuration exactly as a session would; a repeating
   * cue keeps sounding until stopAudition. Returns why it was rejected, or null.
   */
  auditionCue(settingKey: string, config: SoundCueConfig): string | null;
  stopAudition(): void;
  /** Every sound cue, in display order. */
  soundCues: { key: string; label: string; repeating: boolean }[];
  /** End the session because the plugin's frontend is unloading; the next mount resumes it. */
  teardown(): void;
  /** Bring back the session a previous frontend generation was unloaded under. */
  resumeInterruptedSession(): void;
  /** Store the effective settings used by startViewed. */
  configure(opts: ConverseStartOptions): void;
  /** Speak text without starting or advancing a voice session. */
  replay(text: string, sourceThreadId: string): Promise<void>;
  handleSignal(payload: unknown): void;
  /** Report the thread the app route currently shows; utterances route here. */
  noteViewed(threadId: string): void;
  /** Supply current sidebar names for identifying routing and playback. */
  setThreadDisplayNames(namesByThreadId: Record<string, string>): void;
  setProjectDisplayNames(namesByProjectId: Record<string, string>): void;
  /** Report that the New thread screen is in view with the given project selected. */
  noteComposeSurface(projectId: string | null): void;
  leaveComposeSurface(): void;
  /** Supply the host's thread navigation so widget thread references can open them. */
  setNavigateToThread(navigate: (threadId: string) => void): void;
  /** Render the floating chrome widget; returns a disposer. */
  mountWidget(): () => void;
  subscribe(onChange: () => void): () => void;
  getSnapshot(): ConverseSnapshot;
}

export function createController(deps: {
  rpcCall: (method: string, input: unknown) => Promise<unknown>;
  createMicVad: (options: Record<string, unknown>) => Promise<unknown>;
  encodeWav: (audio: Float32Array) => ArrayBuffer;
}): ConverseController;
