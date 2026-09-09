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

export interface SoundCueOptions {
  /** Setting keys of the cues that may sound. */
  enabled: string[];
  /** Master gain, 0–1. */
  volume: number;
  /** Per repeating cue, by setting key: silence before the first sound, then the period. */
  repeats: Record<string, { leadInMs: number; intervalMs: number }>;
}

export interface ConverseStartOptions {
  ttsMode: "browser" | "server";
  speechProbabilityThreshold: number;
  endOfSpeechSilenceMs: number;
  soundCues?: SoundCueOptions;
}

export interface ConverseController {
  start(threadId: string, opts: ConverseStartOptions): void;
  /** Start a session targeting the thread currently in view. */
  startViewed(): void;
  stop(): void;
  /** Play one sound cue on demand, ignoring whether it is enabled. */
  previewCue(settingKey: string): void;
  /** Every sound cue, in display order. */
  soundCues: { key: string; label: string }[];
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
