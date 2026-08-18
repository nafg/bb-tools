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
  error: string | null;
  /** The last utterance that was transcribed and sent. */
  heard: string | null;
  /** Preview transcription of the utterance still being spoken. */
  interim: string | null;
  /** Current microphone RMS level (normalized 0..1), 0 when inactive. */
  level: number;
  /** The VAD threshold the active session was started with. */
  threshold: number;
}

export interface ConverseStartOptions {
  ttsMode: "browser" | "server";
  vadThreshold: number;
}

export interface ConverseController {
  start(threadId: string, opts: ConverseStartOptions): void;
  stop(): void;
  handleSignal(payload: unknown): void;
  /** Report that a composer for this thread is mounted; returns unregister.
   *  Utterances route to the most recently mounted thread. */
  registerView(threadId: string): () => void;
  subscribe(onChange: () => void): () => void;
  getSnapshot(): ConverseSnapshot;
}

export function createController(deps: {
  rpcCall: (method: string, input: unknown) => Promise<unknown>;
}): ConverseController;
