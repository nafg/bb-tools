// Hand-maintained declarations for the Scala.js frontend core (app-core.cjs,
// built from the activity-app Bleep project — see package.json "build").

export interface PaneHandle {
  /** Whether this pane is the focused one in the split layout (true when there is no split). */
  setFocused(focused: boolean): void;
  /** A view span is open for this pane right now. */
  recording(): boolean;
  /** Calls `listener` whenever `recording()` changes; returns the unsubscribe. */
  subscribe(listener: () => void): () => void;
  /** The pane went away; its open span closes now. */
  dispose(): void;
}

export interface ViewTracker {
  /** Follow one pane showing `threadId`. */
  attach(threadId: string): PaneHandle;
  dispose(): void;
}

export function createViewTracker(deps: {
  /** Sends a `reportView` RPC; `keepalive` lets the last report outlive the page. */
  report(input: { id: string; threadId: string; start: number; end: number }, keepalive: boolean): Promise<unknown>;
}): ViewTracker;
