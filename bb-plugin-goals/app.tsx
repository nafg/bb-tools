// Frontend entry. The server resolves labels (own, inherited, lent) and
// attention time; this file only lets the user set labels and draws the
// result: a header picker, menu actions, sidebar row glyphs, and the Goals
// page with its nested-box map.
import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";
import * as Popover from "@radix-ui/react-popover";
import {
  definePluginApp,
  experimental_THREAD_ACTION_GROUPS,
  experimental_useSidebarThreads,
  useBbNavigate,
  useRealtime,
  useRpc,
  type PluginRpcMethodContract,
  type PluginThreadActionRegistration,
  type StandardSchemaV1,
} from "@get-bb/plugin-sdk/app";
import { layoutOverview, type LayoutCell } from "./app-core.cjs";

// ---- the rpc contract as the server implements it (Plugin.scala) ----

type Shape<T> = StandardSchemaV1<T, T>;
type GoalLetter = "B" | "C" | "P" | "S" | "M" | "F";
interface OwnLabels {
  goal: GoalLetter | null;
  value: number | null;
  blockedBy: string[];
}
interface EffectiveLabel {
  goal: GoalLetter;
  value: number | null;
  source: { kind: "own" } | { kind: "inherited"; fromThreadId: string };
}
interface LentLabel {
  fromThreadId: string;
  goal: GoalLetter;
  value: number | null;
}
interface LabelRow {
  threadId: string;
  own: OwnLabels;
  effective: EffectiveLabel | null;
  lent: LentLabel[];
  importance: number | null;
  /** Ids in `own.blockedBy` that are not open threads. */
  closedBlockers: string[];
}
interface LabelsSetInput {
  threadId: string;
  goal?: string;
  value?: string;
  blockedBy?: string[];
}
interface LabelsResult {
  threadId: string;
  own: OwnLabels;
  effective: EffectiveLabel | null;
  text: string;
}
interface OverviewThread {
  id: string;
  title: string;
  projectId: string;
  label: EffectiveLabel | null;
  lent: LentLabel[];
  importance: number | null;
  blockedBy: string[];
  seconds: number;
  children: OverviewThread[];
}
interface OverviewGroup {
  goal: GoalLetter | null;
  title: string;
  seconds: number;
  threads: OverviewThread[];
}
interface Overview {
  from: number;
  to: number;
  recorder: { state: "ok" | "missing" | "error"; message?: string };
  projects: Record<string, string>;
  groups: OverviewGroup[];
}
interface GoalsRpc extends Readonly<Record<string, PluginRpcMethodContract>> {
  rows: PluginRpcMethodContract<Shape<null>, Shape<{ rows: LabelRow[] }>>;
  labels_get: PluginRpcMethodContract<Shape<{ threadId: string }>, Shape<LabelsResult>>;
  labels_set: PluginRpcMethodContract<Shape<LabelsSetInput>, Shape<LabelsResult>>;
  overview: PluginRpcMethodContract<Shape<{ from: number; to: number }>, Shape<Overview>>;
}

// ---- goals: fixed order, fixed hues (validated for light and dark surfaces) ----

interface GoalInfo {
  letter: GoalLetter;
  title: string;
  icon: string;
  light: string;
  dark: string;
}
const GOALS: readonly GoalInfo[] = [
  { letter: "B", title: "Billable client work", icon: "Briefcase", light: "#2a78d6", dark: "#3987e5" },
  { letter: "C", title: "Client work, not billable", icon: "Handshake", light: "#eb6834", dark: "#d95926" },
  { letter: "P", title: "Product or future income", icon: "Rocket", light: "#1baf7a", dark: "#199e70" },
  { letter: "S", title: "Sanity", icon: "HeartPulse", light: "#eda100", dark: "#c98500" },
  { letter: "M", title: "Maintainer obligations", icon: "Wrench", light: "#e87ba4", dark: "#d55181" },
  { letter: "F", title: "Fun", icon: "Gamepad2", light: "#6250d6", dark: "#9085e9" },
];
const goalInfo = (letter: GoalLetter | null | undefined): GoalInfo | null =>
  GOALS.find((g) => g.letter === letter) ?? null;
const UNLABELED = "#8a8a86";

function useDarkMode(): boolean {
  const query = useMemo(() => window.matchMedia("(prefers-color-scheme: dark)"), []);
  const read = useCallback(() => {
    const root = document.documentElement;
    if (root.classList.contains("dark") || root.dataset.theme === "dark") return true;
    if (root.classList.contains("light") || root.dataset.theme === "light") return false;
    return query.matches;
  }, [query]);
  const [dark, setDark] = useState(read);
  useEffect(() => {
    const update = () => setDark(read());
    query.addEventListener("change", update);
    const observer = new MutationObserver(update);
    observer.observe(document.documentElement, { attributes: true, attributeFilter: ["class", "data-theme"] });
    return () => {
      query.removeEventListener("change", update);
      observer.disconnect();
    };
  }, [query, read]);
  return dark;
}

const goalColor = (letter: GoalLetter | null | undefined, dark: boolean): string => {
  const info = goalInfo(letter);
  return info ? (dark ? info.dark : info.light) : UNLABELED;
};

/** "B2", "B" for an unvalued goal, with a trailing * when inherited. */
const shortLabel = (label: EffectiveLabel | null): string =>
  label ? `${label.goal}${label.value ?? ""}${label.source.kind === "inherited" ? "*" : ""}` : "";

const describeLabel = (label: EffectiveLabel | null): string => {
  if (!label) return "No goal";
  const info = goalInfo(label.goal);
  const value = label.value === null ? "unvalued" : `value ${label.value}`;
  const from = label.source.kind === "inherited" ? ", inherited" : "";
  return `${info?.title ?? label.goal}, ${value}${from}`;
};

function formatDuration(seconds: number): string {
  if (seconds < 60) return seconds <= 0 ? "0m" : "<1m";
  const minutes = Math.round(seconds / 60);
  if (minutes < 60) return `${minutes}m`;
  const hours = Math.floor(minutes / 60);
  const rest = minutes % 60;
  return rest === 0 ? `${hours}h` : `${hours}h ${rest}m`;
}

// ---- label rows, shared by every surface in this tab ----

// Plain-fetch rpc so the store exists independently of React.
async function rpcCall<T>(method: string, input: unknown): Promise<T> {
  const response = await fetch(`/api/v1/plugins/goals/rpc/${method}`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(input),
  });
  const envelope = (await response.json()) as { ok: true; result: T } | { ok: false; error?: { message?: string } };
  if (!envelope.ok) throw new Error(envelope.error?.message ?? `rpc ${method} failed`);
  return envelope.result;
}

interface LabelState {
  rows: ReadonlyMap<string, LabelRow>;
  error: string | null;
  /** Bumped on every successful load, so pages that fetch their own data know to refetch. */
  version: number;
}

const labelStore = (() => {
  let state: LabelState = { rows: new Map(), error: null, version: 0 };
  const listeners = new Set<() => void>();
  let inflight: Promise<void> | null = null;
  // A refresh asked for during a fetch (a write landing mid-fetch) must not
  // settle for that fetch's pre-write rows: one more fetch runs after it.
  let again: Promise<void> | null = null;
  const emit = () => listeners.forEach((l) => l());
  const fetchRows = (): Promise<void> =>
    rpcCall<{ rows: LabelRow[] }>("rows", null)
      .then((result) => {
        state = { rows: new Map(result.rows.map((r) => [r.threadId, r])), error: null, version: state.version + 1 };
      })
      .catch((cause: unknown) => {
        state = { ...state, error: cause instanceof Error ? cause.message : String(cause) };
      })
      .finally(emit);
  const refresh = (): Promise<void> => {
    if (inflight) {
      again ??= inflight.then(() => {
        again = null;
        return refresh();
      });
      return again;
    }
    inflight = fetchRows().finally(() => {
      inflight = null;
    });
    return inflight;
  };
  return {
    subscribe(listener: () => void) {
      listeners.add(listener);
      if (state.version === 0 && !inflight) void refresh();
      return () => listeners.delete(listener);
    },
    get: () => state,
    refresh,
  };
})();

function useLabelState(): LabelState {
  return useSyncExternalStore(labelStore.subscribe, labelStore.get);
}

/** Changes a thread's labels through the server's one validating path. */
async function setLabels(input: LabelsSetInput): Promise<LabelsResult> {
  const result = await rpcCall<LabelsResult>("labels_set", input);
  await labelStore.refresh();
  return result;
}

// ---- sidebar row glyphs (bb's own lists draw these; a replaced list may not) ----

type RowStatusSetter = (threadId: string, status: { icon: string; label: string } | null) => void;
let rowStatusSetter: RowStatusSetter | null = null;

/** Mounted app-wide: holds the realtime subscription and paints row glyphs. */
function LabelSync() {
  const { rows } = useLabelState();
  useRealtime("labels-changed", () => void labelStore.refresh());
  const marked = useRef(new Map<string, string>());
  useEffect(() => {
    const setter = rowStatusSetter;
    if (!setter) return;
    const desired = new Map<string, { icon: string; label: string }>();
    for (const row of rows.values()) {
      const info = goalInfo(row.effective?.goal);
      if (row.effective && info)
        desired.set(row.threadId, { icon: info.icon, label: `Goal ${shortLabel(row.effective)}: ${describeLabel(row.effective)}` });
    }
    for (const id of [...marked.current.keys()])
      if (!desired.has(id)) {
        setter(id, null);
        marked.current.delete(id);
      }
    for (const [id, status] of desired) {
      const key = `${status.icon}:${status.label}`;
      if (marked.current.get(id) !== key) {
        setter(id, status);
        marked.current.set(id, key);
      }
    }
  }, [rows]);
  return null;
}

// ---- the header picker ----

const buttonClass = "rounded border px-2 py-0.5 text-xs hover:bg-accent";
const selectedClass = "border-primary bg-primary/15 font-medium";

function ChoiceButton({ selected, onClick, children, title }: { selected: boolean; onClick: () => void; children: React.ReactNode; title?: string }) {
  return (
    <button type="button" title={title} aria-pressed={selected} className={`${buttonClass} ${selected ? selectedClass : ""}`} onClick={onClick}>
      {children}
    </button>
  );
}

function ThreadName({ threadId }: { threadId: string }) {
  const sidebar = experimental_useSidebarThreads();
  const thread = sidebar.threads.find((t) => t.id === threadId);
  return <span title={threadId}>{thread?.displayTitle ?? threadId}</span>;
}

function GoalHeaderPicker({ threadId }: { threadId: string }) {
  const { rows, error } = useLabelState();
  const dark = useDarkMode();
  const row = rows.get(threadId) ?? null;
  const effective = row?.effective ?? null;
  const own = row?.own ?? { goal: null, value: null, blockedBy: [] };
  const [writeError, setWriteError] = useState<string | null>(null);
  const [blockerInput, setBlockerInput] = useState("");
  const write = (input: Omit<LabelsSetInput, "threadId">) => {
    setWriteError(null);
    setLabels({ threadId, ...input }).catch((cause: unknown) => setWriteError(cause instanceof Error ? cause.message : String(cause)));
  };
  const color = goalColor(effective?.goal, dark);
  const text = effective ? shortLabel(effective) : "Goal";
  const inherited = effective?.source.kind === "inherited";
  return (
    <Popover.Root>
      <Popover.Trigger asChild>
        <button
          type="button"
          aria-label={`Goal: ${describeLabel(effective)}`}
          title={describeLabel(effective)}
          className="flex h-7 items-center gap-1 rounded border px-2 text-xs font-medium"
          style={{ borderColor: color, borderStyle: inherited ? "dashed" : "solid", color: effective ? color : undefined }}
        >
          {text}
        </button>
      </Popover.Trigger>
      <Popover.Portal>
        <Popover.Content align="end" sideOffset={6} className="z-50 w-80 rounded-md border bg-popover p-3 text-sm text-popover-foreground shadow-md">
          <div className="space-y-3">
            <div>
              <div className="mb-1 text-xs text-muted-foreground">Goal</div>
              <div className="flex flex-wrap gap-1">
                {GOALS.map((g) => (
                  <ChoiceButton key={g.letter} title={g.title} selected={own.goal === g.letter} onClick={() => write({ goal: g.letter })}>
                    <span className="mr-1 inline-block h-2 w-2 rounded-sm" style={{ background: dark ? g.dark : g.light }} />
                    {g.letter}
                  </ChoiceButton>
                ))}
                <ChoiceButton selected={own.goal === null} onClick={() => write({ goal: "none" })}>
                  None
                </ChoiceButton>
              </div>
              {own.goal ? (
                <div className="mt-1 text-xs text-muted-foreground">{goalInfo(own.goal)?.title}</div>
              ) : inherited && effective ? (
                <div className="mt-1 text-xs text-muted-foreground">
                  Inherited {shortLabel(effective).replace("*", "")} from <ThreadName threadId={effective.source.kind === "inherited" ? effective.source.fromThreadId : ""} />
                </div>
              ) : null}
            </div>
            <div>
              <div className="mb-1 text-xs text-muted-foreground">Value within the goal (1 is the biggest lever)</div>
              <div className="flex gap-1">
                {[1, 2, 3].map((v) => (
                  <ChoiceButton key={v} selected={own.value === v} onClick={() => write({ value: String(v) })}>
                    {v}
                  </ChoiceButton>
                ))}
                <ChoiceButton selected={own.value === null} onClick={() => write({ value: "none" })}>
                  Unvalued
                </ChoiceButton>
              </div>
            </div>
            <div>
              <div className="mb-1 text-xs text-muted-foreground">Waiting on</div>
              <ul className="space-y-1">
                {own.blockedBy.map((id) => (
                  <li key={id} className="flex items-center justify-between gap-2 text-xs">
                    <span>
                      <ThreadName threadId={id} />
                      {row?.closedBlockers.includes(id) && <span className="ml-1 text-muted-foreground">(closed)</span>}
                    </span>
                    <button type="button" className={buttonClass} aria-label={`Stop waiting on ${id}`} onClick={() => write({ blockedBy: own.blockedBy.filter((b) => b !== id) })}>
                      ×
                    </button>
                  </li>
                ))}
              </ul>
              <form
                className="mt-1 flex gap-1"
                onSubmit={(event) => {
                  event.preventDefault();
                  const id = blockerInput.trim();
                  if (!id) return;
                  write({ blockedBy: [...own.blockedBy, id] });
                  setBlockerInput("");
                }}
              >
                <input
                  className="w-full rounded border bg-background px-2 py-0.5 text-xs"
                  placeholder="thr_… this thread waits on"
                  value={blockerInput}
                  onChange={(event) => setBlockerInput(event.target.value)}
                />
                <button type="submit" className={buttonClass}>
                  Add
                </button>
              </form>
            </div>
            {row && row.lent.length > 0 && (
              <div className="text-xs text-muted-foreground">
                Lent by blocked threads:{" "}
                {row.lent.map((l, i) => (
                  <span key={l.fromThreadId}>
                    {i > 0 && ", "}
                    {l.goal}
                    {l.value ?? ""} from <ThreadName threadId={l.fromThreadId} />
                  </span>
                ))}
              </div>
            )}
            {(writeError ?? error) && <div className="text-xs text-destructive">{writeError ?? error}</div>}
          </div>
        </Popover.Content>
      </Popover.Portal>
    </Popover.Root>
  );
}

// ---- menu actions ----

const goalAction: PluginThreadActionRegistration<ReadonlyMap<string, LabelRow>> = {
  id: "goal",
  title: "Goal",
  icon: "Target",
  group: experimental_THREAD_ACTION_GROUPS.organize,
  order: 90,
  useData: () => useLabelState().rows,
  item: ({ thread, data }) => {
    const row = data.get(thread.id);
    const own = row?.own.goal ?? null;
    return {
      label: "Goal",
      detail: describeLabel(row?.effective ?? null),
      icon: "Target",
      choices: {
        heading: "Goal",
        hint: row?.effective?.source.kind === "inherited" ? "Currently inherited from an ancestor." : undefined,
        items: [
          ...GOALS.map((g) => ({ id: g.letter, label: `${g.letter}  ${g.title}`, icon: g.icon, selected: own === g.letter })),
          { id: "none", label: "No own goal", selected: own === null },
        ],
      },
      run: async ({ value }) => {
        if (value) await setLabels({ threadId: thread.id, goal: value });
      },
    };
  },
};

const valueAction: PluginThreadActionRegistration<ReadonlyMap<string, LabelRow>> = {
  id: "goal-value",
  title: "Goal value",
  icon: "Gauge",
  group: experimental_THREAD_ACTION_GROUPS.organize,
  order: 91,
  useData: () => useLabelState().rows,
  item: ({ thread, data }) => {
    const row = data.get(thread.id);
    const own = row?.own.value ?? null;
    return {
      label: "Goal value",
      detail: own === null ? "Unvalued" : `Value ${own}`,
      icon: "Gauge",
      choices: {
        heading: "Value within the goal",
        items: [
          { id: "1", label: "1  Biggest lever", selected: own === 1 },
          { id: "2", label: "2", selected: own === 2 },
          { id: "3", label: "3  Smallest lever", selected: own === 3 },
          { id: "none", label: "Unvalued", selected: own === null },
        ],
      },
      run: async ({ value }) => {
        if (value) await setLabels({ threadId: thread.id, value });
      },
    };
  },
};

// ---- the Goals page ----

type Window = "1d" | "7d" | "30d";
const WINDOWS: readonly { id: Window; label: string; ms: number }[] = [
  { id: "1d", label: "Today", ms: 86_400_000 },
  { id: "7d", label: "7 days", ms: 7 * 86_400_000 },
  { id: "30d", label: "30 days", ms: 30 * 86_400_000 },
];

function useElementSize<T extends HTMLElement>(): [React.RefObject<T | null>, { width: number; height: number }] {
  const ref = useRef<T | null>(null);
  const [size, setSize] = useState({ width: 0, height: 0 });
  useLayoutEffect(() => {
    const element = ref.current;
    if (!element) return;
    const observer = new ResizeObserver(([entry]) => {
      if (!entry) return;
      const { width, height } = entry.contentRect;
      setSize((prev) => (prev.width === width && prev.height === height ? prev : { width, height }));
    });
    observer.observe(element);
    return () => observer.disconnect();
  }, []);
  return [ref, size];
}

const flatten = (threads: OverviewThread[]): OverviewThread[] => threads.flatMap((t) => [t, ...flatten(t.children)]);

/** What to work on next: the most important first (1 before 3, lent labels counted, unranked last), then the most attended, then by title. */
const byImportance = (a: OverviewThread, b: OverviewThread): number =>
  (a.importance ?? Number.POSITIVE_INFINITY) - (b.importance ?? Number.POSITIVE_INFINITY) ||
  b.seconds - a.seconds ||
  a.title.localeCompare(b.title);

function GoalsPage() {
  const rpc = useRpc<GoalsRpc>();
  const navigate = useBbNavigate();
  const dark = useDarkMode();
  const { version } = useLabelState();
  const [window_, setWindow] = useState<Window>("7d");
  const [overview, setOverview] = useState<Overview | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [hover, setHover] = useState<{ x: number; y: number; thread: OverviewThread } | null>(null);
  const [mapRef, mapSize] = useElementSize<HTMLDivElement>();

  useEffect(() => {
    let cancelled = false;
    const to = Date.now();
    const from = to - (WINDOWS.find((w) => w.id === window_)?.ms ?? 0);
    rpc
      .call("overview", { from, to })
      .then((result) => {
        if (!cancelled) {
          setOverview(result);
          setError(null);
        }
      })
      .catch((cause: unknown) => {
        if (!cancelled) setError(cause instanceof Error ? cause.message : String(cause));
      });
    return () => {
      cancelled = true;
    };
  }, [rpc, window_, version]);

  const threadsById = useMemo(() => {
    const map = new Map<string, OverviewThread>();
    if (overview) for (const t of flatten(overview.groups.flatMap((g) => g.threads))) map.set(t.id, t);
    return map;
  }, [overview]);

  const PAD = 4;
  const HEADER = 18;
  const cells = useMemo(
    () => (overview && mapSize.width > 0 && mapSize.height > 0 ? layoutOverview(overview.groups, mapSize.width, mapSize.height, PAD, HEADER) : []),
    [overview, mapSize.width, mapSize.height],
  );
  const threadCells = useMemo(() => {
    const map = new Map<string, LayoutCell>();
    for (const c of cells) if (c.kind === "thread" && c.id) map.set(c.id, c);
    return map;
  }, [cells]);

  const totalSeconds = overview?.groups.reduce((sum, g) => sum + g.seconds, 0) ?? 0;
  const counts = useMemo(() => {
    const map = new Map<GoalLetter | null, number>();
    for (const t of threadsById.values()) map.set(t.label?.goal ?? null, (map.get(t.label?.goal ?? null) ?? 0) + 1);
    return map;
  }, [threadsById]);

  const alphaFor = (label: EffectiveLabel | null): number => (!label ? 0.25 : label.value === 1 ? 0.85 : label.value === 2 ? 0.6 : label.value === 3 ? 0.4 : 0.3);

  return (
    <div className="flex h-full min-h-0 flex-col gap-3 p-4">
      <div className="flex flex-wrap items-center gap-2">
        <div role="group" aria-label="Time window" className="flex gap-1">
          {WINDOWS.map((w) => (
            <ChoiceButton key={w.id} selected={window_ === w.id} onClick={() => setWindow(w.id)}>
              {w.label}
            </ChoiceButton>
          ))}
        </div>
        <span className="text-xs text-muted-foreground">
          Attention time per thread, parallel threads each counted in full. Box area follows it; * marks an inherited label.
        </span>
      </div>
      {overview?.recorder.state === "missing" && (
        <div className="rounded border border-dashed px-3 py-2 text-xs text-muted-foreground">
          Activity recorder is not installed; boxes are unsized and show the tree only.
        </div>
      )}
      {overview?.recorder.state === "error" && (
        <div className="rounded border border-destructive/50 px-3 py-2 text-xs text-destructive">
          Could not read attention time from the activity recorder: {overview.recorder.message}. Boxes are unsized.
        </div>
      )}
      {error && <div className="text-xs text-destructive">{error}</div>}

      {overview && (
        <table className="w-full text-xs">
          <caption className="sr-only">Attention by goal</caption>
          <tbody>
            {[...GOALS.map((g) => g.letter), null].map((letter) => {
              const group = overview.groups.find((g) => g.goal === letter);
              const seconds = group?.seconds ?? 0;
              const share = totalSeconds > 0 ? seconds / totalSeconds : 0;
              const count = counts.get(letter) ?? 0;
              if (!group && count === 0) return null;
              return (
                <tr key={letter ?? "none"}>
                  <td className="w-40 py-0.5 pr-2">
                    <span className="mr-1 inline-block h-2 w-2 rounded-sm align-middle" style={{ background: goalColor(letter, dark) }} />
                    {letter ? `${letter} ${goalInfo(letter)?.title}` : "Unlabeled"}
                  </td>
                  <td className="py-0.5">
                    <div className="h-2 w-full rounded-sm bg-muted">
                      <div className="h-2 rounded-sm" style={{ width: `${Math.round(share * 100)}%`, background: goalColor(letter, dark) }} />
                    </div>
                  </td>
                  <td className="w-16 py-0.5 pl-2 text-right tabular-nums">{Math.round(share * 100)}%</td>
                  <td className="w-20 py-0.5 pl-2 text-right tabular-nums">{formatDuration(seconds)}</td>
                  <td className="w-20 py-0.5 pl-2 text-right tabular-nums text-muted-foreground">
                    {count} thread{count === 1 ? "" : "s"}
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      )}

      <div ref={mapRef} className="relative min-h-[320px] flex-1 overflow-hidden rounded border" onMouseLeave={() => setHover(null)}>
        {cells.length > 0 && (
          <svg width={mapSize.width} height={mapSize.height} role="img" aria-label="Threads as nested boxes, grouped by goal">
            <defs>
              <marker id="goals-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
                <path d="M 0 0 L 10 5 L 0 10 z" fill="currentColor" />
              </marker>
            </defs>
            {cells.map((cell, i) => {
              if (cell.kind === "goal") {
                const letter = (cell.goal ?? null) as GoalLetter | null;
                const color = goalColor(letter, dark);
                const info = goalInfo(letter);
                return (
                  <g key={`g${i}`}>
                    <rect x={cell.x} y={cell.y} width={cell.w} height={cell.h} fill={color} fillOpacity={0.08} stroke={color} strokeWidth={1.5} rx={4} />
                    {cell.w > 40 && (
                      <text x={cell.x + 6} y={cell.y + 13} fontSize={11} fontWeight={600} fill="currentColor">
                        {info ? `${info.letter} ${info.title}` : "Unlabeled"}
                      </text>
                    )}
                  </g>
                );
              }
              const thread = cell.id ? threadsById.get(cell.id) : undefined;
              if (!thread) return null;
              const color = goalColor(thread.label?.goal, dark);
              if (cell.kind === "self") {
                return <rect key={`s${i}`} x={cell.x} y={cell.y} width={cell.w} height={cell.h} fill={color} fillOpacity={0.12} stroke="none" rx={2} />;
              }
              const inherited = thread.label?.source.kind === "inherited";
              const unvalued = thread.label !== null && thread.label.value === null;
              const label = thread.label ? shortLabel(thread.label) : "";
              const lentBest = thread.lent.length > 0 ? thread.lent.reduce((a, b) => ((a.value ?? 2) <= (b.value ?? 2) ? a : b)) : null;
              return (
                <g
                  key={`t${i}`}
                  style={{ cursor: "pointer" }}
                  onClick={() => navigate.toThread(thread.id)}
                  onMouseMove={(event) => {
                    const box = (event.currentTarget.ownerSVGElement as SVGSVGElement).getBoundingClientRect();
                    setHover({ x: event.clientX - box.left, y: event.clientY - box.top, thread });
                  }}
                >
                  <rect
                    x={cell.x}
                    y={cell.y}
                    width={cell.w}
                    height={cell.h}
                    fill={color}
                    fillOpacity={alphaFor(thread.label)}
                    stroke={color}
                    strokeWidth={1}
                    strokeDasharray={inherited ? "3 2" : unvalued ? "1 2" : undefined}
                    rx={3}
                  />
                  {cell.w > 48 && cell.h > 14 && (
                    <text x={cell.x + 4} y={cell.y + 11} fontSize={10} fill="currentColor">
                      <tspan fontWeight={600}>{label || "–"}</tspan>
                      <tspan dx={4}>{thread.title.slice(0, Math.max(0, Math.floor((cell.w - 30) / 6)))}</tspan>
                    </text>
                  )}
                  {cell.w > 48 && cell.h > 28 && (
                    <text x={cell.x + 4} y={cell.y + 23} fontSize={9} fill="currentColor" opacity={0.75}>
                      {formatDuration(thread.seconds)}
                      {lentBest ? `  ← ${lentBest.goal}${lentBest.value ?? ""} lent` : ""}
                    </text>
                  )}
                </g>
              );
            })}
            {[...threadsById.values()].flatMap((blocked) =>
              blocked.blockedBy.flatMap((blockerId) => {
                const a = threadCells.get(blocked.id);
                const b = threadCells.get(blockerId);
                if (!a || !b) return [];
                return [
                  <line
                    key={`${blocked.id}->${blockerId}`}
                    x1={a.x + a.w / 2}
                    y1={a.y + a.h / 2}
                    x2={b.x + b.w / 2}
                    y2={b.y + b.h / 2}
                    stroke="currentColor"
                    strokeWidth={1.5}
                    strokeOpacity={0.7}
                    markerEnd="url(#goals-arrow)"
                    pointerEvents="none"
                  />,
                ];
              }),
            )}
          </svg>
        )}
        {overview && cells.length === 0 && mapSize.width > 0 && (
          <div className="p-3 text-xs text-muted-foreground">No open threads.</div>
        )}
        {hover && (
          <div
            className="pointer-events-none absolute z-10 max-w-xs rounded border bg-popover px-2 py-1 text-xs text-popover-foreground shadow"
            style={{ left: Math.min(hover.x + 12, Math.max(0, mapSize.width - 260)), top: hover.y + 12 }}
          >
            <div className="font-medium">{hover.thread.title}</div>
            <div className="text-muted-foreground">{overview?.projects[hover.thread.projectId] ?? hover.thread.projectId}</div>
            <div>{describeLabel(hover.thread.label)}</div>
            <div>{formatDuration(hover.thread.seconds)} of attention in this window</div>
            {hover.thread.lent.length > 0 && (
              <div>Lent: {hover.thread.lent.map((l) => `${l.goal}${l.value ?? ""} from ${threadsById.get(l.fromThreadId)?.title ?? l.fromThreadId}`).join("; ")}</div>
            )}
            {hover.thread.blockedBy.length > 0 && (
              <div>Waiting on: {hover.thread.blockedBy.map((id) => threadsById.get(id)?.title ?? id).join("; ")}</div>
            )}
          </div>
        )}
      </div>

      {overview && (
        <details className="text-xs">
          <summary className="cursor-pointer text-muted-foreground">All threads as a table, most important first</summary>
          <table className="mt-2 w-full">
            <thead>
              <tr className="text-left text-muted-foreground">
                <th className="py-0.5 pr-2 font-normal">Rank</th>
                <th className="py-0.5 pr-2 font-normal">Label</th>
                <th className="py-0.5 pr-2 font-normal">Thread</th>
                <th className="py-0.5 pr-2 font-normal">Project</th>
                <th className="py-0.5 pr-2 text-right font-normal">Attention</th>
              </tr>
            </thead>
            <tbody>
              {[...threadsById.values()]
                .sort(byImportance)
                .map((t) => (
                  <tr key={t.id} className="cursor-pointer hover:bg-accent" onClick={() => navigate.toThread(t.id)}>
                    <td className="py-0.5 pr-2 tabular-nums">{t.importance ?? "–"}</td>
                    <td className="py-0.5 pr-2 tabular-nums">{t.label ? shortLabel(t.label) : "–"}</td>
                    <td className="py-0.5 pr-2">{t.title}</td>
                    <td className="py-0.5 pr-2 text-muted-foreground">{overview.projects[t.projectId] ?? t.projectId}</td>
                    <td className="py-0.5 pr-2 text-right tabular-nums">{formatDuration(t.seconds)}</td>
                  </tr>
                ))}
            </tbody>
          </table>
        </details>
      )}
    </div>
  );
}

export default definePluginApp((app) => {
  app.slots.experimental_threadHeaderAction({
    id: "goal",
    title: "Goal",
    component: GoalHeaderPicker,
  });
  app.slots.experimental_threadAction(goalAction);
  app.slots.experimental_threadAction(valueAction);
  app.slots.experimental_appOverlay({ id: "label-sync", component: LabelSync });
  app.contentScripts.register({
    id: "row-status",
    mount({ experimental_setThreadRowStatus }) {
      rowStatusSetter = experimental_setThreadRowStatus ?? null;
      return () => {
        rowStatusSetter = null;
      };
    },
  });
  app.slots.navPanel({
    id: "goals",
    title: "Goals",
    icon: "Target",
    path: "goals",
    component: GoalsPage,
  });
});
