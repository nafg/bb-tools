// Frontend entry. The server resolves labels (own, inherited, lent) and
// attention time; this file only lets the user set labels and draws the
// result: a header picker, menu actions, sidebar row glyphs, and the Goals
// page with its nested-box map.
import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";
import * as Popover from "@radix-ui/react-popover";
import {
  definePluginApp,
  experimental_THREAD_ACTION_GROUPS,
  experimental_ThreadStatusGlyph as ThreadStatusGlyph,
  experimental_useSidebarThreads,
  ThreadChat,
  ThreadTitle,
  useBbNavigate,
  useRealtime,
  useRpc,
  type PluginNavPanelProps,
  type PluginRpcMethodContract,
  type PluginSidebarThread,
  type PluginThreadActionRegistration,
  type StandardSchemaV1,
} from "@get-bb/plugin-sdk/app";
import { layoutOverview, type LayoutCell } from "./app-core.cjs";

// ---- the rpc contract as the server implements it (Plugin.scala) ----

type Shape<T> = StandardSchemaV1<T, T>;
type GoalLetter = "B" | "C" | "P" | "S" | "M" | "F";
type MyMove = "quick" | "focus" | "later";
interface OwnLabels {
  goal: GoalLetter | null;
  value: number | null;
  blockedBy: string[];
  /** What the user still owes the thread; null when nothing. Clears when they next write to it. */
  myMove: MyMove | null;
  myMoveAt: number | null;
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
  myMove?: string;
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
  /** Archived in bb, but still active in the window (changed, attended, or marked). */
  archived: boolean;
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
// Hues are categorical slots 1 to 5 and 7 of the dataviz skill's palette, stepped separately for light and dark surfaces, and passed its
// validator in both modes. In light mode aqua, yellow, and magenta fall under 3:1 contrast, which the visible text labels and the table cover.
// Rerun the validator before changing any hue.
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
const EMPTY_OWN: OwnLabels = { goal: null, value: null, blockedBy: [], myMove: null, myMoveAt: null };

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

const MY_MOVES: readonly { id: MyMove; label: string; title: string }[] = [
  { id: "quick", label: "Quick", title: "A question or decision that takes little effort" },
  { id: "focus", label: "Focus", title: "Needs real focused time; reserve space for it" },
  { id: "later", label: "Later", title: "Nothing owed now, but more to prompt later" },
];

/** The "my move" buttons for one thread. One click sets; the same click again clears. */
function MyMoveControl({ threadId, own, onError }: { threadId: string; own: OwnLabels; onError: (message: string | null) => void }) {
  const set = (value: MyMove | "none") => {
    onError(null);
    setLabels({ threadId, myMove: value }).catch((cause: unknown) => onError(cause instanceof Error ? cause.message : String(cause)));
  };
  return (
    <div className="flex items-center gap-1">
      <span className="mr-1 text-xs text-muted-foreground">My move</span>
      {MY_MOVES.map((m) => (
        <ChoiceButton key={m.id} title={m.title} selected={own.myMove === m.id} onClick={() => set(own.myMove === m.id ? "none" : m.id)}>
          {m.label}
        </ChoiceButton>
      ))}
    </div>
  );
}

/** The controls for one thread's labels: goal, value, my move, and what it waits on. Used inline in the Goals pane and inside the
 * thread-header popover. */
function LabelEditor({ threadId }: { threadId: string }) {
  const { rows, error } = useLabelState();
  const dark = useDarkMode();
  const row = rows.get(threadId) ?? null;
  const effective = row?.effective ?? null;
  const own = row?.own ?? EMPTY_OWN;
  const [writeError, setWriteError] = useState<string | null>(null);
  const [blockerInput, setBlockerInput] = useState("");
  const write = (input: Omit<LabelsSetInput, "threadId">) => {
    setWriteError(null);
    setLabels({ threadId, ...input }).catch((cause: unknown) => setWriteError(cause instanceof Error ? cause.message : String(cause)));
  };
  const inherited = effective?.source.kind === "inherited";
  return (
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
      <MyMoveControl threadId={threadId} own={own} onError={setWriteError} />
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
  );
}

/** The thread header's control: the current label as a pill, the editor in a popover. */
function GoalHeaderPicker({ threadId }: { threadId: string }) {
  const { rows } = useLabelState();
  const dark = useDarkMode();
  const effective = rows.get(threadId)?.effective ?? null;
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
          <LabelEditor threadId={threadId} />
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

/** The label as a map box shows it: the enclosing goal box already names the goal, and where the label came from lives in the tooltip,
 * so only the value digit remains. */
const boxLabel = (label: EffectiveLabel | null): string => (label ? `${label.value ?? "?"}` : "–");

/** What to work on next: the most important first (1 before 3, lent labels counted, unranked last), then the most attended, then by title. */
const byImportance = (a: OverviewThread, b: OverviewThread): number =>
  (a.importance ?? Number.POSITIVE_INFINITY) - (b.importance ?? Number.POSITIVE_INFINITY) ||
  b.seconds - a.seconds ||
  a.title.localeCompare(b.title);

const PANEL_PATH = "goals";

/** One slice of the donut: a goal (or unlabeled) with its share of the day's attention. */
const arcPath = (cx: number, cy: number, r0: number, r1: number, a0: number, a1: number): string => {
  const p = (r: number, a: number) => [cx + r * Math.sin(a), cy - r * Math.cos(a)] as const;
  const [x0, y0] = p(r1, a0);
  const [x1, y1] = p(r1, a1);
  const [x2, y2] = p(r0, a1);
  const [x3, y3] = p(r0, a0);
  const large = a1 - a0 > Math.PI ? 1 : 0;
  return `M ${x0} ${y0} A ${r1} ${r1} 0 ${large} 1 ${x1} ${y1} L ${x2} ${y2} A ${r0} ${r0} 0 ${large} 0 ${x3} ${y3} Z`;
};

/** Where attention went in the window: a donut by goal with its legend, and the threads that received it, most first. */
function DayView({
  overview,
  dark,
  selectedId,
  onSelect,
}: {
  overview: Overview;
  dark: boolean;
  selectedId: string | null;
  onSelect: (id: string) => void;
}) {
  const all = flatten(overview.groups.flatMap((g) => g.threads));
  const total = overview.groups.reduce((sum, g) => sum + g.seconds, 0);
  const slices = [...GOALS.map((g) => g.letter), null]
    .map((letter) => ({ letter, seconds: overview.groups.find((g) => g.goal === letter)?.seconds ?? 0 }))
    .filter((s) => s.seconds > 0);
  let angle = 0;
  const arcs = slices.map((s) => {
    const a0 = angle;
    angle += (s.seconds / total) * 2 * Math.PI;
    return { ...s, a0, a1: angle };
  });
  const threads = all.filter((t) => t.seconds > 0).sort((a, b) => b.seconds - a.seconds);
  return (
    <div className="flex min-h-0 flex-1 flex-col gap-3 overflow-auto">
      <div className="flex flex-wrap items-start gap-6">
        <svg width={220} height={220} role="img" aria-label="Attention by goal in the last 24 hours">
          {arcs.map((a) => (
            <path key={a.letter ?? "none"} d={arcPath(110, 110, 62, 104, a.a0, a.a1 - 0.012)} fill={goalColor(a.letter, dark)} />
          ))}
          <text x={110} y={106} textAnchor="middle" fontSize={20} fontWeight={600} fill="currentColor">
            {formatDuration(total)}
          </text>
          <text x={110} y={126} textAnchor="middle" fontSize={11} fill="currentColor" opacity={0.7}>
            {total > 0 ? "of attention" : "nothing recorded"}
          </text>
        </svg>
        <table className="text-xs">
          <caption className="sr-only">Attention by goal</caption>
          <tbody>
            {slices.map((s) => (
              <tr key={s.letter ?? "none"}>
                <td className="py-0.5 pr-3">
                  <span className="mr-1 inline-block h-2 w-2 rounded-sm align-middle" style={{ background: goalColor(s.letter, dark) }} />
                  {s.letter ? `${s.letter} ${goalInfo(s.letter)?.title}` : "Unlabeled"}
                </td>
                <td className="py-0.5 pr-3 text-right tabular-nums">{Math.round((s.seconds / total) * 100)}%</td>
                <td className="py-0.5 text-right tabular-nums">{formatDuration(s.seconds)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <table className="w-full text-xs">
        <caption className="sr-only">Threads by attention</caption>
        <thead>
          <tr className="text-left text-muted-foreground">
            <th className="py-0.5 pr-2 font-normal">Label</th>
            <th className="py-0.5 pr-2 font-normal">Thread</th>
            <th className="py-0.5 pr-2 font-normal">Project</th>
            <th className="py-0.5 pr-2 text-right font-normal">Attention</th>
            <th className="py-0.5 pr-2 text-right font-normal">Share</th>
          </tr>
        </thead>
        <tbody>
          {threads.map((t) => (
            <tr key={t.id} className={`cursor-pointer hover:bg-accent ${t.id === selectedId ? "bg-accent" : ""}`} onClick={() => onSelect(t.id)}>
              <td className="py-0.5 pr-2 tabular-nums">
                <span className="mr-1 inline-block h-2 w-2 rounded-sm align-middle" style={{ background: goalColor(t.label?.goal, dark) }} />
                {t.label ? shortLabel(t.label) : "–"}
              </td>
              <td className="py-0.5 pr-2">
                {t.title}
                {t.archived && <span className="ml-1 text-muted-foreground">(archived)</span>}
              </td>
              <td className="py-0.5 pr-2 text-muted-foreground">{overview.projects[t.projectId] ?? t.projectId}</td>
              <td className="py-0.5 pr-2 text-right tabular-nums">{formatDuration(t.seconds)}</td>
              <td className="py-0.5 pr-2 text-right tabular-nums">{Math.round((t.seconds / total) * 100)}%</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

/** How much of the page the thread pane takes; remembered per browser. */
const PANE_FRACTION_KEY = "goals.paneFraction";
const DEFAULT_PANE_FRACTION = 0.6;
const clampFraction = (f: number) => Math.min(0.85, Math.max(0.2, f));
function usePaneFraction(): [number, (fraction: number, persist: boolean) => void] {
  const [fraction, setFraction] = useState(() => {
    const stored = Number(window.localStorage.getItem(PANE_FRACTION_KEY));
    return Number.isFinite(stored) && stored > 0 ? clampFraction(stored) : DEFAULT_PANE_FRACTION;
  });
  const update = useCallback((f: number, persist: boolean) => {
    setFraction(f);
    if (persist) window.localStorage.setItem(PANE_FRACTION_KEY, String(f));
  }, []);
  return [fraction, update];
}

/** The order the waiting list and Next walk: the user's quick moves, then focus, then unread replies, then what is only owed later. */
type WaitingGroup = MyMove | "unread";
const WAITING_GROUPS: readonly WaitingGroup[] = ["quick", "focus", "unread", "later"];
const WAITING_GROUP_TITLE: Record<WaitingGroup, string> = { quick: "Quick", focus: "Focus", unread: "Unread replies", later: "Later" };

/** Why a thread is in the waiting list: the user's own mark first, else bb's attention state. */
const reasonFor = (thread: OverviewThread, move: MyMove | null, state: LiveState): string => {
  const parts: string[] = [];
  if (move) parts.push(`${move} (your mark)`);
  if (state === "needs-input") parts.push("needs input");
  else if (state === "failed") parts.push("failed, unread");
  else if (state === "waiting") parts.push("unread reply");
  else if (state === "working") parts.push("working");
  return parts.join(", ") || "idle";
};

/**
 * What a thread is doing right now, from bb's live sidebar view. "needs-input",
 * "failed", and "waiting" are the states bb paints its attention dot for
 * (indicator waiting-for-input, unread-error, unread-success), which is what
 * "waiting on you" means here; "working" is a running agent.
 */
type LiveState = "working" | "needs-input" | "failed" | "waiting" | "idle";

const liveStateOf = (t: PluginSidebarThread | undefined): LiveState => {
  if (!t) return "idle";
  if (t.hasPendingInteraction || t.indicator === "waiting-for-input") return "needs-input";
  if (t.status === "active" || t.status === "starting" || t.status === "stopping" || t.indicator === "runtime") return "working";
  if (t.indicator === "unread-error" || (t.status === "error" && t.isUnread)) return "failed";
  if (t.indicator === "unread-success" || t.indicator === "queued-failed" || t.isUnread) return "waiting";
  return "idle";
};
const isWaitingOnYou = (state: LiveState): boolean => state === "needs-input" || state === "failed" || state === "waiting";
const LIVE_STATE_TEXT: Record<LiveState, string> = {
  working: "Working",
  "needs-input": "Needs your input",
  failed: "Failed, unread",
  waiting: "Waiting on you",
  idle: "Idle",
};
const FAILED_COLOR = "#e34948";

/** A small marker in a map box's corner: a pulsing ring while working, a solid dot while waiting on you. */
function LiveMarker({ state, x, y }: { state: LiveState; x: number; y: number }) {
  if (state === "idle") return null;
  if (state === "working")
    return (
      <circle cx={x} cy={y} r={3.5} fill="none" stroke="currentColor" strokeWidth={1.5}>
        <animate attributeName="r" values="2.5;4.5;2.5" dur="1.4s" repeatCount="indefinite" />
        <animate attributeName="stroke-opacity" values="1;0.4;1" dur="1.4s" repeatCount="indefinite" />
      </circle>
    );
  return <circle cx={x} cy={y} r={5} fill={state === "failed" ? FAILED_COLOR : "currentColor"} stroke="white" strokeWidth={2} />;
}

/** The Goals page: bars and the map on the left, and, once a thread is picked, that thread's chat on the right. The picked thread lives
 * in the route's subPath, so a reload keeps it. The page hosts its own `ThreadChat` because bb gives plugins no way to replace or focus a
 * given pane: `toThread` with `split` keeps adding panes and only replaces at the pane cap, and plugins cannot run the pane commands. */
function GoalsPage({ subPath }: PluginNavPanelProps) {
  const rpc = useRpc<GoalsRpc>();
  const navigate = useBbNavigate();
  const selectedId = /^thr_[a-z0-9]+$/.test(subPath) ? subPath : null;
  const select = useCallback((id: string | null) => navigate.toPluginPanel(PANEL_PATH, { subPath: id ?? "", replace: true }), [navigate]);
  const dark = useDarkMode();
  const { version } = useLabelState();
  const [window_, setWindow] = useState<Window>("7d");
  const [view, setView] = useState<"map" | "day">("map");
  // The day view follows the recorder's live timer, so it refetches once a minute.
  const [tick, setTick] = useState(0);
  useEffect(() => {
    if (view !== "day") return;
    const id = window.setInterval(() => setTick((n) => n + 1), 60_000);
    return () => window.clearInterval(id);
  }, [view]);
  const [overview, setOverview] = useState<Overview | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [hover, setHover] = useState<{ x: number; y: number; thread: OverviewThread } | null>(null);
  const [mapRef, mapSize] = useElementSize<HTMLDivElement>();
  const [pageRef, pageSize] = useElementSize<HTMLDivElement>();
  const [paneFraction, setPaneFraction] = usePaneFraction();
  const sidebar = experimental_useSidebarThreads();
  const live = useMemo(() => new Map(sidebar.threads.map((t) => [t.id, t])), [sidebar.threads]);

  useEffect(() => {
    let cancelled = false;
    const to = Date.now();
    const from = to - (view === "day" ? 86_400_000 : (WINDOWS.find((w) => w.id === window_)?.ms ?? 0));
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
  }, [rpc, window_, version, view, tick]);

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

  // Fill strength is importance, lending included: the same number the table ranks by.
  const alphaFor = (importance: number | null): number => (importance === 1 ? 0.85 : importance === 2 ? 0.6 : importance === 3 ? 0.4 : 0.22);

  const selected = selectedId ? threadsById.get(selectedId) ?? null : null;

  // Threads the user owes something: their own "my move" marks and bb's attention states, grouped quick, focus, unread, later, each
  // most important first, then longest waiting.
  const { rows: labelRows } = useLabelState();
  const waiting = useMemo(() => {
    const groupOf = (t: OverviewThread): WaitingGroup | null => {
      const move = labelRows.get(t.id)?.own.myMove ?? null;
      if (move) return move;
      const state = liveStateOf(live.get(t.id));
      if (state === "needs-input") return "quick";
      if (state === "failed" || state === "waiting") return "unread";
      return null;
    };
    const waitedSince = (t: OverviewThread) => {
      const move = labelRows.get(t.id)?.own;
      return move?.myMove && move.myMoveAt ? move.myMoveAt : (live.get(t.id)?.latestAttentionAt ?? 0);
    };
    return [...threadsById.values()]
      .flatMap((t) => {
        const group = groupOf(t);
        return group ? [{ thread: t, group, reason: reasonFor(t, labelRows.get(t.id)?.own.myMove ?? null, liveStateOf(live.get(t.id))), since: waitedSince(t) }] : [];
      })
      .sort(
        (a, b) =>
          WAITING_GROUPS.indexOf(a.group) - WAITING_GROUPS.indexOf(b.group) ||
          (a.thread.importance ?? Number.POSITIVE_INFINITY) - (b.thread.importance ?? Number.POSITIVE_INFINITY) ||
          a.since - b.since ||
          a.thread.title.localeCompare(b.thread.title),
      );
  }, [threadsById, live, labelRows]);
  const selectNext = useCallback(() => {
    if (waiting.length === 0) return;
    const at = waiting.findIndex((w) => w.thread.id === selectedId);
    const next = waiting[(at + 1) % waiting.length];
    if (next) select(next.thread.id);
  }, [waiting, selectedId, select]);
  useEffect(() => {
    // "n" picks the next waiting thread unless the user is typing somewhere.
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== "n" || event.metaKey || event.ctrlKey || event.altKey) return;
      const target = event.target as HTMLElement | null;
      if (target && (target.closest("input, textarea, select, [contenteditable]") || target.isContentEditable)) return;
      event.preventDefault();
      selectNext();
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [selectNext]);
  const now = Date.now();
  const startResize = (event: React.PointerEvent) => {
    event.preventDefault();
    const page = pageRef.current;
    if (!page) return;
    const rect = page.getBoundingClientRect();
    const move = (e: PointerEvent) => setPaneFraction(clampFraction((rect.right - e.clientX) / rect.width), false);
    const stop = (e: PointerEvent) => {
      setPaneFraction(clampFraction((rect.right - e.clientX) / rect.width), true);
      window.removeEventListener("pointermove", move);
      window.removeEventListener("pointerup", stop);
    };
    window.addEventListener("pointermove", move);
    window.addEventListener("pointerup", stop);
  };
  return (
    <div ref={pageRef} className="flex h-full min-h-0 flex-row">
    <div className="flex h-full min-h-0 min-w-0 flex-1 flex-col gap-3 p-4">
      <div className="flex flex-wrap items-center gap-2">
        <div role="group" aria-label="View" className="flex gap-1">
          <ChoiceButton selected={view === "map"} onClick={() => setView("map")}>
            Map
          </ChoiceButton>
          <ChoiceButton selected={view === "day"} onClick={() => setView("day")}>
            Last 24 hours
          </ChoiceButton>
        </div>
        {view === "map" && (
          <div role="group" aria-label="Time window" className="flex gap-1">
            {WINDOWS.map((w) => (
              <ChoiceButton key={w.id} selected={window_ === w.id} onClick={() => setWindow(w.id)}>
                {w.label}
              </ChoiceButton>
            ))}
          </div>
        )}
        <span className="text-xs text-muted-foreground">
          {view === "map"
            ? "Threads active in the window, archived ones included. Box area is attention time, parallel threads each counted in full; darker fill is a bigger lever; a dot means your turn; a ring means the open thread."
            : "Where your attention went in the last 24 hours, by goal and by thread; parallel threads each counted in full. Refreshes every minute."}
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

      {overview && view === "day" && (
        <DayView overview={overview} dark={dark} selectedId={selectedId} onSelect={select} />
      )}

      {overview && view === "map" && (
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

      {view === "map" && (
      <details open className="rounded border text-xs">
        <summary className="flex cursor-pointer items-center gap-2 px-3 py-1.5">
          <span className="font-medium">Waiting on you ({waiting.length})</span>
          <span className="text-muted-foreground">your marks and unread replies; quick, focus, unread, later; most important first, then longest waiting</span>
          <span className="flex-1" />
          <button
            type="button"
            className={buttonClass}
            disabled={waiting.length === 0}
            title="Select the next waiting thread (n)"
            onClick={(event) => {
              event.preventDefault();
              selectNext();
            }}
          >
            Next
          </button>
        </summary>
        {waiting.length > 0 && (
          <ul className="max-h-48 overflow-auto border-t">
            {waiting.map((w, i) => {
              const row = live.get(w.thread.id);
              const first = i === 0 || waiting[i - 1]!.group !== w.group;
              return (
                <li key={w.thread.id}>
                  {first && <div className="bg-muted/50 px-3 py-0.5 font-medium text-muted-foreground">{WAITING_GROUP_TITLE[w.group]}</div>}
                  <div
                    className={`flex cursor-pointer items-center gap-2 px-3 py-1 hover:bg-accent ${w.thread.id === selectedId ? "bg-accent" : ""}`}
                    onClick={() => select(w.thread.id)}
                  >
                    {row && <ThreadStatusGlyph indicator={row.indicator} size="compact" />}
                    <span className="w-8 tabular-nums text-muted-foreground">{w.thread.label ? shortLabel(w.thread.label) : "–"}</span>
                    <span className="min-w-0 flex-1 truncate">{w.thread.title}</span>
                    <span className="w-32 truncate text-muted-foreground">{overview?.projects[w.thread.projectId] ?? w.thread.projectId}</span>
                    <span className="text-muted-foreground">{w.reason}</span>
                    <span className="w-14 text-right tabular-nums text-muted-foreground">{w.since ? formatDuration((now - w.since) / 1000) : ""}</span>
                  </div>
                </li>
              );
            })}
          </ul>
        )}
      </details>
      )}

      {view === "map" && (
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
              const label = boxLabel(thread.label);
              const project = overview?.projects[thread.projectId] ?? thread.projectId;
              const lentBest = thread.lent.length > 0 ? thread.lent.reduce((a, b) => ((a.value ?? 2) <= (b.value ?? 2) ? a : b)) : null;
              const state = liveStateOf(live.get(thread.id));
              const move = labelRows.get(thread.id)?.own.myMove ?? null;
              // The dot means "your turn": bb's attention states, or a quick or focus mark you set yourself.
              const marker: LiveState = state === "idle" && (move === "quick" || move === "focus") ? "waiting" : state;
              return (
                <g
                  key={`t${i}`}
                  style={{ cursor: "pointer" }}
                  onClick={() => select(thread.id)}
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
                    fillOpacity={alphaFor(thread.importance)}
                    stroke={thread.id === selectedId ? "currentColor" : color}
                    strokeWidth={thread.id === selectedId ? 3 : 1}
                    rx={3}
                  />
                  {cell.w > 16 && cell.h > 16 && <LiveMarker state={marker} x={cell.x + cell.w - 7} y={cell.y + 7} />}
                  {cell.w > 48 && cell.h > 14 && (
                    <text x={cell.x + 4} y={cell.y + 11} fontSize={10} fill="currentColor">
                      {label && <tspan fontWeight={600}>{label}</tspan>}
                      <tspan dx={4}>{thread.title.slice(0, Math.max(0, Math.floor((cell.w - 30) / 6)))}</tspan>
                    </text>
                  )}
                  {cell.w > 48 && cell.h > 28 && (
                    <text x={cell.x + 4} y={cell.y + 23} fontSize={9} fill="currentColor" opacity={0.75}>
                      <tspan opacity={0.8}>{project.slice(0, Math.max(0, Math.floor((cell.w - 40) / 5)))}</tspan>
                      <tspan dx={5}>{formatDuration(thread.seconds)}</tspan>
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
            <div className="text-muted-foreground">
              {overview?.projects[hover.thread.projectId] ?? hover.thread.projectId}
              {hover.thread.archived ? " · archived" : ""}
            </div>
            <div>{describeLabel(hover.thread.label)}</div>
            <div>
              {LIVE_STATE_TEXT[liveStateOf(live.get(hover.thread.id))]}
              {labelRows.get(hover.thread.id)?.own.myMove ? `; my move: ${labelRows.get(hover.thread.id)!.own.myMove}` : ""}
              {hover.thread.importance !== null ? `; importance ${hover.thread.importance}` : ""}
            </div>
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

      )}

      {overview && view === "map" && (
        <details className="text-xs">
          <summary className="cursor-pointer text-muted-foreground">All threads as a table, most important first</summary>
          <table className="mt-2 w-full">
            <thead>
              <tr className="text-left text-muted-foreground">
                <th className="py-0.5 pr-2 font-normal">State</th>
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
                  <tr key={t.id} className={`cursor-pointer hover:bg-accent ${t.id === selectedId ? "bg-accent" : ""}`} onClick={() => select(t.id)}>
                    <td className="py-0.5 pr-2">
                      <span className="flex items-center gap-1">
                        {live.get(t.id) && <ThreadStatusGlyph indicator={live.get(t.id)!.indicator} size="compact" />}
                        {LIVE_STATE_TEXT[liveStateOf(live.get(t.id))]}
                      </span>
                    </td>
                    <td className="py-0.5 pr-2 tabular-nums">{t.importance ?? "–"}</td>
                    <td className="py-0.5 pr-2 tabular-nums">{t.label ? shortLabel(t.label) : "–"}</td>
                    <td className="py-0.5 pr-2">
                      {t.title}
                      {t.archived && <span className="ml-1 text-muted-foreground">(archived)</span>}
                    </td>
                    <td className="py-0.5 pr-2 text-muted-foreground">{overview.projects[t.projectId] ?? t.projectId}</td>
                    <td className="py-0.5 pr-2 text-right tabular-nums">{formatDuration(t.seconds)}</td>
                  </tr>
                ))}
            </tbody>
          </table>
        </details>
      )}
    </div>
    {selectedId && (
      <div
        role="separator"
        aria-orientation="vertical"
        aria-label="Resize the thread pane"
        title="Drag to resize"
        className="h-full w-1.5 shrink-0 cursor-col-resize bg-border hover:bg-primary/40"
        onPointerDown={startResize}
      />
    )}
    {selectedId && (
      <aside
        className="flex h-full min-h-0 shrink-0 flex-col"
        style={{ width: pageSize.width > 0 ? Math.round(pageSize.width * paneFraction) : "60%" }}
        aria-label="Selected thread"
      >
        <div className="flex items-center gap-2 border-b px-3 py-2 text-sm">
          <span className="min-w-0 flex-1 truncate font-medium">
            {selected ? selected.title : <ThreadTitle threadId={selectedId} />}
            {selected && (
              <span className="ml-2 font-normal text-muted-foreground">
                {overview?.projects[selected.projectId] ?? selected.projectId}
                {selected.archived ? " · archived" : ""}
              </span>
            )}
          </span>
          <button type="button" className={buttonClass} onClick={() => navigate.toThread(selectedId)}>
            Open
          </button>
          <button type="button" className={buttonClass} aria-label="Close thread pane" onClick={() => select(null)}>
            ×
          </button>
        </div>
        <div className="border-b px-3 py-2 text-sm">
          <LabelEditor threadId={selectedId} />
        </div>
        <ThreadChat key={selectedId} threadId={selectedId} variant="full" layout="contained" className="min-h-0 flex-1" />
      </aside>
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
