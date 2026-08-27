// Frontend entry: registers a viewer for *.wbs.yaml estimate files. The server
// renders the file to a self-contained HTML document (the same one `bb wbs
// html` prints); this component only fetches it and shows it in an iframe.
import { useEffect, useState } from "react";
import {
  definePluginApp,
  useRpc,
  type PluginFileOpenerProps,
  type PluginFileOpenerSource,
  type PluginRpcMethodContract,
  type StandardSchemaV1,
} from "@get-bb/plugin-sdk/app";

// The RPC contract as the server implements it (Plugin.scala: RenderInput).
// Only the types matter here; the server registers pass-through schemas.
type Shape<T> = StandardSchemaV1<T, T>;
interface RenderInput {
  path: string;
  source: PluginFileOpenerSource;
  depth?: number;
}
interface WbsRpc extends Readonly<Record<string, PluginRpcMethodContract>> {
  render: PluginRpcMethodContract<Shape<RenderInput>, Shape<{ html: string }>>;
}

const depths = ["all", "1", "2", "3", "4"] as const;
type Depth = (typeof depths)[number];

function WbsViewer({ path, source, Original }: PluginFileOpenerProps) {
  const isWbs = /\.wbs\.ya?ml$/i.test(path);
  const rpc = useRpc<WbsRpc>();
  const [depth, setDepth] = useState<Depth>("all");
  const [html, setHtml] = useState<string | null>(null);
  const [generation, setGeneration] = useState(0);
  useEffect(() => {
    if (!isWbs) return;
    let cancelled = false;
    setHtml(null);
    const input: RenderInput = depth === "all" ? { path, source } : { path, source, depth: Number(depth) };
    rpc
      .call("render", input)
      .then((result) => {
        if (!cancelled) setHtml(result.html);
      })
      .catch((error: unknown) => {
        if (!cancelled) setHtml(`<p>${error instanceof Error ? error.message : String(error)}</p>`);
      });
    return () => {
      cancelled = true;
    };
  }, [rpc, path, source, depth, isWbs, generation]);
  if (!isWbs) return <Original />;
  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
      <label style={{ padding: "0.25rem 0.5rem", fontSize: "0.85rem" }}>
        Depth{" "}
        <select value={depth} onChange={(e) => setDepth(e.target.value as Depth)}>
          {depths.map((d) => (
            <option key={d} value={d}>
              {d}
            </option>
          ))}
        </select>{" "}
        <button type="button" onClick={() => setGeneration((g) => g + 1)}>
          Refresh
        </button>
      </label>
      {html === null ? (
        <p style={{ padding: "0.5rem" }}>Loading…</p>
      ) : (
        <iframe title={path} sandbox="" srcDoc={html} style={{ flex: 1, border: 0, width: "100%" }} />
      )}
    </div>
  );
}

export default definePluginApp((app) => {
  app.slots.fileOpener({
    id: "wbs-viewer",
    title: "WBS estimate",
    extensions: ["yaml", "yml"],
    component: WbsViewer,
  });
});
