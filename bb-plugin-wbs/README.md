# bb-plugin-wbs

Viewer for work-breakdown estimate files (`docs/proposals/<name>.wbs.yaml`;
format in `skills/wbs/SKILL.md`). Opening such a file in bb's panel shows a
tree table with hours rolled up at every level, ranges, conditional items,
placeholders, and the open questions. A depth selector collapses deeper items
into their parents' totals.

`bb wbs html <file.wbs.yaml> [--depth N]` prints the same table as an
inline-styled HTML fragment (no page wrapper), ready to paste into a proposal
document or an HTML email.

## Layout

- `wbs-core/` — pure logic (cross-built JVM/JS): the YAML subset parser, the
  item model with totals, and the HTML renderer.
- `wbs-core-test/` — munit tests (JVM).
- `wbs/` — plugin backend (Scala.js): the `render` RPC and the `bb wbs` command.
- `app.tsx` — the `fileOpener` registration: an iframe over the rendered HTML.
- `skills/wbs/` — the file-format skill shipped with the plugin.
- `sample.wbs.yaml` — a small example exercising every value kind (numbers,
  a range, a placeholder, an if/then/else fork).

## Build and install

```sh
npm run build          # bleep-links server.js
bb plugin install .    # from this directory
```
