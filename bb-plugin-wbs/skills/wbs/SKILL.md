---
name: wbs
description: Work-breakdown-structure (WBS) estimate files. Use whenever the user wants to create, read, edit, or total a project estimate, an effort breakdown, a scope breakdown with hours, or a proposal's work items — even if they never say "WBS". Also use when a `*.wbs.yaml` file is mentioned or found under docs/proposals/. Defines the file format and where the files live.
---

# WBS estimate files

An estimate is a tree of work items with hour estimates at the leaves. It is a plain YAML file that the user edits by hand and that agents read and write directly; no tool is required.

## Location

`docs/proposals/<name>.wbs.yaml`, relative to the project root. One file per proposal/engagement; `<name>` is a short kebab-case slug for the engagement (e.g. `docs/proposals/client-portal-v2.wbs.yaml`). Keep the files flat in that directory — related proposal artifacts for the same engagement use the same `<name>` prefix (e.g. `<name>.proposal.md`), so a directory listing groups them and `*.wbs.yaml` globs all estimates.

Create `docs/proposals/` if it does not exist.

## Format

The file is a single YAML mapping. Each key is a work item; its value is one of:

- **a number** — the estimate for that item in hours. Fractions are allowed (`0.5`, `1.25`). This is a leaf.
- **a two-element array `[low, high]`** — a range estimate in hours, when the item's cost genuinely depends on something not yet known. Both elements are numbers, `low <= high`. This is also a leaf. Use a plain number whenever you can; a range should be the exception, and the reason for the spread should be evident from the item name or a YAML comment on that line.
- **a mapping** — a further breakdown of that item into sub-items, using the same rule recursively.
- **a string** — the item is not estimated yet. The string says what is missing (an open question, "scope not confirmed", etc.). It contributes nothing to totals and marks every ancestor as incomplete. Use it to sketch the whole tree before all the numbers are known.

That is the whole format. There are no other value types, no metadata keys, and no reserved names. Order is meaningful only for presentation; keep items in the order the user wrote them.

### Conditional items

A key that starts with `if ` (followed by a proposition) is a fork, not a work item. Its value is a mapping with a `then` key and/or an `else` key — at least one — and each branch is a mapping of work items following the same rules as the top level (never a bare number, range, or string).

```yaml
Playback and re-record:
  Restructure handler into record→play→confirm loop: 1.5
  if Twilio URL plays unauthenticated:
    then:
      Use Twilio recording URL directly: 0.5
    else:
      Public signed-URL audio route: 3
      Test coverage for the signed route: 1
  Deploy + phone verification: 0.5
```

A proposition is free text. Two forks refer to the same proposition when their text matches after whitespace normalization, case-insensitively. Reuse the exact same text wherever the same unknown applies — including in other branches or other parents — so that totals treat it as one unknown, not several. Propositions should be genuine unknowns (a fact to verify, or a decision the user or client must make), stated so that "true" has an obvious meaning.

A two-way choice has both `then` and `else`. Optional work is a fork with only `then` (or only `else`, when the work is needed exactly when the proposition is false). Because YAML keys are unique within a mapping, one proposition appears at most once per parent: put everything that hinges on it under that one fork. If some of that work must sit between unconditional siblings, wrap that piece in its own named group.

Since `if ` at the start of a key is reserved, a work item whose name begins that way must be quoted or reworded.

```yaml
Authentication:
  Login form: 2
  Password reset: 3.5
  Session handling: 1.5
Reporting:
  Data export:
    CSV: 1
    PDF: [3, 6]   # 3 if the existing renderer handles it, 6 if a new one is needed
  Dashboard: 6
Deployment: 2
```

Keys are free text: use natural, client-readable item names.

## Totals

An item's total is the sum of its leaves. Compute totals whenever the user asks for them or for a report; there is no stored total in the file, so never add one (it would go stale and would be mistaken for a leaf).

Ranges propagate: a plain number counts as `[n, n]`, and an item containing any range leaf has a range total (sum the lows and sum the highs separately). Show hours as durations (`4h`, `30m`, `1h 30m`). Show a range total as `low–high`; show it as a single value only when low equals high.

Forks propagate too. A fork contributes its `then` branch when the proposition is true and its `else` branch when it is false (a missing branch contributes nothing). While propositions are unresolved, an item's total is `min–max` over every assignment of true/false to the propositions appearing beneath it, with each proposition assigned the same value everywhere it occurs. Enumerating assignments is exponential in the number of distinct propositions; that is fine for a handful and is a reason to keep propositions to genuine unknowns.

Forks are transparent when collapsing to a depth: the items inside a branch count at the same level as the fork's siblings.

A string leaf contributes nothing, and any item containing one is reported as incomplete — e.g. `2h + 1 unestimated` — so that a sketch is never mistaken for a finished quote.

When reporting, also list the **open questions**: every distinct proposition still present, and every string leaf with its path. That list is what the user still has to find out, decide, or ask the client.

The file is usually more fine-grained than what the user sends the client. When asked for a summary "at level N" or "at the top level", collapse the tree to that depth and show the summed hours for each remaining item, e.g. for the example above at depth 1:

| Item | Estimate |
|---|---|
| Authentication | 7h |
| Reporting | 10h–13h |
| Deployment | 2h |
| **Total** | **19h–22h** |

## Editing

- Preserve the user's item names, order, and nesting; change only what was asked.
- To refine an item, replace its number with a mapping of sub-items; the sub-items should sum to roughly the old number unless the user is re-estimating.
- To estimate a placeholder, replace the string with a number, a range, or a mapping.
- To resolve a proposition, edit in place: replace each fork on it with the contents of the branch that applies (delete the fork entirely if that branch is missing). Do this everywhere the proposition occurs. There is no decision log in the file; git history is the record.
- Keep the file valid YAML. Quote a key (single quotes; double any `'` inside) when it contains `: `, contains ` #` (a space followed by `#` starts a comment — this bites keys like `Fix logout (#6)`), or begins with a quote character. Validate after editing (e.g. `python3 -c 'import yaml,sys; yaml.safe_load(open(sys.argv[1]))' <file>`).
