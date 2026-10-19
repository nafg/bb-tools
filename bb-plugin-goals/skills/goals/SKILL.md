---
name: goals
description: Label a bb thread with the goal it serves (B billable client work, C client work not billable, P product or future income, S sanity, M maintainer obligations, F fun), its value within that goal (1 biggest lever to 3), and the threads it is waiting on. Use when the user says what a thread is for, how important it is, or that it is blocked on another thread, and when they ask which goal a thread serves or how their threads are labeled.
---

# Goals

Each thread can carry a goal, a value within that goal, and a list of threads it waits on. A thread without its own goal is read as having its nearest labeled ancestor's goal and value. A blocked thread lends its goal and value to the thread blocking it, which raises the blocker's importance without changing its own label.

## Setting a label

Prefer the `goals_set_label` tool: it defaults to the current thread, and omitted fields are left as they are. Pass `"none"` to clear a goal or value, and `blockedBy` as the complete list of thread ids (an empty list clears it).

Without the tool, the CLI does the same:

```
bb goals set [thread-id] [--goal B|C|P|S|M|F|none] [--value 1|2|3|none] [--blocked-by thr_a,thr_b|none]
bb goals show [thread-id]
bb goals list
bb goals import <file.csv>
```

With no thread id, `set` and `show` act on the current thread. `import` reads a CSV with `thread_id`, `goal`, and optional `value` columns, writes each row that has a goal, and prints what it skipped and why.

## Reading labels

`bb goals list` prints every open thread as `B2 thr_… title`; a trailing `*` means the label is inherited from an ancestor. The Goals page in the bb sidebar shows the same threads as nested boxes sized by attention time.

## Meaning of the letters

- **B** billable client work
- **C** client work that is not billable
- **P** product or future income
- **S** sanity: tooling that keeps the user's own work sustainable
- **M** maintainer obligations (open-source projects the user maintains)
- **F** fun

Value 1 is the biggest lever for its goal, 3 the smallest; a goal can be set without a value.
