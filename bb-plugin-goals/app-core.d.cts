// Hand-maintained declarations for the Scala.js frontend core (app-core.cjs,
// built from the goals-app Bleep project — see package.json "build").

/** One rectangle of the nested layout, in the pixel space given to layoutOverview. */
export interface LayoutCell {
  /** A goal's outer box, a thread's box, or the cell for a thread's own time among its children. */
  kind: "goal" | "thread" | "self";
  /** The goal letter for a goal box; null for the unlabeled box. Absent on thread cells. */
  goal?: string | null;
  /** The thread id for thread and self cells. */
  id?: string;
  x: number;
  y: number;
  w: number;
  h: number;
  /** 0 for goal boxes, 1 for root threads, and so on. */
  depth: number;
}

/**
 * Nested boxes for the `overview` rpc's `groups`: a box per goal, root
 * threads inside, children inside parents, a thread's own time as a self
 * cell. Areas follow attention time plus a floor so every thread shows. `pad`
 * insets children inside a box; `header` reserves a strip at its top. Cells
 * come in paint order, parents before children.
 */
export function layoutOverview(groups: unknown[], width: number, height: number, pad: number, header: number): LayoutCell[];
