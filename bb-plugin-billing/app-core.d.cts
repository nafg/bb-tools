// Hand-maintained declarations for the Scala.js frontend core (app-core.cjs,
// built from the billing-app Bleep project — see package.json "build").
import type * as React from "react";

export type RpcCall = (method: string, input: unknown) => Promise<unknown>;

export interface ReviewPageCoreProps {
  call: RpcCall;
  /** Route remainder inside the plugin panel: "" for the review, "report" for reports. */
  subPath: string;
  navigate: (subPath: string) => void;
  openThread: (threadId: string) => void;
  /** Increments when the backend published a change; the page reloads. */
  signal: number;
}

export interface AssignPanelCoreProps {
  threadId: string;
  call: RpcCall;
}

export interface SendThreadFormCoreProps {
  /** The `plan` result the backend attached to the interaction. */
  plan: unknown;
  submit: (value: unknown) => void;
  cancel: () => void;
}

export const ReviewPage: React.FC<ReviewPageCoreProps>;
export const SendThreadForm: React.FC<SendThreadFormCoreProps>;
export const AssignPanel: React.FC<AssignPanelCoreProps>;
