// Hand-maintained declarations for the Scala.js frontend core (app-core.cjs,
// built from the agentmail-app Bleep project — see package.json "build").
import type * as React from "react";

export type RpcCall = (method: string, input: unknown) => Promise<unknown>;

export interface PanelCoreProps {
  threadId: string;
  call: RpcCall;
  /** Increment to make the panel refetch (realtime change signal). */
  signal: number;
}

export interface HeaderButtonCoreProps {
  threadId: string;
  call: RpcCall;
  signal: number;
  openPanel: () => void;
}

export interface DirectiveCoreProps {
  draft?: string;
  openPanel: () => void;
}

export const EmailPanel: React.FC<PanelCoreProps>;
export const EmailHeaderButton: React.FC<HeaderButtonCoreProps>;
export const AgentmailDirective: React.FC<DirectiveCoreProps>;
