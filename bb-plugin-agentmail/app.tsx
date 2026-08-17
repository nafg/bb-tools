// Frontend entry: the outgoing-email review form. While the agentmail_send /
// agentmail_reply tool (or the `bb agentmail send`/`reply` CLI) waits in
// bb.ui.requestInput, this form replaces the thread's composer; submitting
// returns the (possibly edited) draft to the waiting handler, which performs
// the actual send.
import { useState } from "react";
import { definePluginApp } from "@get-bb/plugin-sdk/app";

interface EmailReviewPayload {
  kind: "send" | "reply";
  to: string[];
  cc: string[];
  subject: string;
  body: string;
  hasHtml: boolean;
  attachments: string[];
}

// The payload round-trips through JSON persistence, so read it defensively.
function parsePayload(raw: unknown): EmailReviewPayload {
  const p = (raw ?? {}) as Record<string, unknown>;
  const list = (v: unknown): string[] =>
    Array.isArray(v) ? v.filter((x): x is string => typeof x === "string") : [];
  return {
    kind: p.kind === "reply" ? "reply" : "send",
    to: list(p.to),
    cc: list(p.cc),
    subject: typeof p.subject === "string" ? p.subject : "",
    body: typeof p.body === "string" ? p.body : "",
    hasHtml: p.hasHtml === true,
    attachments: list(p.attachments),
  };
}

function splitAddresses(text: string): string[] {
  return text
    .split(/[,\s]+/)
    .map((s) => s.trim())
    .filter(Boolean);
}

const inputClass =
  "w-full rounded-md border border-input bg-background px-2.5 py-1.5 text-sm " +
  "outline-none focus-visible:ring-2 focus-visible:ring-ring";

function Field({ label, children }: { label: string; children: React.ReactNode }) {
  return (
    <label className="block space-y-1">
      <span className="text-xs font-medium text-muted-foreground">{label}</span>
      {children}
    </label>
  );
}

interface PendingInteractionProps {
  interaction: { payload: unknown };
  submit(value: unknown): Promise<void>;
  cancel(): Promise<void>;
}

function EmailReviewForm({ interaction, submit, cancel }: PendingInteractionProps) {
  const draft = parsePayload(interaction.payload);
  const isReply = draft.kind === "reply";
  const [to, setTo] = useState(draft.to.join(", "));
  const [cc, setCc] = useState(draft.cc.join(", "));
  const [subject, setSubject] = useState(draft.subject);
  const [body, setBody] = useState(draft.body);
  const [busy, setBusy] = useState(false);

  const canSend = body.trim() !== "" && (isReply || splitAddresses(to).length > 0);

  async function act(run: () => Promise<void>) {
    setBusy(true);
    try {
      await run();
    } finally {
      setBusy(false);
    }
  }

  return (
    <div className="space-y-3 rounded-lg border border-border bg-card p-4 text-sm">
      <div className="font-medium">
        {isReply ? "Review email reply before sending" : "Review email before sending"}
      </div>
      {isReply ? (
        <div className="space-y-0.5 text-xs text-muted-foreground">
          <div>To: {draft.to.join(", ")}</div>
          {draft.subject !== "" && <div>Subject: {draft.subject}</div>}
        </div>
      ) : (
        <>
          <Field label="To">
            <input className={inputClass} value={to} onChange={(e) => setTo(e.target.value)} />
          </Field>
          <Field label="Cc">
            <input className={inputClass} value={cc} onChange={(e) => setCc(e.target.value)} />
          </Field>
          <Field label="Subject">
            <input
              className={inputClass}
              value={subject}
              onChange={(e) => setSubject(e.target.value)}
            />
          </Field>
        </>
      )}
      <Field label="Body">
        <textarea
          className={inputClass + " min-h-40 resize-y"}
          rows={10}
          value={body}
          onChange={(e) => setBody(e.target.value)}
        />
      </Field>
      {draft.attachments.length > 0 && (
        <div className="space-y-0.5 text-xs text-muted-foreground">
          <div className="font-medium">Attachments</div>
          {draft.attachments.map((path) => (
            <div key={path} className="font-mono">
              {path}
            </div>
          ))}
        </div>
      )}
      {draft.hasHtml && (
        <div className="text-xs text-muted-foreground">
          An HTML version was also composed; if you edit the body text, the email is sent as
          plain text only.
        </div>
      )}
      <div className="flex items-center justify-end gap-2">
        <button
          type="button"
          disabled={busy}
          onClick={() => void act(cancel)}
          className="rounded-md border border-border px-3 py-1.5 text-sm hover:bg-accent disabled:opacity-50"
        >
          Don't send
        </button>
        <button
          type="button"
          disabled={busy || !canSend}
          onClick={() =>
            void act(() =>
              submit(
                isReply
                  ? { body }
                  : { to: splitAddresses(to), cc: splitAddresses(cc), subject, body },
              ),
            )
          }
          className="rounded-md bg-primary px-3 py-1.5 text-sm font-medium text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
        >
          Send
        </button>
      </div>
    </div>
  );
}

export default definePluginApp((app) => {
  app.slots.pendingInteraction({ id: "email-review", component: EmailReviewForm });
});
