# Product decisions and what was turned down

Why the plugin is the way it is, where the code and README cannot say it. Decisions are Naftoli's, relayed through the
parent design thread; dates are 2026.

## Settled

- **Nothing is written to Zoho in tests.** Tests use a fake client. The first real send is the user's own reviewed one
  (Oct 2026). Backdated entries (`log_date` in the past) were never spiked on purpose; they work in practice: entries
  dated Oct 8 were created on Oct 26.
- **Credentials are plugin settings** (client id, secret, organization id, refresh token) entered by the user. No OAuth
  consent flow, and no reading of the older timer tool's token cache or config.
- **Exact minutes are stored and shown.** One billing-increment setting (default 1 minute) is applied only when the
  entries are built for sending.
- **Time shared by two or more threads always needs a decision**, with one explicit action for "all to the one client
  thread". There is no implicit default. `turn` time (the agent running) is never requested from the recorder.
- **Archiving a thread is the moment to bill it, or to settle it without billing**, subthreads included. An open thread
  can be billed at any time. Billing is per thread, not per day: one submission can cover several days and still makes
  one Zoho entry per calendar day, because a Zoho entry has one date.
- **The user's own sends are deliberate.** A send, a cut ("up to here") or a strike-out is never described as a mistake
  on the page, the CLI or in reports.
- **Entry notes come from a model run in a hidden thread** on a model the user picks (see the summaries section of the
  README). The user will not supply an API key.
- **All of billing is doable by an agent**, but sending always waits for the user's confirmation in the thread.

## Turned down

- **A setting for which attention kinds count** ("is view time too generous?"). The recorder's defaults were designed
  deliberately; the user does not want them re-argued.
- **A minimum per thread-day** to avoid tiny entries. The problem seen was an invoice made of one 3-minute entry, not
  the existence of a small entry.
- **Day-at-a-time review as the workflow.** Doing it daily "defeats the purpose"; the unit is the thread.
- **Subtracting hand-entered Zoho time by clock window.** Zoho entries carry a date and hours but no start or end, so
  there is nothing to subtract against. The plugin shows the evidence (the agent's timer calls and Zoho's own entries)
  and the user leaves stretches out.

## Worth knowing before sending

- A subthread of the October reconciliation reported that Zoho invoiced a 3-minute entry sent on Oct 19 as its own
  invoice line within days (INV-619, Oct 22). Treat a send as client-visible quickly. This is reported second-hand;
  whether it is Zoho's automatic invoicing for that organization is not verified here.
