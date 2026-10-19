# Guide: how to bill last week's work with the Billing plugin as it is today

## Screen map

- **Navigation rail** (left edge): icons; "Billing" opens the Billing page.
- **Billing page header**: tabs "Week review" and "Client report".
- **Week controls row**: "← Previous week", "Week of Sun …", "Next week →", "This week", "Reload", "days in <zone>".
- **Banner area**: zero or more colored boxes (recorder unavailable, stale, lost, server zone, mapping warnings).
- **Day card**, seven of them, each: heading with the date; right-side summary ("billable 0m", "unresolved …", "N stale decision(s)"); timeline; thread table (Thread, Billed to, Alone, From overlaps, Total, Pushed); "Overlaps (N)" list with overlap cards; stale list; "Preview Zoho entries" button; after clicking it, the preview area with "Push to Zoho Books".
- **Thread page right panel**: the "+" at the top of the right panel opens an Actions list that includes "Billing"; it opens the thread's Billing panel tab.
- **Plugin settings page**: Settings → Installed plugins → Billing → the six settings.

## G1. First time: get it working

Method A (the only route):
1. Open Settings (gear, bottom of the rail) → Installed plugins → Billing.
2. Fill in Zoho OAuth client id, client secret, organization id, refresh token. Save.
3. Reload the plugin (plugin page "Reload").
4. Go to the Billing page.

Know this: the Billing page never tells you step 1 exists. Its only hint is an amber banner saying days follow the server's zone "because Zoho is not configured". Without step 1 the Client report tab shows an error and the thread Billing panel has no project list. The week review itself works without Zoho, but every row reads "not billable" because no thread is mapped yet (see G2), so the page looks broken.

## G2. Say which threads are for which client

Method A: thread panel. 1. Open the thread. 2. In the right panel click "+" (new tab). 3. Choose "Billing". 4. In "Bill this thread to", pick "Customer: Project" then the task. 5. Click "Assign". 6. Back on the Billing page click "Reload".

Method B: CLI. In the thread, ask the agent to run `bb billing assign --project <name> --task <name>`, or run it yourself with `--thread <id>`.

Know this: nothing on the Billing page links to Method A. The Thread column buttons open the thread page, not the Billing panel. Child threads inherit the parent's mapping; nothing on the Billing page shows which rows inherit. Both methods need Zoho configured (G1) to list projects.

## G3. See billable time this week per client/project

No direct route. Per day, the table shows a Total per thread and the day heading shows "billable Xm". To get a client's week you add the day totals yourself, or push everything and use the Client report tab.

## G4. Understand and settle an overlap

1. In a day card, find "Overlaps (N)". 2. Each card shows a time range, duration, and the threads involved. 3. Click "All to <thread>", or "Split minutes" then type minutes per thread and "Save split", or "Bill none of it". 4. Or click the day's "Give all of this day's overlaps to the billable thread where there is exactly one".

Know this: an "overlap" is a stretch when two or more threads were open at once (voice, typing, reading). "Alone" is the time a thread was the only one open; "From overlaps" is what your decisions gave it. The screen explains none of this. Overlaps between threads that are both "not billable" show "no billable thread involved" and have no buttons; they are listed anyway.

## G5. Push and know it worked

1. In a day card click "Preview Zoho entries". 2. Read the entries list, any red blocker lines, and the "Will delete from Zoho" list. 3. Click "Push to Zoho Books" (disabled while a blocker exists). 4. Confirm the dialog. 5. A box lists what was created/updated/deleted; the Pushed column fills in.

Know this: there is no week-level push; repeat per day. Empty days also offer "Preview Zoho entries"; clicking it says "nothing billable on this day". Nothing marks a day as ready before you click Preview.

## G6. Fix a mistake after pushing

Re-assign the thread (G2) or change the overlap decision (G4), then Preview and Push that day again. The preview shows "(updates existing)" and "Will delete from Zoho" for thread-days that lost their time.

## G7. Know when data is missing

Banners at the top of the week review. Red: recorder unavailable. Amber: speech feed stale, lost spans, server zone.

## G8. Ignore non-client threads

Method A: thread panel → "Mark not billable". Know this: every unmapped thread is already not billable, so this only matters to stop a child from inheriting a parent's mapping. The page still lists not-billable threads in every table.

## G9. Client report

Tab "Client report" → pick Customer, From, To → "Generate". Know this: needs Zoho configured for the customer list; only pushed time appears.

## G10. Why is this number what it is?

Hover the timeline bars for times. There is no other explanation.
