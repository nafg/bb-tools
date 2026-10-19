# Goals: what a user wants from the Billing plugin, in their words

Use case: "Bill last week's work on a client's project to Zoho Books."

- **G1. Get the plugin working the first time.** "I opened Billing. What do I do first?" Covered: **No apparent way** on the page. Zoho settings exist in Settings → Plugins; nothing on the page says so. The week review renders with everything "not billable".
- **G2. Say which threads are for which client.** "This thread is Acme development; bill it there." Covered: **Partly**. Only through the thread's right-panel "+" → Billing, or `bb billing assign`. Nothing on the Billing page leads there.
- **G3. See how much billable time I have this week per client/project.** Covered: **Partly**. Per thread per day there is a Total; there is no per-client or per-week sum, and billable and unbillable threads are mixed in one table.
- **G4. Understand what an "overlap" is and settle it quickly.** Covered: **Partly**. Controls exist; the words "overlap", "Alone", "From overlaps", "decision" are not explained anywhere on screen.
- **G5. Push a day (or the whole week) to Zoho and know it worked.** Covered: **Partly**. Per day: Preview → Push. No week-level push; no indication of which days are ready.
- **G6. Fix a mistake after pushing** (wrong project, wrong split). Covered: re-assign then push again updates; deletion previewed. Not visible until Preview is clicked.
- **G7. Know when data is missing** (recorder off, voice feed stale). Covered: banners.
- **G8. Ignore threads that are not client work.** Covered: **Partly**: "Mark not billable" exists only in the thread panel. By default every unmapped thread already counts as not billable, which the page shows as rows anyway.
- **G9. Produce a client report.** Covered: Client report tab; requires Zoho catalog for the customer list.
- **G10. Understand why a number is what it is** ("why 29m?"). Covered: **Partly**: timeline bars with tooltips; no per-row explanation.
