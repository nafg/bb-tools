package activitycore

/** The activity recorder's RPC surface, for plugins that read its data.
  *
  * Times are epoch milliseconds. `threadIds` and `kinds` are optional filters;
  * `kinds` names [[SignalKind]]s by `name`.
  *
  *   - `intervals`: input `{ from, to, threadIds?, kinds?, gapMs? }`, output
  *     `{ intervals: [{ threadId, start, end, kinds }] }`. Per thread, signals of
  *     the requested kinds (default [[SignalKind.Attention]]) are merged by the
  *     gap rule and clipped to `[from, to)`; see [[Intervals.derive]]. `gapMs`
  *     defaults to the plugin's setting.
  *   - `signals`: input `{ from, to, threadIds?, kinds? }` (default: every
  *     kind), output `{ signals: [{ id, threadId, kind, start, end, source }] }`
  *     for signals reaching into `[from, to)`.
  *   - `status`: input `null`, output `{ gapMs, staleAfterMs, sources: { converse:
  *     { available, cursorSeq, cursorAt, lastSyncAt, stale, error } } }`.
  *
  * A caller that gets an unknown-plugin failure should treat the recorder as
  * absent.
  */
object ActivityRecorderRpc:
  val PluginId  = "activity-recorder"
  val Intervals = "intervals"
  val Signals   = "signals"
  val Status    = "status"
