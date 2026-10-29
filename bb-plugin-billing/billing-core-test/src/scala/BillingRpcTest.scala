package billingcore

import munit.FunSuite

class BillingRpcTest extends FunSuite:
  private val key = BlockKey("2026-10-26", 1000L, 2000L, Set("a", "b"))

  /** One request per method: what the page sends. */
  private val samples: Vector[BillingRpc[?]] = Vector(
    BillingRpc.Overview(1_700_000_000_000L),
    BillingRpc.SessionScreen(None),
    BillingRpc.SessionScreen(Some(1_700_000_000_000L)),
    BillingRpc.Ledger("thr_1", subthreads = false),
    BillingRpc.Draft("thr_1", subthreads = true, DraftPick.Keys(Set("k1", "k2"))),
    BillingRpc.Draft("thr_1", subthreads = true, DraftPick.Keys(Set.empty)),
    BillingRpc.Summarize("thr_1", subthreads = true, DraftPick.AllUnbilled),
    BillingRpc.SetExcluded("k1", excluded = false),
    BillingRpc.SetNotes("thr_1", CivilDate(2026, 10, 26), Some("notes")),
    BillingRpc.SetNotes("thr_1", CivilDate(2026, 10, 26), None),
    BillingRpc.Decide("thr_1", subthreads = true, key, Allocation.Portions(Map("a" -> 600_000L))),
    BillingRpc.Decide("thr_1", subthreads = false, key, Allocation.WholeTo("a")),
    BillingRpc.Decide("thr_1", subthreads = true, key, Allocation.Nobody),
    BillingRpc.Undecide(key),
    BillingRpc.SettleAll("thr_1", subthreads = false),
    BillingRpc.Submit("thr_1", subthreads = true, DraftPick.Keys(Set("k1")), billed = true),
    BillingRpc.Undo("sub_1"),
    BillingRpc.Audit("sub_1"),
    BillingRpc.Submissions,
    BillingRpc.Catalog(refresh = true),
    BillingRpc.Mapping("thr_1"),
    BillingRpc.Assign("thr_1", "p1", "t1"),
    BillingRpc.MarkUnbillable("thr_1"),
    BillingRpc.ClearMapping("thr_1"),
    BillingRpc.ProjectMappings,
    BillingRpc.SetProjectMapping("proj_1", Vector(ProjectZoho("p1", "t1"), ProjectZoho("p2", "t2"))),
    BillingRpc.SetProjectNotClientWork("proj_1", notClientWork = true),
    BillingRpc.Report("c1", CivilDate(2026, 10, 1), CivilDate(2026, 10, 31))
  )

  test("every request survives the wire"):
    samples.foreach(rpc => assertEquals(BillingRpc.decode(rpc.method, rpc.encode), Right(rpc), rpc.toString))

  test("every registered method has a request, and every request a registered method"):
    assertEquals(samples.map(_.method).distinct.sorted, BillingRpc.methods.sorted)

  test("every event survives the wire"):
    Vector(
      BillingEvent.Changed,
      BillingEvent.SummariesWritten("thr_1"),
      BillingEvent.SummariesFailed("thr_1", "no model")
    ).foreach(e => assertEquals(BillingEvent.decode(e.encode), Some(e)))

  test("a missing thread id is refused by name"):
    assertEquals(BillingRpc.decode("decide", Wire.obj("key" -> Wire.Str(key.text))), Left("threadId is required"))

  test("an unknown method is refused"):
    assertEquals(BillingRpc.decode("plan", Wire.obj()), Left("unknown method plan"))
