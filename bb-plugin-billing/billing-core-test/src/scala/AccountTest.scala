package billingcore

import activitycore.Window

import munit.FunSuite

class AccountTest extends FunSuite:
  private val target                      = ZohoTarget("p", "Project", "t", "Coding", "c", "Customer")
  private def win(start: Long, end: Long) = Window("a", "2026-10-08", start, end)

  test("totals count each state, with in-Zoho time inside unbilled"):
    val t = Ledger.totals(Vector(
      LedgerStretch(win(0, 60_000), StretchState.Unbilled),
      LedgerStretch(win(60_000, 180_000), StretchState.InZohoByHand(ZohoTrace.TimerRan(60_000, 180_000, "x"))),
      LedgerStretch(win(180_000, 240_000), StretchState.LeftOut),
      LedgerStretch(win(240_000, 300_000), StretchState.Sent("s1")),
      LedgerStretch(win(300_000, 420_000), StretchState.Settled("s2"))
    ))
    assertEquals(t, Totals(180_000, 120_000, 60_000, 60_000, 120_000, 0))
    assertEquals(t.recordedMs, 420_000L)

  test("an account survives the wire and knows its standing"):
    val a = ThreadAccount(
      "thr_1",
      "Title",
      "proj",
      "Proj",
      Some("thr_0"),
      1000L,
      Some(2000L),
      Resolved(Billability.Billable(target), Some(MappingSource.Project)),
      Totals(60_000, 0, 0, 120_000, 0, 0),
      1,
      Some(1000L),
      Some(3000L)
    )
    assertEquals(ThreadAccount.decode(a.encode), Some(a))
    assertEquals(a.standing, Standing.ArchivedToSettle)
    assertEquals(
      ThreadAccount.decode(a.copy(resolved = Resolved(Billability.NotBillable, None), parentThreadId = None).encode)
        .map(_.standing),
      Some(Standing.Undecided)
    )
    assertEquals(
      a.copy(resolved = Resolved(Billability.NotBillable, Some(MappingSource.Thread("thr_1")))).standing,
      Standing.NotClientWork
    )
    assertEquals(a.copy(resolved = Resolved(Billability.NotBillable, None), totals = Totals.zero).standing, Standing.NotClientWork)

  test("a submission survives the wire and reads the same everywhere"):
    val s     = SubmissionRecord(
      "sub_1",
      "thr_1",
      "Title",
      Vector(SubmissionSpan("thr_1", "Title", 0L, 3_600_000L)),
      billed = true,
      target,
      5_000L,
      Vector(SubmissionEntry("2026-10-08", "e1", 39, "notes"))
    )
    assertEquals(SubmissionRecord.decode(s.encode), Some(s))
    val stamp = (ms: Long) => s"t$ms"
    assertEquals(
      Words.submission(s, stamp),
      "Sent to Zoho on t5000: Thu 2026-10-08 39m; covers Title from t0 to t3600000"
    )
    assertEquals(
      Words.submission(s.copy(billed = false), stamp),
      "Settled without billing on t5000; covers Title from t0 to t3600000"
    )
    assertEquals(
      Words.submission(s.copy(spans = Vector.empty), stamp),
      "Sent per day on t5000, before submissions recorded their stretches: Thu 2026-10-08 39m"
    )

  test("totals in words"):
    assertEquals(Words.totals(Totals(0, 0, 0, 0, 0, 0), 0), "nothing unbilled, nothing sent before")
    assertEquals(
      Words.totals(Totals(2_820_000, 720_000, 60_000, 8_700_000, 0, 0), 2),
      "47m unbilled (12m in Zoho by hand), 2h 25m sent or settled in 2 submissions, 1m left out"
    )
