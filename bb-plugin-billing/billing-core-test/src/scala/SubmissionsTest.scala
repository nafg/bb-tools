package billingcore

import activitycore.Window

class SubmissionsTest extends munit.FunSuite:
  private val Day                                      = "2026-10-19"
  private def at(minutes: Int): Long                   = 1_000_000_000L + minutes * 60_000L
  private def span(thread: String, from: Int, to: Int) = DayInterval(Day, thread, at(from), at(to))
  private def win(thread: String, from: Int, to: Int)  = Window(thread, Day, at(from), at(to))
  private def minutes(n: Int): Long                    = n * 60_000L

  private val acme                               = ZohoTarget("p1", "Acme", "t1", "Development", "c1", "Acme Inc")
  private val billability: String => Billability =
    case "a" | "b" => Billability.Billable(acme)
    case _         => Billability.NotBillable

  // a: 0-30 alone; a+scratch share 30-40; a 40-50 alone; b 60-80 alone
  private val recording = Seq(span("a", 0, 50), span("scratch", 30, 40), span("b", 60, 80))

  test("a draft sums time alone per day and lists the shared stretches it touches"):
    val plan = Submissions.plan(Seq("a"), Vector(win("a", 10, 50)), recording, Map.empty, billability)
    // 40 minutes in the draft, of which 10 are shared and undecided
    assertEquals(plan.totalsByDay, Map(Day -> minutes(30)))
    assertEquals(plan.undecided.map(b => (b.start, b.end)), Vector(at(30) -> at(40)))

  test("shared time counts once decided, confined to what the draft includes"):
    val key  = BlockKey(Day, at(30), at(40), Set("a", "scratch"))
    val plan = Submissions.plan(
      Seq("a"),
      Vector(win("a", 35, 50)),
      recording,
      Map(key -> Allocation.WholeTo("a")),
      billability
    )
    // alone: 40-50 = 10; shared 30-40 decided to a but only 35-40 is in the draft
    assertEquals(plan.totalsByDay, Map(Day -> minutes(15)))
    assertEquals(plan.undecided, Vector.empty)

  test("decision keys do not depend on the draft"):
    val early = Submissions.plan(Seq("a"), Vector(win("a", 0, 50)), recording, Map.empty, billability)
    val late  = Submissions.plan(Seq("a"), Vector(win("a", 35, 50)), recording, Map.empty, billability)
    assertEquals(early.undecided.map(_.key), late.undecided.map(_.key))

  test("a stretch outside the draft counts for nothing and needs no decision"):
    val plan = Submissions.plan(Seq("a"), Vector(win("a", 40, 50)), recording, Map.empty, billability)
    assertEquals(plan.totalsByDay, Map(Day -> minutes(10)))
    assertEquals(plan.undecided, Vector.empty)

  test("a draft covering a parent and its subthread bills both"):
    val plan =
      Submissions.plan(Seq("a", "b"), Vector(win("a", 40, 50), win("b", 60, 80)), recording, Map.empty, billability)
    assertEquals(plan.windows.map(_.threadId), Vector("a", "b"))
    assertEquals(plan.totalsByDay, Map(Day -> minutes(30)))

  test("the one-click decision settles stretches with exactly one billable thread"):
    val plan = Submissions.plan(Seq("a"), Vector(win("a", 0, 50)), recording, Map.empty, billability)
    assertEquals(
      Submissions.soleBillableDecisions(plan.undecided, billability).map(_.allocation),
      Vector(Allocation.WholeTo("a"))
    )

  test("windows crossing midnight produce entries on both dates"):
    val cal   = Calendar.Utc
    val start = cal.startOfDay("2026-10-20") - minutes(30)
    val split = Intervals.splitByDay(Seq(Interval("a", start, start + minutes(60))), cal)
    val plan  =
      Submissions.plan(Seq("a"), split.map(i => Window("a", i.day, i.start, i.end)), split, Map.empty, billability)
    assertEquals(plan.totalsByDay, Map("2026-10-19" -> minutes(30), "2026-10-20" -> minutes(30)))

class LedgerTest extends munit.FunSuite:
  private val Day                                     = "2026-10-19"
  private def at(minutes: Int): Long                  = 1_000_000_000L + minutes * 60_000L
  private def win(thread: String, from: Int, to: Int) = Window(thread, Day, at(from), at(to))

  test("a sent span cuts a stretch into pieces with one state each; undoing it brings one unbilled stretch back"):
    val sent   = Seq(CoveredBy("s1", billed = true, Span("a", at(10), at(20))))
    val pieces = Ledger.of(Seq(win("a", 0, 50)), sent, Coverage.empty, Map.empty)
    assertEquals(
      pieces.map(l => (l.window.start, l.window.end, l.state)),
      Vector(
        (at(0), at(10), StretchState.Unbilled),
        (at(10), at(20), StretchState.Sent("s1")),
        (at(20), at(50), StretchState.Unbilled)
      )
    )
    assertEquals(
      Ledger.of(Seq(win("a", 0, 50)), Nil, Coverage.empty, Map.empty).map(_.state),
      Vector(StretchState.Unbilled)
    )

  test("a strike-out, a settle and timer evidence each give their state; only unbilled is in the default draft"):
    val settled = Seq(CoveredBy("s2", billed = false, Span("a", at(0), at(10))))
    val trace   = ZohoTrace.TimerRan(at(30), at(40), "x")
    val pieces  = Ledger.of(
      Seq(win("a", 0, 10), win("a", 10, 20), win("a", 20, 30), win("a", 30, 40)),
      settled,
      Coverage.of(Vector(Span("a", at(10), at(20)))),
      Map("a" -> Seq(trace))
    )
    assertEquals(
      pieces.map(_.state),
      Vector(StretchState.Settled("s2"), StretchState.LeftOut, StretchState.Unbilled, StretchState.InZohoByHand(trace))
    )
    assertEquals(Ledger.unbilledKeys(pieces), Set(win("a", 20, 30).key))
    assertEquals(DraftPick.AllUnbilled.keysOn(pieces), Set(win("a", 20, 30).key))
    assertEquals(DraftPick.Keys(Set("k")).keysOn(pieces), Set("k"))
    assert(pieces(3).state.draftable && !pieces(1).state.draftable)

  test("a strike-out cuts a stretch like a submission does, so totals do not depend on how windows were cut"):
    val struck = Coverage.of(Vector(Span("a", at(10), at(20))))
    val whole  = Ledger.of(Seq(win("a", 0, 50)), Nil, struck, Map.empty)
    val pieces = Ledger.of(Seq(win("a", 0, 10), win("a", 10, 20), win("a", 20, 50)), Nil, struck, Map.empty)
    assertEquals(whole.map(_.state), Vector(StretchState.Unbilled, StretchState.LeftOut, StretchState.Unbilled))
    assertEquals(Ledger.totals(whole), Ledger.totals(pieces))
    assertEquals(Ledger.totals(whole).leftOutMs, 10 * 60_000L)

class WindowsTest extends munit.FunSuite:
  private val Day                    = "2026-10-19"
  private def at(minutes: Int): Long = 1_000_000_000L + minutes * 60_000L
  private val hour                   = 60 * 60_000L

  test("a short window stays whole"):
    val w = Window("a", Day, at(0), at(50))
    assertEquals(Windows.subdivide(w, Seq(at(10), at(20)), hour), Vector(w))

  test("a long window is cut at the first message after each hour"):
    val w = Window("a", Day, at(0), at(200))
    assertEquals(
      Windows.subdivide(w, Seq(at(30), at(65), at(100), at(130), at(190)), hour),
      Vector(Window("a", Day, at(0), at(65)), Window("a", Day, at(65), at(130)), Window("a", Day, at(130), at(200)))
    )

  test("a long window without messages cannot be cut"):
    val w = Window("a", Day, at(0), at(200))
    assertEquals(Windows.subdivide(w, Nil, hour), Vector(w))
