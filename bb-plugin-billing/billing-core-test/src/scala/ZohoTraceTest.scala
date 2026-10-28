package billingcore

import munit.FunSuite

class ZohoTraceTest extends FunSuite:
  private val H                                               = 3600_000L
  private def call(at: Long, name: String, args: String = "") = ToolCall(at, name, args)

  test("create and stop make one run with the timer's description; other tools are ignored"):
    val traces = ZohoTrace.of(Seq(
      call(10 * H, "zoho-books-timer:timer_create", """{"task":"Coding","description":"Fix the thing"}"""),
      call(10 * H + 1, "Bash"),
      call(10 * H + 2, "zoho-books-timer:timer_status"),
      call(12 * H, "zoho-books-timer:timer_stop")
    ))
    assertEquals(traces, Vector(ZohoTrace.TimerRan(10 * H, 12 * H, "task: Coding, description: Fix the thing")))

  test("a create while a timer runs ends the running one, since Zoho runs one timer at a time"):
    val traces = ZohoTrace.of(Seq(
      call(10 * H, "zoho-books-timer:timer_create", """{"description":"a"}"""),
      call(11 * H, "zoho-books-timer:timer_create", """{"description":"b"}"""),
      call(12 * H, "zoho-books-timer:timer_stop")
    ))
    assertEquals(
      traces,
      Vector(ZohoTrace.TimerRan(10 * H, 11 * H, "description: a"), ZohoTrace.TimerRan(11 * H, 12 * H, "description: b"))
    )

  test("a stop days later is not a run: the start is left running, and touches only its own window"):
    val traces = ZohoTrace.of(Seq(
      call(10 * H, "zoho-books-timer:timer_create", """{"description":"a"}"""),
      call(10 * H + 5 * 24 * H, "zoho-books-timer:timer_stop")
    ))
    assertEquals(traces, Vector(ZohoTrace.TimerLeftRunning(10 * H, "description: a")))
    assert(traces.head.touches(9 * H, 11 * H))
    assert(!traces.head.touches(20 * H, 21 * H))

  test("a stop without a start is nothing"):
    assertEquals(ZohoTrace.of(Seq(call(1 * H, "zoho-books-timer:timer_stop"))), Vector.empty)

  test("arguments read as pairs"):
    assertEquals(
      ZohoTrace.readable("""{"project":"ChesedNow LRBCOL","task":"Product management","minutes":30}"""),
      "project: ChesedNow LRBCOL, task: Product management, minutes: 30"
    )

  test("a start replaced within a minute is no run"):
    val traces = ZohoTrace.of(Seq(
      call(10 * H, "zoho-books-timer:timer_start", """{"timeEntryId":"1"}"""),
      call(10 * H + 5000, "zoho-books-timer:timer_create", """{"description":"a"}"""),
      call(11 * H, "zoho-books-timer:timer_stop")
    ))
    assertEquals(traces, Vector(ZohoTrace.TimerRan(10 * H + 5000, 11 * H, "description: a")))

  test("a timer left running runs over the recording until Zoho's logged time is used up, pauses and all"):
    val left = Vector(ZohoTrace.TimerLeftRunning(10 * H, "description: a"), ZohoTrace.TimerLeftRunning(20 * H, "b"))
    // recorded: 9-11, then a gap, then 12-15: 3h logged from 10 ends at 14, not at 13 on the clock
    val recording = Seq((9 * H, 11 * H), (12 * H, 15 * H))
    val out       = ZohoTrace.lengthened(left, t => if t.detail.endsWith("a") then Some(3 * H) else None, recording, Calendar.Utc.nextDayStart)
    assertEquals(
      out,
      Vector(
        ZohoTrace.TimerRan(10 * H, 14 * H, "description: a; 3h 00m logged, measured over recorded activity"),
        ZohoTrace.TimerLeftRunning(20 * H, "b")
      )
    )

  test("a timer left running ends with the recording when the logged time outlasts it"):
    val out = ZohoTrace.lengthened(
      Vector(ZohoTrace.TimerLeftRunning(10 * H, "a")),
      _ => Some(5 * H),
      Seq((9 * H, 11 * H), (12 * H, 13 * H)),
      Calendar.Utc.nextDayStart
    )
    assertEquals(out, Vector(ZohoTrace.TimerRan(10 * H, 13 * H, "a; 5h 00m logged, measured over recorded activity")))

  test("a timer left running does not cover activity on later days: it could not have been running then"):
    val day = 24 * H
    // started on day 0 at 10:00, 2h logged; the thread's activity after that is only on days 2 and 3
    val out = ZohoTrace.lengthened(
      Vector(ZohoTrace.TimerLeftRunning(10 * H, "a")),
      _ => Some(2 * H),
      Seq((2 * day + H, 2 * day + 2 * H), (3 * day, 3 * day + H)),
      Calendar.Utc.nextDayStart
    )
    assertEquals(out, Vector(ZohoTrace.TimerLeftRunning(10 * H, "a")))

  test("a timer left running covers the rest of its own day only, whatever logged time is left"):
    val day = 24 * H
    val out = ZohoTrace.lengthened(
      Vector(ZohoTrace.TimerLeftRunning(10 * H, "a")),
      _ => Some(3 * H),
      Seq((11 * H, 12 * H), (day + H, day + 3 * H)),
      Calendar.Utc.nextDayStart
    )
    assertEquals(out, Vector(ZohoTrace.TimerRan(10 * H, 12 * H, "a; 3h 00m logged, measured over recorded activity")))

  test("activity before the timer started is never covered"):
    val out = ZohoTrace.lengthened(
      Vector(ZohoTrace.TimerLeftRunning(10 * H, "a")),
      _ => Some(2 * H),
      Seq((8 * H, 9 * H), (11 * H, 11 * H + 30 * 60_000L)),
      Calendar.Utc.nextDayStart
    )
    assertEquals(
      out,
      Vector(ZohoTrace.TimerRan(10 * H, 11 * H + 30 * 60_000L, "a; 2h 00m logged, measured over recorded activity"))
    )
