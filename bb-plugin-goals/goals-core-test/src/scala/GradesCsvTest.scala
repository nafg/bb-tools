package goalscore

class GradesCsvTest extends munit.FunSuite:
  private val sample =
    """thread_id,project,title,parent_thread_id,goal,value,effort,sidebar_state
      |thr_aaa111,bookphone,Mimi emails,,B,3,,snoozed until 2026-11-11
      |thr_bbb222,bookphone,"Research: BookPhone competitors, pricing, channels",thr_x,,,,
      |thr_ccc333,chesednow - disp,Port to slick 4 RC 1,,M,,,
      |thr_ddd444,chesednow - disp,Bad goal,,Z,1,,
      |thr_eee555,chesednow - disp,Bad value,,C,9,,
      |not-a-thread,x,Bad id,,B,1,,
      |""".stripMargin

  test("rows with a goal become grades; the rest are skipped with a reason"):
    val (rows, skipped) = GradesCsv.parse(sample)
    assertEquals(
      rows,
      List(GradeRow("thr_aaa111", Goal.B, Some(3)), GradeRow("thr_ccc333", Goal.M, None))
    )
    assertEquals(
      skipped.map(s => s.threadId -> s.reason),
      List(
        "thr_bbb222"   -> "no goal",
        "thr_ddd444"   -> "unknown goal 'Z'",
        "thr_eee555"   -> "value must be 1, 2, or 3, got '9'",
        "not-a-thread" -> "not a thread id"
      )
    )

  test("quoted fields with commas and doubled quotes are read as one field"):
    assertEquals(GradesCsv.splitLine("a,\"b, c\",\"say \"\"hi\"\"\",d"), List("a", "b, c", "say \"hi\"", "d"))

  test("the header must name the columns the import relies on"):
    val (_, skipped) = GradesCsv.parse("id,goal\nthr_a,B\n")
    assertEquals(skipped.map(_.reason), List("header lacks column 'thread_id'"))
