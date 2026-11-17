package subscriptionscore

import subscriptionsource.{Recognition, Text}

class SubscriptionsCoreTest extends munit.FunSuite:
  test("another thread can be watched until it is archived or deleted"):
    assertEquals(ThreadArchived.parse("bb:thread/thr_abc123/archived"), Some(ThreadArchived("thr_abc123")))
    assertEquals(ThreadArchived.parse(ThreadArchived("thr_x1").url), Some(ThreadArchived("thr_x1")))
    // A thread without an event doesn't say what to wait for, so it's refused rather than guessed.
    assert(ThreadArchived.recognize("bb:thread/thr_abc123").isInstanceOf[Recognition.Invalid])
    assert(ThreadArchived.recognize("bb:thread/thr_abc123/messages").isInstanceOf[Recognition.Invalid])
    assertEquals(ThreadArchived.recognize("@thread:thr_abc123/archived"), Recognition.NotMine)
    assertEquals(ThreadWatch.decode(ThreadWatch.encode(ThreadLife.Open)), Right(ThreadLife.Open))
    val target = ThreadArchived("thr_x1")
    assertEquals(ThreadWatch.check(target, None, ThreadLife.Open, "T", None).messages, Nil)
    assertEquals(ThreadWatch.check(target, Some(ThreadLife.Open), ThreadLife.Open, "T", None).messages, Nil)
    val archived = ThreadWatch.check(target, Some(ThreadLife.Open), ThreadLife.Archived, "Port to Slick 4", Some("Done; MR !44 merged."))
    assert(archived.finished)
    assertEquals(
      archived.messages,
      List("Thread \"Port to Slick 4\" (@thread:thr_x1) was archived.\nIts last reply:\n  Done; MR !44 merged.")
    )

  test("table padding and inline HTML are dropped from quoted text"):
    val table = List(
      "| Incompatibility | Symbol      | Problem                                      |",
      "|-----------------|-------------|----------------------------------------------|",
      "| Source          | `a.b.c`     | <strong>DirectMissingMethodProblem</strong><p><small>gone</small></p> |",
      "  indented   code"
    ).mkString("\n")
    assertEquals(
      Text.visibleText(table),
      List(
        "| Incompatibility | Symbol | Problem |",
        "|---|---|---|",
        "| Source | `a.b.c` | DirectMissingMethodProblem gone |",
        "  indented code"
      ).mkString("\n")
    )

  test("hidden HTML comments and runs of blank lines are dropped from quoted text"):
    assertEquals(
      Text.visibleText("<!-- marker -->\n\n**Use 2.7.0.**\n\n\n\n<details>\n<!-- a\nb -->x  \n</details>\n"),
      "**Use 2.7.0.**\n\n<details>\nx\n</details>"
    )

