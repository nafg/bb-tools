package localgitcore

import subscriptionsource.Recognition

class LocalBranchTest extends munit.FunSuite:
  private val bbTools = LocalBranch("/home/me/bb-tools", "master")

  test("parses git:/path#branch, including branch names with slashes, and round-trips"):
    assertEquals(LocalBranch.parse("git:/home/me/bb-tools#master"), Some(bbTools))
    assertEquals(LocalBranch.parse("git:/home/me/bb-tools/#bb/fix-x"), Some(LocalBranch("/home/me/bb-tools", "bb/fix-x")))
    assertEquals(LocalBranch.parse(bbTools.url), Some(bbTools))

  test("a malformed git: target gets a hint; other sources' targets are not this source's"):
    assert(LocalBranch.recognize("git:relative/path#master").isInstanceOf[Recognition.Invalid])
    assert(LocalBranch.recognize("git:/home/me/bb-tools").isInstanceOf[Recognition.Invalid])
    assertEquals(LocalBranch.recognize("https://github.com/slick/slick/tree/main"), Recognition.NotMine)
    assertEquals(LocalBranch.recognize("maven:g:a"), Recognition.NotMine)

  test("a first check reports nothing; a move reports the new commits once"):
    assertEquals(LocalBranchWatch.check(bbTools, None, "aaa", None).messages, Nil)
    assertEquals(LocalBranchWatch.check(bbTools, Some("aaa"), "aaa", None).messages, Nil)
    val moved = LocalBranchWatch.check(bbTools, Some("aaaaaaaaaaaa"), "bbbbbbbbbbbb", Some(Movement(2, List("Second", "First"), rewritten = false)))
    assertEquals(
      moved.messages,
      List("master in /home/me/bb-tools moved from aaaaaaaaaa to bbbbbbbbbb. 2 new commits: Second; First.")
    )
    assert(!moved.finished)
    assertEquals(LocalBranchWatch.decode(moved.state), Right("bbbbbbbbbbbb"))

  test("a rewritten branch says so"):
    val rewritten = LocalBranchWatch.check(bbTools, Some("aaaaaaaaaaaa"), "cccccccccccc", Some(Movement(1, List("Redo"), rewritten = true)))
    assertEquals(
      rewritten.messages,
      List("master in /home/me/bb-tools was rewritten from aaaaaaaaaa to cccccccccc. 1 new commit: Redo.")
    )
