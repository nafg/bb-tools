package subscriptionscore

import subscriptionscore.Snapshot.GitHubCheck

class SubscriptionsCoreTest extends munit.FunSuite:
  test("parses GitHub pull request URLs, ignoring tabs, queries and fragments"):
    val expected = Right(ChangeRequest(Forge.GitHub, "github.com", "slick/slick", 3909))
    assertEquals(ChangeRequest.parse("https://github.com/slick/slick/pull/3909"), expected)
    assertEquals(ChangeRequest.parse("https://github.com/slick/slick/pull/3909/files?w=1"), expected)
    assertEquals(ChangeRequest.parse(" https://github.com/slick/slick/pull/3909#discussion "), expected)

  test("parses GitLab merge request URLs with nested groups"):
    assertEquals(
      ChangeRequest.parse("https://gitlab.com/chesednow/sub/disp/-/merge_requests/530/diffs"),
      Right(ChangeRequest(Forge.GitLab, "gitlab.com", "chesednow/sub/disp", 530))
    )

  test("rejects URLs that are not pull or merge requests"):
    assert(ChangeRequest.parse("https://github.com/slick/slick/issues/1").isLeft)
    assert(ChangeRequest.parse("slick/slick#1").isLeft)

  test("URL and label round-trip"):
    val cr = ChangeRequest(Forge.GitLab, "gitlab.com", "chesednow/disp", 530)
    assertEquals(ChangeRequest.parse(cr.url), Right(cr))
    assertEquals(cr.label, "chesednow/disp!530")

  test("GitHub CI: failure wins over pending, cancellation over pending, nothing means no CI"):
    val done    = GitHubCheck.CheckRun("COMPLETED", Some("SUCCESS"))
    val running = GitHubCheck.CheckRun("IN_PROGRESS", None)
    val failed  = GitHubCheck.CheckRun("COMPLETED", Some("FAILURE"))
    val cancel  = GitHubCheck.CheckRun("COMPLETED", Some("CANCELLED"))
    assertEquals(Snapshot.ciFromGitHub(Seq(done, running, failed)), Right(CiResult.Failed))
    assertEquals(Snapshot.ciFromGitHub(Seq(done, running, cancel)), Right(CiResult.Canceled))
    assertEquals(Snapshot.ciFromGitHub(Seq(done, running)), Right(CiResult.Pending))
    assertEquals(Snapshot.ciFromGitHub(Seq(done, GitHubCheck.StatusContext("SUCCESS"))), Right(CiResult.Passed))
    assertEquals(Snapshot.ciFromGitHub(Nil), Right(CiResult.NoCi))

  test("unrecognized values are errors, not guesses"):
    assert(Snapshot.ciFromGitHub(Seq(GitHubCheck.CheckRun("COMPLETED", Some("SOMETHING_NEW")))).isLeft)
    assert(Snapshot.ciFromGitLab(Some("something_new")).isLeft)
    assert(Snapshot.lifecycleFromGitHub("DRAFT").isLeft)

  test("GitLab pipeline statuses"):
    assertEquals(Snapshot.ciFromGitLab(None), Right(CiResult.NoCi))
    assertEquals(Snapshot.ciFromGitLab(Some("running")), Right(CiResult.Pending))
    assertEquals(Snapshot.ciFromGitLab(Some("success")), Right(CiResult.Passed))
    assertEquals(Snapshot.ciFromGitLab(Some("canceled")), Right(CiResult.Canceled))

  private val open = Snapshot(Lifecycle.Open, CiResult.Pending, "aaa")

  test("a finished CI result is reported once per commit"):
    val passed = open.copy(ci = CiResult.Passed)
    assertEquals(Changes.between(open, passed), List(Change.CiFinished(CiResult.Passed, "aaa")))
    assertEquals(Changes.between(passed, passed), Nil)
    val newCommitPassed = passed.copy(headSha = "bbb")
    assertEquals(Changes.between(passed, newCommitPassed), List(Change.CiFinished(CiResult.Passed, "bbb")))

  test("pending and missing CI are not reported"):
    assertEquals(Changes.between(open.copy(ci = CiResult.Passed), open.copy(headSha = "bbb")), Nil)
    assertEquals(Changes.between(open, open.copy(ci = CiResult.NoCi)), Nil)

  test("merge, close and reopen are reported, CI first"):
    val merged = Snapshot(Lifecycle.Merged, CiResult.Passed, "aaa")
    assertEquals(Changes.between(open, merged), List(Change.CiFinished(CiResult.Passed, "aaa"), Change.Merged))
    assertEquals(Changes.between(open, open.copy(lifecycle = Lifecycle.Closed)), List(Change.Closed))
    assertEquals(Changes.between(open.copy(lifecycle = Lifecycle.Closed), open), List(Change.Reopened))
    assert(Changes.isFinished(merged))
    assert(!Changes.isFinished(open))
