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
    val done    = GitHubCheck.CheckRun("a", None, "COMPLETED", Some("SUCCESS"))
    val running = GitHubCheck.CheckRun("b", None, "IN_PROGRESS", None)
    val failed  = GitHubCheck.CheckRun("c", None, "COMPLETED", Some("FAILURE"))
    val cancel  = GitHubCheck.CheckRun("c", None, "COMPLETED", Some("CANCELLED"))
    assertEquals(Snapshot.ciFromGitHub(Seq(done, running, failed)), Right(CiResult.Failed))
    assertEquals(Snapshot.ciFromGitHub(Seq(done, running, GitHubCheck.CheckRun("d", None, "COMPLETED", Some("CANCELLED")))), Right(CiResult.Canceled))
    assertEquals(Snapshot.ciFromGitHub(Seq(done, running)), Right(CiResult.Pending))
    assertEquals(Snapshot.ciFromGitHub(Seq(done, GitHubCheck.StatusContext("ci/x", "SUCCESS"))), Right(CiResult.Passed))
    assertEquals(Snapshot.ciFromGitHub(Nil), Right(CiResult.NoCi))

  test("unrecognized values are errors, not guesses"):
    assert(Snapshot.ciFromGitHub(Seq(GitHubCheck.CheckRun("a", None, "COMPLETED", Some("SOMETHING_NEW")))).isLeft)
    assert(Snapshot.ciFromGitLab(Some(Snapshot.GitLabPipeline("something_new", "s", "main")), "s").isLeft)
    assert(Snapshot.lifecycleFromGitHub("DRAFT").isLeft)

  test("only the latest attempt of a re-run GitHub check counts"):
    // From nafg/simple-router#345: a run cancelled by a newer run of the same job, which passed.
    val cancelled = GitHubCheck.CheckRun("build", Some("2026-10-22T06:42:08Z"), "COMPLETED", Some("CANCELLED"))
    val rerun     = GitHubCheck.CheckRun("build", Some("2026-10-22T06:42:10Z"), "COMPLETED", Some("SUCCESS"))
    assertEquals(Snapshot.ciFromGitHub(Seq(cancelled, rerun)), Right(CiResult.Passed))
    assertEquals(Snapshot.ciFromGitHub(Seq(rerun, cancelled)), Right(CiResult.Passed))

  private def pipeline(status: String, sha: String = "new", ref: String = "feature") =
    Some(Snapshot.GitLabPipeline(status, sha, ref))

  test("GitLab pipeline statuses"):
    assertEquals(Snapshot.ciFromGitLab(None, "new"), Right(CiResult.NoCi))
    assertEquals(Snapshot.ciFromGitLab(pipeline("running"), "new"), Right(CiResult.Pending))
    assertEquals(Snapshot.ciFromGitLab(pipeline("success"), "new"), Right(CiResult.Passed))
    assertEquals(Snapshot.ciFromGitLab(pipeline("canceled"), "new"), Right(CiResult.Canceled))

  test("a GitLab pipeline for an earlier commit is not the new commit's result"):
    assertEquals(Snapshot.ciFromGitLab(pipeline("success", sha = "old"), "new"), Right(CiResult.Pending))
    val mergedResults = pipeline("success", sha = "merge-commit", ref = "refs/merge-requests/25/merge")
    assertEquals(Snapshot.ciFromGitLab(mergedResults, "new"), Right(CiResult.Passed))

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
