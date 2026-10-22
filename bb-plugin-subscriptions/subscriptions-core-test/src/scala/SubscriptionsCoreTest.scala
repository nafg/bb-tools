package subscriptionscore

import subscriptionscore.Snapshot.GitHubCheck

class SubscriptionsCoreTest extends munit.FunSuite:
  private val slick  = Repo(Forge.GitHub, "github.com", "slick/slick")
  private val config = Repo(Forge.GitLab, "gitlab.com", "chesednow/config")

  test("parses GitHub pull request URLs, ignoring tabs, queries and fragments"):
    val expected = Right(Target.Change(ChangeRequest(slick, 3909)))
    assertEquals(Target.parse("https://github.com/slick/slick/pull/3909"), expected)
    assertEquals(Target.parse("https://github.com/slick/slick/pull/3909/files?w=1"), expected)
    assertEquals(Target.parse(" https://github.com/slick/slick/pull/3909#discussion "), expected)

  test("parses GitLab merge request URLs with nested groups"):
    assertEquals(
      Target.parse("https://gitlab.com/chesednow/sub/disp/-/merge_requests/530/diffs"),
      Right(Target.Change(ChangeRequest(Repo(Forge.GitLab, "gitlab.com", "chesednow/sub/disp"), 530)))
    )

  test("parses branch and tags URLs, including branch names with slashes"):
    assertEquals(Target.parse("https://gitlab.com/chesednow/config/-/tree/main"), Right(Target.Branch(config, "main")))
    assertEquals(Target.parse("https://gitlab.com/chesednow/config/-/commits/main"), Right(Target.Branch(config, "main")))
    assertEquals(Target.parse("https://github.com/slick/slick/tree/bb/fix-x"), Right(Target.Branch(slick, "bb/fix-x")))
    assertEquals(Target.parse("https://github.com/slick/slick/tags"), Right(Target.Tags(slick)))
    assertEquals(Target.parse("https://gitlab.com/chesednow/config/-/tags/"), Right(Target.Tags(config)))

  test("rejects URLs it can't watch"):
    assert(Target.parse("https://github.com/slick/slick/issues/1").isLeft)
    assert(Target.parse("https://github.com/slick/slick").isLeft)
    assert(Target.parse("slick/slick#1").isLeft)

  test("every target's URL parses back to it"):
    val targets = List(
      Target.Change(ChangeRequest(config, 530)),
      Target.Change(ChangeRequest(slick, 1)),
      Target.Branch(config, "feature/x"),
      Target.Branch(slick, "main"),
      Target.Tags(config),
      Target.Tags(slick)
    )
    targets.foreach(t => assertEquals(Target.parse(t.url), Right(t)))
    assertEquals(ChangeRequest(config, 530).label, "chesednow/config!530")

  test("stored states round-trip"):
    val states = List(
      WatchState.Change(Snapshot(Lifecycle.Open, CiResult.Pending, "abc")),
      WatchState.Branch("abc"),
      WatchState.Tags(List("v2", "v1")),
      WatchState.Tags(Nil)
    )
    states.foreach(st => assertEquals(WatchState.decode(st.encode), Right(st)))
    assert(WatchState.decode("change Open Nope abc").isLeft)

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
  private def between(a: Snapshot, b: Snapshot) = Watch.between(WatchState.Change(a), WatchState.Change(b))

  test("a finished CI result is reported once per commit"):
    val passed = open.copy(ci = CiResult.Passed)
    assertEquals(between(open, passed), List(Event.CiFinished(CiResult.Passed, "aaa")))
    assertEquals(between(passed, passed), Nil)
    val newCommitPassed = passed.copy(headSha = "bbb")
    assertEquals(between(passed, newCommitPassed), List(Event.CiFinished(CiResult.Passed, "bbb")))

  test("pending and missing CI are not reported"):
    assertEquals(between(open.copy(ci = CiResult.Passed), open.copy(headSha = "bbb")), Nil)
    assertEquals(between(open, open.copy(ci = CiResult.NoCi)), Nil)

  test("merge, close and reopen are reported, CI first"):
    val merged = Snapshot(Lifecycle.Merged, CiResult.Passed, "aaa")
    assertEquals(between(open, merged), List(Event.CiFinished(CiResult.Passed, "aaa"), Event.Merged))
    assertEquals(between(open, open.copy(lifecycle = Lifecycle.Closed)), List(Event.Closed))
    assertEquals(between(open.copy(lifecycle = Lifecycle.Closed), open), List(Event.Reopened))
    assert(Watch.isFinished(WatchState.Change(merged)))
    assert(!Watch.isFinished(WatchState.Change(open)))

  test("a branch reports each move, and never finishes"):
    assertEquals(Watch.between(WatchState.Branch("a"), WatchState.Branch("b")), List(Event.BranchMoved("a", "b")))
    assertEquals(Watch.between(WatchState.Branch("a"), WatchState.Branch("a")), Nil)
    assert(!Watch.isFinished(WatchState.Branch("a")))

  test("new tags are reported, deleted ones are not"):
    val before = WatchState.Tags(List("v2", "v1"))
    assertEquals(Watch.between(before, WatchState.Tags(List("v3", "v2", "v1"))), List(Event.NewTags(List("v3"))))
    assertEquals(Watch.between(before, WatchState.Tags(List("v1"))), Nil)

  test("branch moves list commit subjects, capped"):
    val target = Target.Branch(config, "main")
    val event  = Event.BranchMoved("aaaaaaaaaaaa", "bbbbbbbbbbbb")
    assertEquals(Watch.describe(target, event), "main on chesednow/config moved from aaaaaaaaaa to bbbbbbbbbb.")
    assertEquals(
      Watch.describe(target, event, Some(CommitsSummary(1, List("Fix it")))),
      "main on chesednow/config moved from aaaaaaaaaa to bbbbbbbbbb. 1 new commit: Fix it."
    )
    val many = Watch.describe(target, event, Some(CommitsSummary(12, (1 to 12).map(i => s"c$i").toList)))
    assert(many.endsWith("c9; c10 and 2 more."), many)
