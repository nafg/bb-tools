package forgejs

import forgecore.*
import forgecore.Snapshot.GitHubCheck
import forgecore.Snapshot.WorkflowRun as GitHubWorkflowRun

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:child_process", JSImport.Namespace)
private object ChildProcess extends js.Object:
  def execFile(
      file: String,
      args: js.Array[String],
      options: js.Any,
      callback: js.Function3[js.UndefOr[js.Dynamic], String, String, Unit]
  ): js.Any = js.native

/** A forge CLI (`gh` or `glab`) could not be run, or exited with an error. */
final class ForgeCliException(message: String) extends Exception(message)

/** Reads pull and merge requests through the `gh` and `glab` CLIs, so their existing logins are used. */
object Forges:
  private def run(command: String, args: String*): Future[String] =
    val promise = Promise[String]()
    val options = js.Dynamic.literal("timeout" -> 60000, "maxBuffer" -> 20 * 1024 * 1024)
    val _ = ChildProcess.execFile(
      command,
      js.Array(args*),
      options,
      (error: js.UndefOr[js.Dynamic], stdout: String, stderr: String) =>
        error.toOption.filter(e => e != null && !js.isUndefined(e)) match
          case None => promise.success(stdout)
          case Some(e) =>
            val detail = Option(stderr).map(_.trim).filter(_.nonEmpty).getOrElse(e.message.asInstanceOf[String])
            promise.failure(ForgeCliException(s"$command ${args.headOption.getOrElse("")} failed: $detail"))
    )
    promise.future

  private def str(v: js.Dynamic): Option[String] =
    if js.isUndefined(v) || v == null then None
    else Some(v.asInstanceOf[String]).filter(_.nonEmpty)

  private def orFail[A](e: Either[String, A]): Future[A] =
    e.fold(msg => Future.failed(ForgeCliException(msg)), Future.successful)

  private def glProject(repo: Repo): String = js.URIUtils.encodeURIComponent(repo.project)

  private def glApi(repo: Repo, path: String)(using ExecutionContext): Future[js.Dynamic] =
    run("glab", "api", "--hostname", repo.host, s"projects/${glProject(repo)}/$path").map(js.JSON.parse(_))

  private def ghApi(repo: Repo, args: String*)(using ExecutionContext): Future[js.Dynamic] =
    run("gh", (Seq("api", "--hostname", repo.host) ++ args)*).map(js.JSON.parse(_))

  /** Checks the target. For a pull or merge request, reviews and comments are fetched only when
    * [[Watch.needsDiscussion]] says so, given what was remembered before (`None` when subscribing).
    */
  def observe(target: ForgeTarget, before: Option[WatchState], force: Boolean)(using ExecutionContext): Future[Observed] =
    target match
      case ForgeTarget.Change(cr) =>
        val head = cr.repo.forge match
          case Forge.GitHub => gitHub(cr)
          case Forge.GitLab => gitLab(cr)
        head.flatMap { (snapshot, updatedAt, ciBasis) =>
          val discussion =
            if Watch.needsDiscussion(before, updatedAt, force) then discussionOf(cr).map(Some(_))
            else Future.successful(None)
          discussion.map(Observed.Change(snapshot, updatedAt, _, ciBasis))
        }
      case ForgeTarget.Branch(repo, name) => branchHead(repo, name).map(Observed.Branch(_))
      case ForgeTarget.Tags(repo)         => newestTags(repo).map(Observed.Tags(_))

  /** Runs a paginated `gh api` call whose jq filter emits one JSON object per line. */
  private def ghLines(repo: Repo, path: String, jq: String)(using ExecutionContext): Future[List[js.Dynamic]] =
    run("gh", "api", "--hostname", repo.host, "--paginate", path, "--jq", jq)
      .map(_.linesIterator.filter(_.nonEmpty).map(js.JSON.parse(_)).toList)

  private def discussionOf(cr: ChangeRequest)(using ExecutionContext): Future[List[DiscussionItem]] =
    def s(v: js.Dynamic) = str(v).getOrElse("")
    def id(v: js.Dynamic) = v.asInstanceOf[Double].toLong.toString
    cr.repo.forge match
      case Forge.GitHub =>
        val base = s"repos/${cr.repo.project}"
        val reviews = ghLines(
          cr.repo,
          s"$base/pulls/${cr.number}/reviews?per_page=100",
          ".[] | {id, login: .user.login, state, body, url: .html_url} | tojson"
        ).map(_.collect {
          // A pending review is an unsubmitted draft. A COMMENTED review with no summary only groups inline
          // comments, which are reported themselves.
          case r if s(r.state) != "PENDING" && !(s(r.state) == "COMMENTED" && s(r.body).trim.isEmpty) =>
            DiscussionItem(ItemKind.Review, id(r.id), s(r.login), s(r.body), s(r.url), reviewState = str(r.state))
        })
        val inline = ghLines(
          cr.repo,
          s"$base/pulls/${cr.number}/comments?per_page=100",
          ".[] | {id, login: .user.login, body, path, line: (.line // .original_line), url: .html_url} | tojson"
        ).map(_.map { c =>
          val line = if js.isUndefined(c.line) || c.line == null then "" else s":${id(c.line)}"
          DiscussionItem(ItemKind.InlineComment, id(c.id), s(c.login), s(c.body), s(c.url), location = Some(s(c.path) + line))
        })
        val comments = ghLines(
          cr.repo,
          s"$base/issues/${cr.number}/comments?per_page=100",
          ".[] | {id, login: .user.login, body, url: .html_url} | tojson"
        ).map(_.map(c => DiscussionItem(ItemKind.Comment, id(c.id), s(c.login), s(c.body), s(c.url))))
        val reactions = ghLines(
          cr.repo,
          s"$base/issues/${cr.number}/reactions?per_page=100",
          ".[] | {id, login: .user.login, content} | tojson"
        ).map(_.map(e => DiscussionItem(ItemKind.Reaction, id(e.id), s(e.login), s(e.content), cr.url)))
        for r <- reviews; i <- inline; c <- comments; e <- reactions yield r ++ i ++ c ++ e
      case Forge.GitLab =>
        val awards = glApi(cr.repo, s"merge_requests/${cr.number}/award_emoji?per_page=100").map { j =>
          j.asInstanceOf[js.Array[js.Dynamic]].toList.map { a =>
            DiscussionItem(ItemKind.Reaction, id(a.id), s(a.user.username), s(a.name), cr.url)
          }
        }
        // The newest 100 notes. System notes ("added 1 commit", approvals) are GitLab's own and are skipped.
        val notes = glApi(cr.repo, s"merge_requests/${cr.number}/notes?per_page=100&sort=desc&order_by=created_at").map { j =>
          j.asInstanceOf[js.Array[js.Dynamic]].toList.reverse.collect {
            case n if !n.system.asInstanceOf[Boolean] =>
              val url = s"${cr.url}#note_${id(n.id)}"
              val author = s(n.author.username)
              if s(n.`type`) == "DiffNote" && !js.isUndefined(n.position) && n.position != null then
                val path = str(n.position.new_path).orElse(str(n.position.old_path)).getOrElse("")
                val line = Seq(n.position.new_line, n.position.old_line)
                  .collectFirst { case l if !js.isUndefined(l) && l != null => s":${id(l)}" }.getOrElse("")
                DiscussionItem(ItemKind.InlineComment, id(n.id), author, s(n.body), url, location = Some(path + line))
              else DiscussionItem(ItemKind.Comment, id(n.id), author, s(n.body), url)
          }
        }
        for n <- notes; a <- awards yield n ++ a

  private def branchHead(repo: Repo, name: String)(using ExecutionContext): Future[String] =
    val head = repo.forge match
      case Forge.GitHub => ghApi(repo, s"repos/${repo.project}/branches/$name").map(j => str(j.commit.sha))
      case Forge.GitLab =>
        glApi(repo, s"repository/branches/${js.URIUtils.encodeURIComponent(name)}").map(j => str(j.commit.id))
    head.flatMap(sha => orFail(sha.toRight(s"no commit found for branch $name of ${repo.project}")))

  private val TagCount = 100

  /** The names of the most recent tags, newest first. */
  private def newestTags(repo: Repo)(using ExecutionContext): Future[List[String]] = repo.forge match
    case Forge.GitHub =>
      val Array(owner, name) = repo.project.split("/", 2)
      val query =
        s"""query($$o: String!, $$r: String!) { repository(owner: $$o, name: $$r) {
           |  refs(refPrefix: "refs/tags/", first: $TagCount, orderBy: {field: TAG_COMMIT_DATE, direction: DESC}) {
           |    nodes { name } } } }""".stripMargin
      ghApi(repo, "graphql", "-f", s"query=$query", "-F", s"o=$owner", "-F", s"r=$name").flatMap { j =>
        val repository = j.data.repository
        if js.isUndefined(repository) || repository == null then orFail(Left(s"repository ${repo.project} not found"))
        else Future.successful(repository.refs.nodes.asInstanceOf[js.Array[js.Dynamic]].toList.flatMap(n => str(n.name)))
      }
    case Forge.GitLab =>
      glApi(repo, s"repository/tags?order_by=updated&per_page=$TagCount")
        .map(_.asInstanceOf[js.Array[js.Dynamic]].toList.flatMap(t => str(t.name)))

  /** The commits reachable from `to` but not from `from`, if the forge can say. */
  def commitsBetween(repo: Repo, from: String, to: String)(using ExecutionContext): Future[CommitsSummary] =
    def subject(message: js.Dynamic) = str(message).getOrElse("").linesIterator.nextOption().getOrElse("")
    repo.forge match
      case Forge.GitHub =>
        ghApi(repo, s"repos/${repo.project}/compare/$from...$to").map { j =>
          val commits = j.commits.asInstanceOf[js.Array[js.Dynamic]].toList
          // GitHub lists oldest first; newest first reads better in a message.
          CommitsSummary(j.total_commits.asInstanceOf[Double].toInt, commits.reverse.map(c => subject(c.commit.message)))
        }
      case Forge.GitLab =>
        glApi(repo, s"repository/compare?from=$from&to=$to").map { j =>
          val commits = j.commits.asInstanceOf[js.Array[js.Dynamic]].toList
          CommitsSummary(commits.size, commits.reverse.map(c => subject(c.title)))
        }

  /** The head commit's GitHub Actions workflow runs. Runs held for a maintainer's approval appear only here. */
  private def workflowRuns(repo: Repo, sha: String)(using ExecutionContext): Future[List[GitHubWorkflowRun]] =
    ghLines(repo, s"repos/${repo.project}/actions/runs?head_sha=$sha&per_page=100",
      ".workflow_runs[] | {name, status, conclusion} | tojson"
    ).map(_.map(r => GitHubWorkflowRun(str(r.name).getOrElse(""), str(r.status).getOrElse(""), str(r.conclusion))))

  /** The request's state, its last-updated time, and which checks the CI result counted. */
  private def gitHub(cr: ChangeRequest)(using ExecutionContext): Future[(Snapshot, String, String)] =
    run("gh", "pr", "view", cr.url, "--json", "state,headRefOid,statusCheckRollup,updatedAt").flatMap { out =>
      val json = js.JSON.parse(out)
      val checks = json.statusCheckRollup.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].toOption
        .filter(_ != null).map(_.toSeq).getOrElse(Nil).map { c =>
          if str(c.__typename).contains("StatusContext") then
            GitHubCheck.StatusContext(str(c.context).getOrElse(""), str(c.state).getOrElse(""))
          else
            // A workflow's job names repeat across workflows, so the workflow is part of the identity.
            val name = s"${str(c.workflowName).getOrElse("")}/${str(c.name).getOrElse("")}"
            GitHubCheck.CheckRun(name, str(c.startedAt), str(c.status).getOrElse(""), str(c.conclusion))
        }
      val sha = str(json.headRefOid).getOrElse("")
      orFail(
        for
          lifecycle <- Snapshot.lifecycleFromGitHub(str(json.state).getOrElse(""))
          ci        <- Snapshot.ciFromGitHub(checks)
        yield (lifecycle, ci)
      ).flatMap { (lifecycle, fromChecks) =>
        // Checks alone can look passed while workflow runs wait for approval; look at the runs before saying so.
        val runs =
          if fromChecks == CiResult.Passed || fromChecks == CiResult.NoCi then workflowRuns(cr.repo, sha)
          else Future.successful(Nil)
        runs.map { rs =>
          val ci = Snapshot.withWorkflowRuns(fromChecks, rs)
          (Snapshot(lifecycle, ci, sha), str(json.updatedAt).getOrElse(""), Snapshot.describeGitHubChecks(checks, rs))
        }
      }
    }

  /** The request's state, its last-updated time, and which pipeline the CI result counted. */
  private def gitLab(cr: ChangeRequest)(using ExecutionContext): Future[(Snapshot, String, String)] =
    glApi(cr.repo, s"merge_requests/${cr.number}").flatMap { json =>
      val mrSha = str(json.sha).getOrElse("")
      val p     = json.head_pipeline
      val pipeline =
        if js.isUndefined(p) || p == null then None
        else
          Some(
            Snapshot.GitLabPipeline(str(p.status).getOrElse(""), str(p.sha).getOrElse(""), str(p.ref).getOrElse(""))
          )
      orFail(
        for
          lifecycle <- Snapshot.lifecycleFromGitLab(str(json.state).getOrElse(""))
          ci        <- Snapshot.ciFromGitLab(pipeline, mrSha)
        yield (
          Snapshot(lifecycle, ci, mrSha),
          str(json.updated_at).getOrElse(""),
          pipeline.fold("")(p => s"head pipeline, ${p.status}${if p.sha != mrSha then s" (for commit ${p.sha.take(10)})" else ""}")
        )
      )
    }

/** A forge plugin's source: recognizes its forge's targets and checks them for the subscriptions plugin. */
object ForgeSource:
  def register(bb: bbplugin.BbApi, forge: Forge)(using ExecutionContext): Unit =
    subscriptionsourcerpc.SourceServer.register(
      bb,
      description = ForgeTarget.supported(forge),
      recognize = ForgeTarget.recognize(_, forge),
      check = request => check(request)
    )

  private def check(request: subscriptionsource.CheckRequest)(using ExecutionContext): Future[subscriptionsource.CheckResult] =
    ForgeTarget.parse(request.url) match
      case None => Future.failed(ForgeCliException(s"not a forge target: ${request.url}"))
      case Some(target) =>
        val previous = request.previous match
          case None    => Right(None)
          case Some(s) => WatchState.decode(s).map(Some(_))
        previous match
          case Left(e) => Future.failed(ForgeCliException(e))
          case Right(before) =>
            Forges.observe(target, before, request.force).flatMap { now =>
              commitsFor(target, before, now).map(Watch.check(target, before, now, request.subscriberThreadId, _))
            }

  /** The commits a branch move brought, when the forge can list them. A failure to list them leaves the message
    * without them rather than failing the check.
    */
  private def commitsFor(target: ForgeTarget, before: Option[WatchState], now: Observed)(using
      ExecutionContext
  ): Future[Option[CommitsSummary]] =
    (target, before, now) match
      case (ForgeTarget.Branch(repo, _), Some(WatchState.Branch(from)), Observed.Branch(to)) if from != to =>
        Forges.commitsBetween(repo, from, to).map(Some(_)).recover { case _ => None }
      case _ => Future.successful(None)
