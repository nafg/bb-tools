package subscriptions

import subscriptionscore.*
import subscriptionscore.Snapshot.GitHubCheck

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
    ChildProcess.execFile(
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

  private def glApi(repo: Repo, path: String): Future[js.Dynamic] =
    run("glab", "api", "--hostname", repo.host, s"projects/${glProject(repo)}/$path").map(js.JSON.parse(_))

  private def ghApi(repo: Repo, args: String*): Future[js.Dynamic] =
    run("gh", (Seq("api", "--hostname", repo.host) ++ args)*).map(js.JSON.parse(_))

  /** Checks the target. For a pull or merge request, reviews and comments are fetched only when
    * [[Watch.needsDiscussion]] says so, given what was remembered before (`None` when subscribing).
    */
  def observe(target: Target, before: Option[WatchState], force: Boolean)(using ExecutionContext): Future[Observed] =
    target match
      case Target.Change(cr) =>
        val head = cr.repo.forge match
          case Forge.GitHub => gitHub(cr)
          case Forge.GitLab => gitLab(cr)
        head.flatMap { (snapshot, updatedAt) =>
          val discussion =
            if Watch.needsDiscussion(before, updatedAt, force) then discussionOf(cr).map(Some(_))
            else Future.successful(None)
          discussion.map(Observed.Change(snapshot, updatedAt, _))
        }
      case Target.Branch(repo, name) => branchHead(repo, name).map(Observed.Branch(_))
      case Target.Tags(repo)         => newestTags(repo).map(Observed.Tags(_))

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
        for r <- reviews; i <- inline; c <- comments yield r ++ i ++ c
      case Forge.GitLab =>
        // The newest 100 notes. System notes ("added 1 commit", approvals) are GitLab's own and are skipped.
        glApi(cr.repo, s"merge_requests/${cr.number}/notes?per_page=100&sort=desc&order_by=created_at").map { j =>
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

  /** The request's state and its last-updated time. */
  private def gitHub(cr: ChangeRequest)(using ExecutionContext): Future[(Snapshot, String)] =
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
      orFail(
        for
          lifecycle <- Snapshot.lifecycleFromGitHub(str(json.state).getOrElse(""))
          ci        <- Snapshot.ciFromGitHub(checks)
        yield (Snapshot(lifecycle, ci, str(json.headRefOid).getOrElse("")), str(json.updatedAt).getOrElse(""))
      )
    }

  /** The request's state and its last-updated time. */
  private def gitLab(cr: ChangeRequest)(using ExecutionContext): Future[(Snapshot, String)] =
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
        yield (Snapshot(lifecycle, ci, mrSha), str(json.updated_at).getOrElse(""))
      )
    }
