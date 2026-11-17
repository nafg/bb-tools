package mavencentral

import bbplugin.*
import mavencore.*
import subscriptionsource.{CheckRequest, CheckResult}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

private def check(request: CheckRequest): Future[CheckResult] =
  MavenArtifact.parse(request.url) match
    case None => Future.failed(MavenCentralException(s"not a Maven Central artifact: ${request.url}"))
    case Some(artifact) =>
      request.previous.map(MavenState.decode) match
        case Some(Left(e)) => Future.failed(MavenCentralException(e))
        case stored =>
          val previous = stored.flatMap(_.toOption)
          val now = artifact.version match
            case Some(v) => MavenCentral.isAvailable(artifact.group, artifact.artifact, v).map(MavenState.Version(_))
            case None    => MavenCentral.versions(artifact.group, artifact.artifact).map(MavenState.Versions(_))
          now.map(MavenWatch.check(artifact, previous, _))

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit =
  subscriptionsourcerpc.SourceServer.register(bb, MavenArtifact.supported, MavenArtifact.recognize, check)
