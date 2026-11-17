package forgegitlab

import bbplugin.*
import forgecore.Forge

import scala.concurrent.ExecutionContext
import scala.scalajs.js.annotation.JSExportTopLevel

given ExecutionContext = ExecutionContext.parasitic

@JSExportTopLevel("default")
def plugin(bb: BbApi): Unit = forgejs.ForgeSource.register(bb, Forge.GitLab)
