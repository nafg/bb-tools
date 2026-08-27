package wbs

import scala.scalajs.js

/** Typed views of what crosses the plugin boundary. Shapes follow the SDK's
  * TypeScript declarations (`PluginFileOpenerSource`, `bb.cli` run context,
  * and the `bb.sdk` results this plugin reads); nullable and optional fields
  * are typed as such so every case has to be handled.
  */
@js.native
trait OpenerSource extends js.Object:
  val kind: String                              = js.native // "workspace" | "host" | "thread-storage"
  val threadId: String | Null                   = js.native
  val environmentId: String | Null              = js.native
  val projectId: String | Null                  = js.native
  val experimental_hostId: js.UndefOr[String]   = js.native

@js.native
trait RenderInput extends js.Object:
  val path: String              = js.native
  val source: OpenerSource      = js.native
  val depth: js.UndefOr[Double] = js.native

@js.native
trait CliContext extends js.Object:
  val cwd: js.UndefOr[String]      = js.native
  val threadId: js.UndefOr[String] = js.native

@js.native
trait FileContent extends js.Object:
  val content: String         = js.native
  val contentEncoding: String = js.native // "utf8" | "base64"

@js.native
trait ThreadInfo extends js.Object:
  val projectId: String            = js.native
  val environmentId: String | Null = js.native

@js.native
trait EnvironmentInfo extends js.Object:
  val hostId: String = js.native

@js.native
trait StorageLocation extends js.Object:
  val hostId: String          = js.native
  val storageRootPath: String = js.native

extension [A](value: A | Null) def nonNull: Option[A] = if value == null then None else Some(value.asInstanceOf[A])
