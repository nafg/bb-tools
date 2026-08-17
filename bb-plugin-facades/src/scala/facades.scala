package bbplugin

import scala.scalajs.js

// Handwritten facades for the subset of the bb plugin API this plugin uses.
// Authoritative signatures: types/bb-plugin-sdk.d.ts (regenerate with `bb plugin types`).

@js.native
trait BbApi extends js.Object:
  val pluginId: String              = js.native
  val log: BbLog                    = js.native
  val settings: BbSettingsArea      = js.native
  val storage: BbStorageArea        = js.native
  val background: BbBackgroundArea  = js.native
  val cli: BbCliArea                = js.native
  val status: BbStatusArea          = js.native
  val ui: BbUiArea                  = js.native
  val sdk: BbSdk                    = js.native

@js.native
trait BbLog extends js.Object:
  def debug(message: String): Unit = js.native
  def info(message: String): Unit  = js.native
  def warn(message: String): Unit  = js.native
  def error(message: String): Unit = js.native

@js.native
trait BbSettingsArea extends js.Object:
  def define(descriptors: js.Any): BbSettingsHandle = js.native

@js.native
trait BbSettingsHandle extends js.Object:
  def get(): js.Promise[js.Dynamic] = js.native

@js.native
trait BbStorageArea extends js.Object:
  def database(): SqliteDb                                       = js.native
  def migrate(db: SqliteDb, statements: js.Array[String]): Unit  = js.native

@js.native
trait SqliteDb extends js.Object:
  def prepare(sql: String): SqliteStmt = js.native

@js.native
trait SqliteStmt extends js.Object:
  def run(params: js.Any*): js.Dynamic                = js.native
  def get(params: js.Any*): js.UndefOr[js.Dynamic]    = js.native
  def all(params: js.Any*): js.Array[js.Dynamic]      = js.native

@js.native
trait BbBackgroundArea extends js.Object:
  def schedule(name: String, cron: String, handler: js.Function0[js.Promise[Unit]]): Unit = js.native

@js.native
trait BbCliArea extends js.Object:
  def register(registration: js.Any): Unit = js.native

@js.native
trait BbStatusArea extends js.Object:
  def needsConfiguration(message: String): Unit = js.native

@js.native
trait BbUiArea extends js.Object:
  /** Blocks until the app submits or cancels the plugin-owned composer form. */
  def requestInput(request: js.Any): js.Promise[js.Dynamic]                  = js.native
  def requestInput(request: js.Any, options: js.Any): js.Promise[js.Dynamic] = js.native

@js.native
trait BbSdk extends js.Object:
  val threads: BbThreadsArea           = js.native
  val environments: BbEnvironmentsArea = js.native
  val files: BbFilesArea               = js.native
  val projects: BbProjectsArea         = js.native

@js.native
trait BbThreadsArea extends js.Object:
  def get(args: js.Any): js.Promise[js.Dynamic]       = js.native
  def send(args: js.Any): js.Promise[js.Dynamic]      = js.native
  def spawn(args: js.Any): js.Promise[js.Dynamic]     = js.native
  def unarchive(args: js.Any): js.Promise[js.Dynamic] = js.native

@js.native
trait BbEnvironmentsArea extends js.Object:
  def get(args: js.Any): js.Promise[js.Dynamic] = js.native

@js.native
trait BbFilesArea extends js.Object:
  def read(args: js.Any): js.Promise[js.Dynamic]  = js.native
  def write(args: js.Any): js.Promise[js.Dynamic] = js.native

@js.native
trait BbProjectsArea extends js.Object:
  def list(args: js.Any): js.Promise[js.Array[js.Dynamic]] = js.native
