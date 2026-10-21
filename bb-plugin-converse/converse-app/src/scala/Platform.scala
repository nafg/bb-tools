package converseapp

import scala.scalajs.js

object Platform:
  /** The page runs on Android. */
  val isAndroid: Boolean = js.Dynamic.global.navigator.userAgent.asInstanceOf[String].contains("Android")
