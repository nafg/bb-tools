package billing

import bbplugin.Dyn.str

import scala.scalajs.js

/** A dynamic value read as a flag: true only when it is exactly `true`. Never cast a dynamic value with
  * `asInstanceOf[Boolean]` (or any primitive): an undefined value makes that a Scala.js UndefinedBehaviorError, which
  * is fatal, escapes every Future and `recover`, and takes the whole bb server down with it.
  */
def bool(value: js.Dynamic): Boolean = value.asInstanceOf[Any] == true

def errorMessage(e: Throwable): String =
  e match
    case js.JavaScriptException(err) =>
      val d = err.asInstanceOf[js.Dynamic]
      Option(str(d.message)).filter(_.nonEmpty).getOrElse(err.toString)
    case other                       => Option(other.getMessage).getOrElse(other.toString)
