package bbwindow

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

object Main:
  private val bbUrl  = "http://127.0.0.1:38886"
  private val bbUrlC = c"http://127.0.0.1:38886"

  private val GTK_WINDOW_TOPLEVEL                        = 0
  private val WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION = 0
  private val WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION = 1

  private def isBbUrl(uri: CString): Boolean =
    fromCString(uri).startsWith(bbUrl)

  private def openExternally(uri: CString): Unit =
    Gtk.gtk_show_uri_on_window(null, uri, 0.toUInt, null): Unit

  private def decisionUri(decision: GObjectPtr): CString =
    val action  = WebKit.webkit_navigation_policy_decision_get_navigation_action(decision)
    val request = WebKit.webkit_navigation_action_get_request(action)
    WebKit.webkit_uri_request_get_uri(request)

  // NAVIGATION_ACTION also fires for subframe loads, and WebKitNavigationAction has no
  // main-frame check, so external subframe navigations are redirected out too.
  private val onDecidePolicy: DecidePolicyCallback =
    CFuncPtr4.fromScalaFunction { (_, decision, decisionType, _) =>
      decisionType match
        case WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION if !isBbUrl(decisionUri(decision)) =>
          WebKit.webkit_policy_decision_ignore(decision)
          openExternally(decisionUri(decision))
          1
        case WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION =>
          WebKit.webkit_policy_decision_ignore(decision)
          openExternally(decisionUri(decision))
          1
        case _ =>
          0
    }

  private val onDestroy: DestroyCallback =
    CFuncPtr2.fromScalaFunction((_, _) => Gtk.gtk_main_quit())

  def main(args: Array[String]): Unit =
    GLib.g_set_prgname(c"bb-window")
    Gtk.gtk_init(null, null)

    val window = Gtk.gtk_window_new(GTK_WINDOW_TOPLEVEL)
    Gtk.gtk_window_set_title(window, c"bb")
    Gtk.gtk_window_set_default_size(window, 1440, 960)

    val webView = WebKit.webkit_web_view_new()
    Gtk.gtk_container_add(window, webView)

    GObject.g_signal_connect_decide_policy(webView, c"decide-policy", onDecidePolicy, null, null, 0.toUInt)
    GObject.g_signal_connect_destroy(window, c"destroy", onDestroy, null, null, 0.toUInt)

    WebKit.webkit_web_view_load_uri(webView, bbUrlC)
    Gtk.gtk_widget_show_all(window)
    Gtk.gtk_main()
