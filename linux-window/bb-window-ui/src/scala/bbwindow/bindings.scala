package bbwindow

import scala.scalanative.unsafe.*

type GObjectPtr = Ptr[Byte]
type Gboolean   = CInt

/** gboolean (*)(WebKitWebView *, WebKitPolicyDecision *, WebKitPolicyDecisionType, gpointer) */
type DecidePolicyCallback =
  CFuncPtr4[GObjectPtr, GObjectPtr, CInt, GObjectPtr, Gboolean]

/** void (*)(GtkWidget *, gpointer) */
type DestroyCallback = CFuncPtr2[GObjectPtr, GObjectPtr, Unit]

@link("glib-2.0")
@extern
object GLib:
  def g_set_prgname(prgname: CString): Unit = extern

@link("gobject-2.0")
@extern
object GObject:
  @name("g_signal_connect_data")
  def g_signal_connect_decide_policy(
      instance: GObjectPtr,
      detailedSignal: CString,
      handler: DecidePolicyCallback,
      data: GObjectPtr,
      destroyData: GObjectPtr,
      connectFlags: CUnsignedInt
  ): CUnsignedLong = extern

  @name("g_signal_connect_data")
  def g_signal_connect_destroy(
      instance: GObjectPtr,
      detailedSignal: CString,
      handler: DestroyCallback,
      data: GObjectPtr,
      destroyData: GObjectPtr,
      connectFlags: CUnsignedInt
  ): CUnsignedLong = extern

@link("gtk-3")
@extern
object Gtk:
  def gtk_init(argc: Ptr[CInt], argv: Ptr[Ptr[Ptr[CChar]]]): Unit = extern
  def gtk_main(): Unit = extern
  def gtk_main_quit(): Unit = extern
  def gtk_window_new(windowType: CInt): GObjectPtr = extern
  def gtk_window_set_title(window: GObjectPtr, title: CString): Unit = extern
  def gtk_window_set_default_size(window: GObjectPtr, width: CInt, height: CInt): Unit = extern
  def gtk_container_add(container: GObjectPtr, widget: GObjectPtr): Unit = extern
  def gtk_widget_show_all(widget: GObjectPtr): Unit = extern
  def gtk_show_uri_on_window(
      parent: GObjectPtr,
      uri: CString,
      timestamp: CUnsignedInt,
      error: Ptr[GObjectPtr]
  ): Gboolean = extern

@link("webkit2gtk-4.1")
@extern
object WebKit:
  def webkit_web_view_new(): GObjectPtr = extern
  def webkit_web_view_load_uri(webView: GObjectPtr, uri: CString): Unit = extern
  def webkit_navigation_policy_decision_get_navigation_action(decision: GObjectPtr): GObjectPtr =
    extern
  def webkit_navigation_action_get_request(navigation: GObjectPtr): GObjectPtr = extern
  def webkit_uri_request_get_uri(request: GObjectPtr): CString = extern
  def webkit_policy_decision_ignore(decision: GObjectPtr): Unit = extern
