package conversecore

/** Whether the user seems to be at the screen of the device running the
  * session: the page is visible and focused, and they have used the mouse,
  * keyboard or touch screen within [[RecentInputMs]].
  */
object ScreenPresence:
  /** How recent input must be to count: long enough to cover reading a reply
    * without touching anything.
    */
  val RecentInputMs = 120_000.0

  def atScreen(pageVisible: Boolean, pageFocused: Boolean, msSinceInput: Double): Boolean =
    pageVisible && pageFocused && msSinceInput <= RecentInputMs
