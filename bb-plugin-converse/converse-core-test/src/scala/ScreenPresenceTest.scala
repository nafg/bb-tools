package conversecore

class ScreenPresenceTest extends munit.FunSuite:
  test("a visible, focused page with recent input means the user is at the screen"):
    assert(ScreenPresence.atScreen(pageVisible = true, pageFocused = true, msSinceInput = 30_000))

  test("input longer ago than the window means the user may have walked away"):
    assert(!ScreenPresence.atScreen(pageVisible = true, pageFocused = true, msSinceInput = ScreenPresence.RecentInputMs + 1))

  test("a hidden or unfocused page means the user is not at this screen"):
    assert(!ScreenPresence.atScreen(pageVisible = false, pageFocused = true, msSinceInput = 0))
    assert(!ScreenPresence.atScreen(pageVisible = true, pageFocused = false, msSinceInput = 0))
