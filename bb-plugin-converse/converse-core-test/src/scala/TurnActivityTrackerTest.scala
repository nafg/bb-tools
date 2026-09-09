package conversecore

import AgentActivity.*
import TurnEvent.*

class TurnActivityTrackerTest extends munit.FunSuite:
  private def activityAfter(events: TurnEvent*): Option[AgentActivity] = TurnActivityTracker.after(events).activity

  test("nothing is known before the turn's first event"):
    assertEquals(TurnActivityTracker.initial.activity, None)

  test("a turn opens with reasoning"):
    assertEquals(activityAfter(TurnStarted), Some(Reasoning))

  test("each item kind maps to its activity"):
    assertEquals(activityAfter(TurnStarted, ItemStarted("i1", "reasoning")), Some(Reasoning))
    assertEquals(activityAfter(TurnStarted, ItemStarted("i1", "agentMessage")), Some(Generating))
    for kind <- List("toolCall", "commandExecution", "fileRead", "fileChange", "webSearch", "webFetch", "imageView", "search") do
      assertEquals(activityAfter(TurnStarted, ItemStarted("i1", kind)), Some(Tool), kind)

  test("a tool completing keeps the activity until the next item says otherwise"):
    val tracker = TurnActivityTracker.after(List(TurnStarted, ItemStarted("i1", "commandExecution"), ItemCompleted("i1")))
    assertEquals(tracker.activity, Some(Tool))
    assertEquals(tracker.openItems, Nil)
    assertEquals(tracker(ItemStarted("i2", "reasoning")).activity, Some(Reasoning))

  test("a nested item completing falls back to the enclosing open item"):
    assertEquals(
      activityAfter(
        TurnStarted,
        ItemStarted("parent", "toolCall"),
        ItemStarted("child", "agentMessage"),
        ItemCompleted("child")
      ),
      Some(Tool)
    )

  test("an item kind with no activity of its own keeps the previous activity"):
    assertEquals(activityAfter(TurnStarted, ItemStarted("i1", "agentMessage"), ItemStarted("i2", "plan")), Some(Generating))
    assertEquals(activityAfter(TurnStarted, ItemStarted("i1", "toolCall"), ItemCompleted("never-started")), Some(Tool))

  test("completing an unknown item is a no-op"):
    assertEquals(activityAfter(TurnStarted, ItemCompleted("i9")), Some(Reasoning))

  test("the turn completing clears everything"):
    val tracker = TurnActivityTracker.after(List(TurnStarted, ItemStarted("i1", "toolCall"), TurnCompleted))
    assertEquals(tracker, TurnActivityTracker.initial)

  test("activity names round-trip through the signal wire"):
    for activity <- AgentActivity.values do
      assertEquals(AgentActivity.fromSignalName(activity.signalName), Some(activity))
    assertEquals(AgentActivity.fromSignalName("none"), None)
