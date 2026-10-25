package conversecore

import ReplyInterruption.*

class ReplyInterruptionTest extends munit.FunSuite:
  test("interrupting your own thread's reply tells that thread where it was cut off"):
    val notes = ReplyInterruption.notes("a", Some(SpeakingReply("a", " Second sentence. ", atScreen = false)), targetReplyUnheard = false)
    assertEquals(notes.map(_.threadId), List("a"))
    assert(notes.head.text.contains("“Second sentence.”"), notes.head.text)
    assert(notes.head.text.contains("not read aloud"))

  test("interrupting at the screen leaves out where the audio stopped"):
    val notes = ReplyInterruption.notes("a", Some(SpeakingReply("a", "Second sentence.", atScreen = true)), targetReplyUnheard = false)
    assertEquals(
      notes.map(_.text),
      List("The user interrupted this reply while it was being read aloud; they were at the screen, so they may have read all of it.")
    )

  test("interrupting before the first sentence had started says none of it was read"):
    val notes = ReplyInterruption.notes("a", Some(SpeakingReply("a", "", atScreen = false)), targetReplyUnheard = false)
    assertEquals(notes.map(_.text), List("The user interrupted this reply as it was about to be read aloud; none of it was read aloud."))

  test("speaking before the reply was read aloud is noted on the utterance's thread"):
    assertEquals(
      ReplyInterruption.notes("a", None, targetReplyUnheard = true),
      List(Note("a", "Spoken before the previous reply was read aloud."))
    )

  test("speaking after the reply finished adds nothing"):
    assertEquals(ReplyInterruption.notes("a", None, targetReplyUnheard = false), Nil)

  test("interrupting another thread's reply notes the interrupted thread, not the utterance's"):
    val notes = ReplyInterruption.notes("b", Some(SpeakingReply("a", "Still talking.", atScreen = false)), targetReplyUnheard = false)
    assertEquals(notes.map(_.threadId), List("a"))

  test("interrupting another thread's reply while your own is still thinking produces both notes"):
    val notes = ReplyInterruption.notes("b", Some(SpeakingReply("a", "Still talking.", atScreen = false)), targetReplyUnheard = true)
    assertEquals(notes.map(_.threadId), List("a", "b"))

  test("notes come before the transcript, in the order things happened"):
    assertEquals(
      ReplyInterruption.heardText("yes go on", List(Note("a", "Spoken before the previous reply was read aloud."))),
      "(Spoken before the previous reply was read aloud.)\nyes go on"
    )
    assertEquals(ReplyInterruption.heardText("yes go on", Nil), "yes go on")
    assertEquals(ReplyInterruption.preface(Nil), None)

  test("speech continuing a message the agent waits on is not 'spoken before the reply', keeps other notes, and says it continues"):
    val carried = Note("a", "The user interrupted this reply while it was being read aloud, during: “x”.")
    val notes   = carried :: ReplyInterruption.notes("a", None, targetReplyUnheard = true)
    assertEquals(
      ReplyInterruption.forContinuation("a", notes),
      List(carried, Note("a", "Continues the previous voice message, which you waited for more of."))
    )
