package conversecore

import ReplyInterruption.*

class ReplyInterruptionTest extends munit.FunSuite:
  test("interrupting your own thread's reply tells that thread where it was cut off"):
    val notes = ReplyInterruption.notes("a", Some(SpeakingReply("a", " Second sentence. ")), targetStillThinking = false)
    assertEquals(notes.map(_.threadId), List("a"))
    assert(notes.head.text.contains("“Second sentence.”"), notes.head.text)
    assert(notes.head.text.contains("not read aloud"))

  test("interrupting before the first sentence had started says none of it was read"):
    val notes = ReplyInterruption.notes("a", Some(SpeakingReply("a", "")), targetStillThinking = false)
    assertEquals(notes.map(_.text), List("The user interrupted this reply as it was about to be read aloud; none of it was read aloud."))

  test("speaking before the reply was read aloud is noted on the utterance's thread"):
    assertEquals(
      ReplyInterruption.notes("a", None, targetStillThinking = true),
      List(Note("a", "Spoken before the previous reply was read aloud."))
    )

  test("speaking after the reply finished adds nothing"):
    assertEquals(ReplyInterruption.notes("a", None, targetStillThinking = false), Nil)

  test("interrupting another thread's reply notes the interrupted thread, not the utterance's"):
    val notes = ReplyInterruption.notes("b", Some(SpeakingReply("a", "Still talking.")), targetStillThinking = false)
    assertEquals(notes.map(_.threadId), List("a"))

  test("interrupting another thread's reply while your own is still thinking produces both notes"):
    val notes = ReplyInterruption.notes("b", Some(SpeakingReply("a", "Still talking.")), targetStillThinking = true)
    assertEquals(notes.map(_.threadId), List("a", "b"))

  test("notes come before the transcript, in the order things happened"):
    assertEquals(
      ReplyInterruption.heardText("yes go on", List(Note("a", "Spoken before the previous reply was read aloud."))),
      "(Spoken before the previous reply was read aloud.)\nyes go on"
    )
    assertEquals(ReplyInterruption.heardText("yes go on", Nil), "yes go on")
    assertEquals(ReplyInterruption.preface(Nil), None)
