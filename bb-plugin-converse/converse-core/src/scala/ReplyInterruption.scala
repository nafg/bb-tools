package conversecore

/** What the model is told about when an utterance was spoken relative to the
  * replies being spoken aloud: whether it cut a reply off and where, or came
  * before the reply was heard at all.
  */
object ReplyInterruption:
  /** A reply being spoken aloud when the utterance began. */
  final case class SpeakingReply(threadId: String, sentence: String)

  /** A note the model should see, and which thread it is for. */
  final case class Note(threadId: String, text: String)

  /** The notes an utterance to `utteranceThreadId` produces. `speaking` is the
    * reply playing when the utterance began, if any; `targetReplyUnheard`
    * says the utterance's own thread had not yet spoken its last reply.
    */
  def notes(utteranceThreadId: String, speaking: Option[SpeakingReply], targetReplyUnheard: Boolean): List[Note] =
    val interrupted = speaking.map { reply =>
      val sentence = reply.sentence.trim
      Note(
        reply.threadId,
        if sentence.isEmpty then "The user interrupted this reply as it was about to be read aloud; none of it was read aloud."
        else s"The user interrupted this reply while it was being read aloud, during: “$sentence”. The rest was not read aloud."
      )
    }
    val beforeHearing =
      Option.when(targetReplyUnheard && !speaking.exists(_.threadId == utteranceThreadId))(
        Note(utteranceThreadId, SpokenBeforeReply)
      )
    interrupted.toList ++ beforeHearing

  private val SpokenBeforeReply = "Spoken before the previous reply was read aloud."

  /** The notes for speech that continues a message an agent is still waiting
    * on: being the rest of that message, it comes before no reply.
    */
  def forContinuation(notesForThread: List[Note]): List[Note] =
    notesForThread.filterNot(_.text == SpokenBeforeReply)

  /** The notes as the lines that precede a transcript, in the order things happened. */
  def preface(notesForThread: List[Note]): Option[String] =
    Option.when(notesForThread.nonEmpty)(notesForThread.map(n => s"(${n.text})").mkString("\n"))

  /** A transcript with its notes before it, for places without separate visibility. */
  def heardText(transcript: String, notesForThread: List[Note]): String =
    preface(notesForThread).fold(transcript)(p => s"$p\n$transcript")
