package conversecore

/** Pure selection policy for persisted per-thread voice assignments. Storage is
  * deliberately owned by the plugin backend; this object only decides which
  * enabled voice should receive a thread that is not assigned yet.
  */
object ThreadVoiceAssignments:
  def chooseVoice(
      threadId: String,
      enabledVoices: Vector[String],
      assignmentCounts: Map[String, Int]
  ): Option[String] =
    val voices = enabledVoices.map(_.trim).filter(_.nonEmpty).distinct
    if threadId == null || voices.isEmpty then None
    else
      val leastUsed = voices.map(voice => assignmentCounts.getOrElse(voice, 0)).min
      val candidates = voices.filter(voice => assignmentCounts.getOrElse(voice, 0) == leastUsed)
      val index = (threadId.hashCode & Int.MaxValue) % candidates.length
      Some(candidates(index))
