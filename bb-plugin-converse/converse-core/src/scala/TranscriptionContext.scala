package conversecore

object TranscriptionContext:
  val MaxPromptChars = 800

  /** Whisper-style transcription prompts describe audio immediately preceding
    * the current segment. Keep the most recent context so long utterances do
    * not grow every preview request without bound.
    */
  def promptFrom(precedingTranscript: String): String =
    precedingTranscript.trim.takeRight(MaxPromptChars)

