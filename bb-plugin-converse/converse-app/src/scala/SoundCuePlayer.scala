package converseapp

import conversecore.*

import scala.collection.mutable
import scala.scalajs.js.timers.{SetIntervalHandle, clearInterval, setInterval}

/** Plays the session's sound cues as synthesized sine tones on its own
  * AudioContext, so a cue can still sound while the session's context is
  * being torn down. Repeating cues are scheduled ahead on the audio clock,
  * which keeps them steady when JavaScript timers are throttled.
  */
final class SoundCuePlayer:
  /** Seconds of notes queued ahead of the audio clock, and how often the queue is topped up. */
  private val LookaheadSec  = 0.3
  private val SchedulerMs   = 100.0
  private val ReleaseSec    = 0.02

  private var audioContext: AudioContext   = null
  private var master: GainNode             = null
  private var settings: SoundCueSettings   = SoundCueSettings.default

  private final case class ScheduledNote(sources: List[AudioScheduledSourceNode], envelope: GainNode)

  /** A sound queued every `intervalSec` from `nextAt` until stopped. */
  private final class Loop(sound: ToneSound, intervalSec: Double, private var nextAt: Double):
    // Only what is still queued, so it can be cut at a phase change.
    private val queued = mutable.Set.empty[ScheduledNote]
    private val timer: SetIntervalHandle = setInterval(SchedulerMs)(topUp())
    topUp()

    private def topUp(): Unit =
      while nextAt < audioContext.currentTime + LookaheadSec do
        queued ++= schedule(sound, nextAt, queued -= _)
        nextAt += intervalSec

    def stop(): Unit =
      clearInterval(timer)
      val now = audioContext.currentTime
      queued.foreach { note =>
        val _ = note.envelope.gain.cancelScheduledValues(now)
        val _ = note.envelope.gain.setValueAtTime(note.envelope.gain.value, now)
        val _ = note.envelope.gain.linearRampToValueAtTime(0, now + ReleaseSec)
        note.sources.foreach(_.stop(now + ReleaseSec))
      }
      queued.clear()

  private var repeating: SoundCue    = null
  private var repeatingLoop: Loop    = null
  private var auditionLoop: Loop     = null
  private var pinkNoiseBuffer: AudioBuffer = null

  def configure(next: SoundCueSettings): Unit =
    settings = next
    if master != null then master.gain.value = settings.volume
    if repeating != null && !settings.cues(repeating).enabled then stopRepeating()

  /** Must be called inside a user gesture at least once before any cue is
    * wanted; otherwise browsers keep the context suspended and cues are
    * silently skipped.
    */
  def ensureReady(): Unit =
    if audioContext == null then
      audioContext = new AudioContext
      master = audioContext.createGain()
      master.gain.value = settings.volume
      val _ = master.connect(audioContext.destination)
    val _ = audioContext.resume()

  def onPhase(from: String, to: String, stoppedByUser: Boolean): Unit =
    val next = SoundCue.repeatingIn(to).orNull
    if next != repeating then stopRepeating()
    val chimeEndsAt = SoundCue.onTransition(from, to, stoppedByUser).flatMap(play)
    // A repeating cue waits for the transition chime so the two never overlap.
    if next != null && next != repeating then startRepeating(next, chimeEndsAt)

  /** Plays a cue's configured sound once, regardless of its enabled setting. */
  def preview(cue: SoundCue): Unit = audition(settings.cues(cue).copy(repeat = None))

  /** Plays a setting that need not be saved, regardless of its enabled flag:
    * once, or for a repeating one at its interval (without its lead-in) until
    * `stopAudition` or the next audition.
    */
  def audition(setting: CueSetting): Unit =
    stopAudition()
    ensureReady()
    if audioContext != null then
      val startAt = audioContext.currentTime + 0.02
      setting.repeat match
        case Some(repeat) => auditionLoop = new Loop(setting.sound, repeat.intervalMs / 1000.0, startAt)
        case None         => val _ = schedule(setting.sound, startAt, _ => ())

  def stopAudition(): Unit =
    if auditionLoop != null then
      auditionLoop.stop()
      auditionLoop = null

  /** Returns the audio-clock time the cue ends, or None if it did not play. */
  private def play(cue: SoundCue): Option[Double] =
    val setting = settings.cues(cue)
    if setting.enabled && audioContext != null then
      val startAt = audioContext.currentTime + 0.02
      val _       = schedule(setting.sound, startAt, _ => ())
      Some(startAt + setting.sound.durationSec)
    else None

  private def startRepeating(cue: SoundCue, notBefore: Option[Double]): Unit =
    val setting = settings.cues(cue)
    if setting.enabled && audioContext != null then
      repeating = cue
      setting.repeat.foreach { repeat =>
        repeatingLoop = new Loop(
          setting.sound,
          repeat.intervalMs / 1000.0,
          notBefore.getOrElse(audioContext.currentTime) + repeat.leadInMs / 1000.0
        )
      }

  private def stopRepeating(): Unit =
    repeating = null
    if repeatingLoop != null then
      repeatingLoop.stop()
      repeatingLoop = null

  /** Queues the sound's notes back to back from `startAt`; returns the nodes queued. */
  private def schedule(sound: ToneSound, startAt: Double, onEnded: ScheduledNote => Unit): List[ScheduledNote] =
    var at = startAt
    sound.notes.map { note =>
      val envelope = audioContext.createGain()
      val _        = envelope.gain.setValueAtTime(0, at)
      val _        = envelope.gain.linearRampToValueAtTime(note.gain, at + note.attackSec)
      val _        = envelope.gain.setValueAtTime(note.gain, at + note.releaseStartSec)
      val _        = envelope.gain.linearRampToValueAtTime(0, at + note.durationSec)
      val _ = envelope.connect(master)

      def sine(frequencyHz: Double, level: Double): AudioScheduledSourceNode =
        val osc = audioContext.createOscillator()
        osc.frequency.value = frequencyHz
        connectAtLevel(osc, level, envelope)
        osc
      val sources =
        List(sine(note.frequencyHz, 1.0)) ++
          Option.when(note.harmonic2 > 0)(sine(note.frequencyHz * 2, note.harmonic2)) ++
          Option.when(note.pinkNoise > 0) {
            val noise = audioContext.createBufferSource()
            noise.buffer = pinkNoise()
            noise.loop = true
            val airInto: AudioNode = note.lowpassHz match
              case Some(hz) =>
                val filter = audioContext.createBiquadFilter()
                filter.`type` = "lowpass"
                filter.frequency.value = hz
                val _ = filter.connect(envelope)
                filter
              case None => envelope
            connectAtLevel(noise, note.pinkNoise, airInto)
            noise
          }
      val entry = ScheduledNote(sources, envelope)
      sources.head.onended = _ => onEnded(entry)
      sources.foreach { s =>
        s.start(at)
        s.stop(at + note.durationSec)
      }
      at += note.durationSec
      entry
    }

  private def connectAtLevel(source: AudioNode, level: Double, into: AudioNode): Unit =
    if level == 1.0 then { val _ = source.connect(into) }
    else
      val gain = audioContext.createGain()
      gain.gain.value = level
      val _ = source.connect(gain)
      val _ = gain.connect(into)

  /** Four seconds of pink noise (Paul Kellet's three-pole approximation), built once. */
  private def pinkNoise(): AudioBuffer =
    if pinkNoiseBuffer == null then
      val length = (audioContext.sampleRate * 4).toInt
      pinkNoiseBuffer = audioContext.createBuffer(1, length, audioContext.sampleRate)
      val data = pinkNoiseBuffer.getChannelData(0)
      var b0 = 0.0; var b1 = 0.0; var b2 = 0.0
      var i  = 0
      while i < length do
        val white = scala.util.Random.nextDouble() * 2 - 1
        b0 = 0.997 * b0 + 0.029 * white
        b1 = 0.985 * b1 + 0.032 * white
        b2 = 0.95 * b2 + 0.048 * white
        data(i) = ((b0 + b1 + b2) * 0.25).toFloat
        i += 1
    pinkNoiseBuffer
