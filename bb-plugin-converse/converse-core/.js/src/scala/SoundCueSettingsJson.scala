package conversecore

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

/** The unchecked sound-cue settings as the plain JSON object the RPCs and the
  * settings store carry. Reading never fails: anything that is not a finite
  * number is read as absent, for `SoundCueSettings.fromUnchecked` to judge.
  */
object SoundCueSettingsJson:
  import SoundCueSettings.Unchecked

  def read(value: js.Dynamic): Unchecked =
    Unchecked(
      number(value.volume),
      obj(value.cues).fold(Map.empty) { cues =>
        cues.asInstanceOf[js.Dictionary[js.Dynamic]].map((key, cue) => key -> readCue(cue)).toMap
      }
    )

  def readCue(value: js.Dynamic): Unchecked.Cue =
    Unchecked.Cue(
      enabled = value.enabled.asInstanceOf[Any] == true,
      notes = value.notes.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].fold(Nil)(_.toList.map(readNote)),
      repeat = obj(value.repeat).map(r => Unchecked.Repeat(number(r.leadInMs), number(r.intervalMs)))
    )

  def readNote(value: js.Dynamic): Unchecked.Note =
    Unchecked.Note(
      frequencyHz = number(value.frequencyHz),
      durationSec = number(value.durationSec),
      attackSec = number(value.attackSec),
      gain = number(value.gain),
      releaseSec = number(value.releaseSec),
      harmonic2 = number(value.harmonic2),
      pinkNoise = number(value.pinkNoise),
      lowpassHz = number(value.lowpassHz)
    )

  def write(unchecked: Unchecked): js.Dynamic =
    withPresent(
      "volume" -> unchecked.volume,
      "cues"   -> Some(js.Dictionary(unchecked.cues.toSeq.map((key, cue) => key -> writeCue(cue))*))
    )

  def writeCue(cue: Unchecked.Cue): js.Dynamic =
    js.Dynamic.literal(
      "enabled" -> cue.enabled,
      "notes"   -> cue.notes.map(writeNote).toJSArray,
      "repeat" -> cue.repeat.fold(null: js.Any)(r =>
        withPresent("leadInMs" -> r.leadInMs, "intervalMs" -> r.intervalMs)
      )
    )

  def writeNote(note: Unchecked.Note): js.Dynamic =
    withPresent(
      "frequencyHz" -> note.frequencyHz,
      "durationSec" -> note.durationSec,
      "attackSec"   -> note.attackSec,
      "gain"        -> note.gain,
      "releaseSec"  -> note.releaseSec,
      "harmonic2"   -> note.harmonic2,
      "pinkNoise"   -> note.pinkNoise,
      "lowpassHz"   -> note.lowpassHz
    )

  // Absent values are left out entirely: bb's RPC layer rejects `undefined` as non-JSON.
  private def withPresent(fields: (String, Option[Any])*): js.Dynamic =
    val out = js.Dynamic.literal()
    fields.foreach((key, value) => value.foreach(v => out.updateDynamic(key)(v.asInstanceOf[js.Any])))
    out

  private def number(value: js.Dynamic): Option[Double] =
    Option.when(js.typeOf(value) == "number")(value.asInstanceOf[Double]).filter(d => !d.isNaN && !d.isInfinite)

  private def obj(value: js.Dynamic): Option[js.Dynamic] =
    Option.when(value != null && js.typeOf(value) == "object")(value)
