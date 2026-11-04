package conversecore

/** A speech synthesis provider whose local server holds GPU memory while it
  * runs. The 8 GB card is shared with the desktop (which has frozen with about
  * 1.45 GB free), so none of these fits beside Parakeet on the GPU.
  * `wordsPerRequest` is the most words one request to it may hold, when the
  * memory a request takes grows with its length; a longer chunk is sent in
  * pieces (see [[Speech.withinWords]]).
  */
enum GpuTtsProvider(val id: String, val wordsPerRequest: Option[Int]):
  case Qwen3Tts       extends GpuTtsProvider("qwen3-tts", None)
  case ChatterboxNano extends GpuTtsProvider("chatterbox-nano", Some(30))

object GpuTtsProvider:
  /** The provider the settings call `id`, if it runs on the local GPU. */
  def named(id: String): Option[GpuTtsProvider] = values.find(_.id == id)

/** Where Parakeet runs its model. */
enum ParakeetDevice(val flag: String):
  /** The GPU when loading the model leaves it enough free memory, otherwise
    * the CPU.
    */
  case GpuIfFree extends ParakeetDevice("gpu-if-free")

  /** The CPU, leaving the GPU to speech synthesis. */
  case Cpu extends ParakeetDevice("cpu")

object ParakeetDevice:
  /** The device Parakeet is to run on while `ttsProvider` speaks: the CPU
    * when that provider holds the GPU.
    */
  def whileSpeakingWith(ttsProvider: String): ParakeetDevice =
    if GpuTtsProvider.named(ttsProvider).isDefined then Cpu else GpuIfFree
