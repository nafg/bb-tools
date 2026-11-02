package conversecore

/** A speech synthesis provider whose local server holds GPU memory while it
  * runs. The 8 GB card is shared with the desktop (which has frozen with about
  * 1.45 GB free), so none of these fits beside Parakeet on the GPU.
  */
enum GpuTtsProvider(val id: String):
  case Qwen3Tts       extends GpuTtsProvider("qwen3-tts")
  case ChatterboxNano extends GpuTtsProvider("chatterbox-nano")

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
