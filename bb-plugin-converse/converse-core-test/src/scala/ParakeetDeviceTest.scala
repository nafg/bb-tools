package conversecore

class ParakeetDeviceTest extends munit.FunSuite:
  test("Parakeet leaves the GPU to each speech provider that runs on it"):
    assertEquals(ParakeetDevice.whileSpeakingWith("qwen3-tts"), ParakeetDevice.Cpu)
    assertEquals(ParakeetDevice.whileSpeakingWith("chatterbox-nano"), ParakeetDevice.Cpu)

  test("Parakeet may use the GPU beside every other speech provider"):
    for provider <- List("browser", "pocket-tts", "google-cloud-tts", "kokoro", "openai-compatible", "") do
      assertEquals(ParakeetDevice.whileSpeakingWith(provider), ParakeetDevice.GpuIfFree, provider)

  test("every provider that holds the GPU is found by the id the settings store"):
    for provider <- GpuTtsProvider.values do assertEquals(GpuTtsProvider.named(provider.id), Some(provider))
    assertEquals(GpuTtsProvider.named("pocket-tts"), None)

  test("the device flags are distinct, as they are part of the service's version"):
    assertEquals(ParakeetDevice.values.map(_.flag).distinct.length, ParakeetDevice.values.length)
