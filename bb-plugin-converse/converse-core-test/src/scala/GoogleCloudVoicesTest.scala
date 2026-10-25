package conversecore

class GoogleCloudVoicesTest extends munit.FunSuite:
  test("a voice's language code is the locale its name starts with"):
    assertEquals(GoogleCloudVoice("en-GB-Wavenet-B", GoogleCloudVoice.Gender.Male).languageCode, "en-GB")

  test("a voice is labelled by locale, model, letter and gender"):
    assertEquals(GoogleCloudVoices.Default.label, "US English WaveNet D (male)")
    assertEquals(GoogleCloudVoices.byName("en-AU-Wavenet-C").map(_.label), Some("Australian English WaveNet C (female)"))

  test("the default is a male WaveNet voice from the catalog"):
    assert(GoogleCloudVoices.English.contains(GoogleCloudVoices.Default))
    assertEquals(GoogleCloudVoices.Default.gender, GoogleCloudVoice.Gender.Male)

  test("every catalog voice is a distinct WaveNet voice"):
    val names = GoogleCloudVoices.English.map(_.name)
    assertEquals(names.distinct, names)
    assert(names.forall(_.contains("-Wavenet-")))
