package conversecore

class UploadLimitTest extends munit.FunSuite:
  test("a recording fits exactly when its base64 form is within the limit"):
    val largestFitting = UploadLimit.MaxAudioBase64Chars / 4 * 3
    assert(UploadLimit.fits(largestFitting))
    assert(!UploadLimit.fits(largestFitting + 1))

  test("about twenty minutes of 32 kbps speech fits"):
    assert(UploadLimit.fits(20 * 60 * 32000 / 8))
