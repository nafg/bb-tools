package conversecore

class OutboxTest extends munit.FunSuite:
  private def uploadingOne: Outbox[String] =
    Outbox.empty[String].enqueue("a", "first").startNext.get._2

  test("recordings upload one at a time, in the order queued"):
    val queued          = Outbox.empty[String].enqueue("a", "first").enqueue("b", "second")
    val Some((first, o1)) = queued.startNext: @unchecked
    assertEquals(first.id, "a")
    assertEquals(o1.startNext, None)
    val Some((second, _)) = o1.stored("a").startNext: @unchecked
    assertEquals(second.id, "b")

  test("a recording is busy until its outcome arrives, after its storing was confirmed"):
    val stored = uploadingOne.stored("a")
    assert(!stored.holdsUnstored)
    assert(stored.busy)
    assert(!stored.settled("a").busy)

  // The case that kept a session in its transcribing phase: the backend
  // transcribed and delivered the recording, and announced it, before its
  // reply confirming the upload reached the page.
  test("an outcome arriving before the storing is confirmed is not lost"):
    val settledFirst = uploadingOne.settled("a")
    assert(settledFirst.busy, "still uploading until the confirmation arrives")
    assert(!settledFirst.stored("a").busy)

  test("replies wait only while a recording is not yet stored"):
    assert(Outbox.empty[String].enqueue("a", "first").holdsUnstored)
    assert(uploadingOne.holdsUnstored)
    assert(!uploadingOne.stored("a").holdsUnstored)

  test("a confirmation for a recording not uploading changes nothing"):
    assertEquals(uploadingOne.stored("other"), uploadingOne)
