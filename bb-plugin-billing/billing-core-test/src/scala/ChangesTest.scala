package billingcore

class ChangesTest extends munit.FunSuite:
  private val inputs = Inputs(Set("a", "b"), Set("p1"))

  test("a change to a thread a result read affects it; one to another thread does not"):
    assert(inputs.affectedBy(Change.Decided(BlockKey("d", 1L, 2L, Set("b", "z")))))
    assert(!inputs.affectedBy(Change.Decided(BlockKey("d", 1L, 2L, Set("y", "z")))))
    assert(inputs.affectedBy(Change.LeftOut("a")) && !inputs.affectedBy(Change.LeftOut("z")))
    assert(inputs.affectedBy(Change.Submitted(Set("z", "a"))) && !inputs.affectedBy(Change.Submitted(Set("z"))))
    assert(inputs.affectedBy(Change.Summarized(Set("b"))) && !inputs.affectedBy(Change.Summarized(Set("z"))))
    assert(inputs.affectedBy(Change.Archived("b")) && !inputs.affectedBy(Change.Archived("z")))
    assert(inputs.affectedBy(Change.NotesEdited("a")) && !inputs.affectedBy(Change.NotesEdited("z")))

  test("Zoho entries affect only results that read that Zoho project"):
    assert(inputs.affectedBy(Change.ZohoHandEntries("p1")))
    assert(!inputs.affectedBy(Change.ZohoHandEntries("p2")))

  test("where threads bill, and a restart, affect everything"):
    assert(Inputs(Set.empty, Set.empty).affectedBy(Change.BillsTo))
    assert(Inputs(Set.empty, Set.empty).affectedBy(Change.Restarted))
