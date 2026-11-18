package billingcore

class DecisionsTest extends munit.FunSuite:
  private val Day                    = "2026-10-19"
  private def at(minutes: Int): Long = 1_000_000_000L + minutes * 60_000L
  private val ab                     = Set("a", "b")
  private def key(from: Int, to: Int) = BlockKey(Day, at(from), at(to), ab)
  private def block(from: Int, to: Int) = OverlapBlock(Day, at(from), at(to), ab)

  test("a block's own decision is its decision"):
    assertEquals(
      Decisions.resolve(Seq(block(0, 10)), Map(key(0, 10) -> Allocation.WholeTo("a"))),
      Map(key(0, 10) -> Allocation.WholeTo("a"))
    )

  test("pieces that cover a block exactly decide it, joined"):
    val whole = Map(key(0, 4) -> Allocation.WholeTo("b"), key(4, 10) -> Allocation.WholeTo("b"))
    assertEquals(Decisions.resolve(Seq(block(0, 10)), whole), Map(key(0, 10) -> Allocation.WholeTo("b")))
    val mixed = Map(key(0, 4) -> Allocation.WholeTo("a"), key(4, 10) -> Allocation.Nobody)
    assertEquals(
      Decisions.resolve(Seq(block(0, 10)), mixed),
      Map(key(0, 10) -> Allocation.Portions(Map("a" -> 4 * 60_000L)))
    )

  test("pieces that leave a gap, or are of other threads, decide nothing"):
    assertEquals(Decisions.resolve(Seq(block(0, 10)), Map(key(0, 4) -> Allocation.WholeTo("a"))), Map.empty)
    val other = Map(BlockKey(Day, at(0), at(10), Set("a", "c")) -> Allocation.WholeTo("a"))
    assertEquals(Decisions.resolve(Seq(block(0, 10)), other), Map.empty)

  test("the pieces of a key are the stored keys inside it with its threads"):
    val stored = Seq(key(0, 4), key(4, 10), key(10, 12), BlockKey(Day, at(0), at(4), Set("a")), key(0, 10))
    assertEquals(Decisions.piecesOf(key(0, 10), stored).toSet, Set(key(0, 4), key(4, 10)))
