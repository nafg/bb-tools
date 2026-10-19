package goalscore

class TreemapTest extends munit.FunSuite:
  private val eps                                       = 1e-6
  private def area(r: Rect)                             = r.w * r.h
  private def within(outer: Rect, inner: Rect): Boolean =
    inner.x >= outer.x - eps && inner.y >= outer.y - eps &&
      inner.x + inner.w <= outer.x + outer.w + eps && inner.y + inner.h <= outer.y + outer.h + eps
  private def overlap(a: Rect, b: Rect): Boolean =
    a.x < b.x + b.w - eps && b.x < a.x + a.w - eps && a.y < b.y + b.h - eps && b.y < a.y + a.h - eps

  test("squarify fills the rectangle with areas proportional to weights"):
    val rect   = Rect(0, 0, 100, 60)
    val placed = Treemap.squarify(List("a" -> 6.0, "b" -> 3.0, "c" -> 1.0), rect)
    assertEquals(placed.map(_._1).toSet, Set("a", "b", "c"))
    val total = placed.map(p => area(p._2)).sum
    assertEqualsDouble(total, 6000.0, 1e-6)
    val byKey = placed.toMap
    assertEqualsDouble(area(byKey("a")), 3600.0, 1e-6)
    assertEqualsDouble(area(byKey("b")), 1800.0, 1e-6)
    assertEqualsDouble(area(byKey("c")), 600.0, 1e-6)
    placed.foreach(p => assert(within(rect, p._2), p))
    for a <- placed; b <- placed if a._1 != b._1 do assert(!overlap(a._2, b._2), (a, b))

  test("squarify prefers squarish cells over long strips"):
    val placed = Treemap.squarify((1 to 8).map(i => i -> 1.0).toList, Rect(0, 0, 80, 40))
    // Eight equal weights in a 2:1 rectangle lay out as 4 x 2 squares of 20.
    placed.foreach { case (_, r) => assertEqualsDouble(r.w / r.h, 1.0, 1e-6) }

  test("squarify handles one item, zero weights, and an empty list"):
    assertEquals(Treemap.squarify(List("only" -> 5.0), Rect(1, 2, 3, 4)), List("only" -> Rect(1, 2, 3, 4)))
    assertEquals(Treemap.squarify(List.empty[(String, Double)], Rect(0, 0, 1, 1)), Nil)
    val zero = Treemap.squarify(List("a" -> 0.0, "b" -> 0.0), Rect(0, 0, 10, 10))
    assertEqualsDouble(zero.map(p => area(p._2)).sum, 100.0, 1e-6)

  test("nested layout keeps children inside their parent below its header and leaves a self cell"):
    val tree   = Node("root", self = 2.0, children = List(Node("a", 6.0, Nil), Node("b", 2.0, Nil)))
    val placed = Treemap.layoutNested(List(tree), Rect(0, 0, 100, 100), pad = 2, header = 10)
    val root   = placed.find(p => p.key == "root" && !p.isSelf).get
    val self   = placed.find(p => p.key == "root" && p.isSelf).get
    val a      = placed.find(_.key == "a").get
    val b      = placed.find(_.key == "b").get
    assertEquals(root.rect, Rect(0, 0, 100, 100))
    assertEquals(root.depth, 0)
    assertEquals(a.depth, 1)
    val inner = Rect(2, 12, 96, 86)
    List(self, a, b).foreach(p => assert(within(inner, p.rect), p))
    assertEqualsDouble(area(a.rect) / area(b.rect), 3.0, 1e-6)
    assertEqualsDouble(area(self.rect) / area(b.rect), 1.0, 1e-6)

  test("a parent too small for its padding still places its children without negative sizes"):
    val tree   = Node("root", 0.0, List(Node("a", 1.0, Nil)))
    val placed = Treemap.layoutNested(List(tree), Rect(0, 0, 3, 3), pad = 2, header = 10)
    placed.foreach(p => assert(p.rect.w >= 0 && p.rect.h >= 0, p))

  test("total weight of a node is its own plus its descendants"):
    val tree = Node("root", 1.0, List(Node("a", 2.0, List(Node("aa", 3.0, Nil)))))
    assertEqualsDouble(tree.total, 6.0, 1e-9)
