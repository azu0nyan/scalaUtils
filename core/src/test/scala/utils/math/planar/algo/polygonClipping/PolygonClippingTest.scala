package utils.math.planar.algo.polygonClipping

import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.{Polygon, PolygonRegion, V2}

/**
 * Mirrors `math-core`'s `tests/polygon_clipping_test.rs`, fixture-for-
 * fixture, so both the Rust rewrite and this original `PolyBool` port are
 * exercised against the same inputs.
 *
 * scalaUtils only ever had one real assertion-bearing test for this module
 * (`utils.math.PolygonClipping`'s `"hole"` test, ported below as
 * `difference cuts a hole`) - the rest is new coverage, same as the Rust
 * side. The two Rust-only `split_self_touching_chain_*` unit tests aren't
 * mirrored here: that helper is a private `splitChain` closure inside
 * `PolygonClipping.opThenCombine` in Scala, not part of the public API, so
 * there's nothing to call directly - its behavior is still exercised
 * indirectly via `corner touching squares` below.
 */
class PolygonClippingTest extends AnyFunSuite {

  def v(x: Double, y: Double): V2 = V2(x, y)

  /** CCW axis-aligned square/rectangle spanning `[x0,x1] x [y0,y1]`. */
  def rect(x0: Double, y0: Double, x1: Double, y1: Double): PolygonRegion =
    PolygonRegion(Seq(v(x0, y0), v(x1, y0), v(x1, y1), v(x0, y1)))

  def poly(regions: Seq[PolygonRegion]): Polygon = Polygon(regions)

  val EPS = 1e-6
  def approx(a: Double, b: Double): Boolean = Math.abs(a - b) < EPS

  // ---- ported: scalaUtils `utils.math.PolygonClipping`'s "hole" test ----

  test("difference cuts a hole") {
    val p1 = rect(0, 0, 100, 100)
    val h1 = rect(40, 40, 60, 60)
    assert(p1.isCcw)
    assert(h1.isCcw)

    val square = poly(Seq(p1))
    val hole = poly(Seq(h1))

    val diff1 = PolygonClipping.difference(square, poly(Seq()))
    assert(diff1.regions.forall(_.isCcw))
    assert(approx(diff1.area, 10000.0))

    val diff2 = PolygonClipping.difference(square, hole)
    val outer = diff2.regions.maxBy(_.area)
    val inner = diff2.regions.minBy(_.area)
    assert(outer.isCcw)
    assert(inner.isCw)
    assert(approx(diff2.area, 10000.0 - 400.0))
  }

  // ---- basic boolean-op correctness on two overlapping squares ----
  // p1 = [0,10]x[0,10] (area 100), p2 = [5,15]x[0,10] (area 100), overlap = [5,10]x[0,10] (area 50)

  def overlappingSquares: (Polygon, Polygon) =
    (poly(Seq(rect(0, 0, 10, 10))), poly(Seq(rect(5, 0, 15, 10))))

  test("union area overlapping squares") {
    val (p1, p2) = overlappingSquares
    val u = PolygonClipping.union(p1, p2)
    assert(approx(u.area, 150.0))
    assert(u.contains(v(2, 5)))
    assert(u.contains(v(13, 5)))
    assert(u.contains(v(7, 5)))
    assert(!u.contains(v(-1, 5)))
    assert(!u.contains(v(16, 5)))
  }

  test("intersection area overlapping squares") {
    val (p1, p2) = overlappingSquares
    val i = PolygonClipping.intersection(p1, p2)
    assert(approx(i.area, 50.0))
    assert(i.contains(v(7, 5)))
    assert(!i.contains(v(2, 5)))
    assert(!i.contains(v(13, 5)))
  }

  test("difference area overlapping squares") {
    val (p1, p2) = overlappingSquares
    val d = PolygonClipping.difference(p1, p2)
    assert(approx(d.area, 50.0))
    assert(d.contains(v(2, 5)))
    assert(!d.contains(v(7, 5)))

    val dr = PolygonClipping.reverseDifference(p1, p2)
    val d2 = PolygonClipping.difference(p2, p1)
    assert(approx(dr.area, d2.area))
    assert(approx(dr.area, 50.0))
  }

  test("xor area overlapping squares") {
    val (p1, p2) = overlappingSquares
    val x = PolygonClipping.xor(p1, p2)
    assert(approx(x.area, 100.0))
    assert(x.contains(v(2, 5)))
    assert(x.contains(v(13, 5)))
    assert(!x.contains(v(7, 5)))
  }

  test("union plus intersection equals sum of areas invariant") {
    val cases: Seq[(Polygon, Polygon)] = Seq(
      overlappingSquares,
      (poly(Seq(rect(0, 0, 10, 10))), poly(Seq(rect(2, 2, 8, 8)))),
      (poly(Seq(rect(0, 0, 10, 10))), poly(Seq(rect(100, 100, 110, 110)))),
      (
        poly(Seq(PolygonRegion(Seq(v(0, 0), v(10, 0), v(5, 10))))),
        poly(Seq(rect(2, 1, 8, 6))),
      ),
    )
    for ((p1, p2) <- cases) {
      val u = PolygonClipping.union(p1, p2)
      val i = PolygonClipping.intersection(p1, p2)
      assert(approx(u.area + i.area, p1.area + p2.area), s"union=${u.area} intersect=${i.area} p1=${p1.area} p2=${p2.area}")
    }
  }

  // ---- disjoint inputs ----

  test("disjoint squares union keeps both intersection is empty") {
    val p1 = poly(Seq(rect(0, 0, 10, 10)))
    val p2 = poly(Seq(rect(100, 100, 110, 110)))

    val u = PolygonClipping.union(p1, p2)
    assert(u.regions.size == 2)
    assert(approx(u.area, 200.0))

    val i = PolygonClipping.intersection(p1, p2)
    assert(approx(i.area, 0.0))

    val d = PolygonClipping.difference(p1, p2)
    assert(approx(d.area, 100.0))

    val x = PolygonClipping.xor(p1, p2)
    assert(approx(x.area, 200.0))
  }

  // ---- touching-but-not-overlapping: shares a whole edge ----

  test("edge adjacent squares union merges into one rectangle") {
    val p1 = poly(Seq(rect(0, 0, 10, 10)))
    val p2 = poly(Seq(rect(10, 0, 20, 10)))

    val u = PolygonClipping.union(p1, p2)
    assert(approx(u.area, 200.0))
    assert(u.regions.size == 1)
    assert(u.contains(v(5, 5)))
    assert(u.contains(v(15, 5)))
    assert(u.contains(v(10, 5)))

    val i = PolygonClipping.intersection(p1, p2)
    assert(approx(i.area, 0.0))
  }

  // ---- touching at a single vertex only ----

  test("corner touching squares union area is sum of both") {
    val p1 = poly(Seq(rect(0, 0, 10, 10)))
    val p2 = poly(Seq(rect(10, 10, 20, 20)))

    val u = PolygonClipping.union(p1, p2)
    assert(approx(u.area, 200.0))
    assert(u.contains(v(5, 5)))
    assert(u.contains(v(15, 15)))
    assert(!u.contains(v(15, 5)))
    assert(!u.contains(v(5, 15)))
  }

  // ---- identical / self ops ----

  test("union of polygon with itself is unchanged") {
    val p = poly(Seq(rect(0, 0, 10, 10)))
    val u = PolygonClipping.union(p, p)
    assert(approx(u.area, 100.0))
    assert(u.contains(v(5, 5)))
  }

  test("intersection of polygon with itself is unchanged") {
    val p = poly(Seq(rect(0, 0, 10, 10)))
    val i = PolygonClipping.intersection(p, p)
    assert(approx(i.area, 100.0))
  }

  test("difference of polygon with itself is empty") {
    val p = poly(Seq(rect(0, 0, 10, 10)))
    val d = PolygonClipping.difference(p, p)
    assert(approx(d.area, 0.0))
  }

  test("xor of polygon with itself is empty") {
    val p = poly(Seq(rect(0, 0, 10, 10)))
    val x = PolygonClipping.xor(p, p)
    assert(approx(x.area, 0.0))
  }

  // ---- identity with the empty polygon ----

  test("ops against empty polygon") {
    val p = poly(Seq(rect(0, 0, 10, 10)))
    val empty = poly(Seq())

    assert(approx(PolygonClipping.union(p, empty).area, 100.0))
    assert(approx(PolygonClipping.intersection(p, empty).area, 0.0))
    assert(approx(PolygonClipping.difference(p, empty).area, 100.0))
    assert(approx(PolygonClipping.reverseDifference(p, empty).area, 0.0))
    assert(approx(PolygonClipping.xor(p, empty).area, 100.0))
  }

  // ---- hole-producing / hole-consuming interactions ----

  test("intersection with a polygon straddling a hole boundary") {
    val outer = rect(0, 0, 20, 20)
    val hole = PolygonRegion(Seq(v(8, 8), v(8, 12), v(12, 12), v(12, 8))) // CW
    assert(hole.isCw)
    val donut = poly(Seq(outer, hole))
    assert(approx(donut.area, 400.0 - 16.0))

    val clip = poly(Seq(PolygonRegion(Seq(v(9, -5), v(11, -5), v(11, 11), v(9, 11)))))
    val i = PolygonClipping.intersection(donut, clip)
    assert(approx(i.area, 16.0), s"got ${i.area}")
  }

  test("union preserves a hole not touched by the other operand") {
    val outer = rect(0, 0, 20, 20)
    val hole = PolygonRegion(Seq(v(8, 8), v(8, 12), v(12, 12), v(12, 8)))
    val donut = poly(Seq(outer, hole))

    val other = poly(Seq(rect(100, 100, 110, 110)))
    val u = PolygonClipping.union(donut, other)
    assert(approx(u.area, (400.0 - 16.0) + 100.0))
    assert(!u.contains(v(10, 10)))
    assert(u.contains(v(1, 1)))
    assert(u.contains(v(105, 105)))
  }

  // ---- fully-nested input ----

  test("fully nested squares") {
    val outer = poly(Seq(rect(0, 0, 10, 10)))
    val inner = poly(Seq(rect(2, 2, 8, 8)))

    assert(approx(PolygonClipping.union(outer, inner).area, 100.0))
    assert(approx(PolygonClipping.intersection(outer, inner).area, 36.0))
    assert(approx(PolygonClipping.difference(outer, inner).area, 64.0))
    assert(approx(PolygonClipping.reverseDifference(outer, inner).area, 0.0))
    assert(approx(PolygonClipping.xor(outer, inner).area, 64.0))

    val d = PolygonClipping.difference(outer, inner)
    assert(d.regions.size == 2)
  }

  /**
   * Mirrors the Rust suite's regression test for a real bug found while
   * writing the Rust benchmark's `clip_blobs` case: that port's
   * `chainer.append_chain` extracted and removed the second chain from the
   * working buffer before reading the first one by its (possibly now-stale)
   * index, panicking whenever the two chains' positions were in descending
   * order. This original Scala `appendChain` always mutates `chains(index1)`
   * before `chains.remove(index2)`, so it was never susceptible - this test
   * exists here purely so both sides keep running the same fixture.
   */
  test("many intersection blobs union does not panic and conserves area") {
    def blob(centerX: Double): PolygonRegion = {
      val n = 48
      val vs = (0 until n).map { i =>
        val t = i.toDouble / n * 2 * Math.PI
        val r = 20.0 + 6.0 * Math.sin(7.0 * t)
        v(centerX + r * Math.cos(t), r * Math.sin(t))
      }
      PolygonRegion(vs)
    }

    val p1 = poly(Seq(blob(-8)))
    val p2 = poly(Seq(blob(8)))

    val u = PolygonClipping.union(p1, p2)
    val i = PolygonClipping.intersection(p1, p2)
    assert(approx(u.area + i.area, p1.area + p2.area))
  }

  // ---- concave input ----

  test("concave L shape intersected with a square") {
    val lShape = PolygonRegion(Seq(v(0, 0), v(10, 0), v(10, 5), v(5, 5), v(5, 10), v(0, 10)))
    assert(lShape.isCcw)
    assert(approx(lShape.area, 75.0))

    val l = poly(Seq(lShape))
    val square = poly(Seq(rect(4, 4, 6, 6)))

    val i = PolygonClipping.intersection(l, square)
    assert(approx(i.area, 4.0 - 1.0))

    val u = PolygonClipping.union(l, square)
    assert(approx(u.area, 75.0 + 4.0 - (4.0 - 1.0)))
  }
}
