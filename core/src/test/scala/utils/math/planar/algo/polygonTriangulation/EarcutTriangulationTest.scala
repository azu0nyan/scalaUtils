package utils.math.planar.algo.polygonTriangulation

import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.{PolygonRegion, TrianglePlanar, V2}

/**
 * Mirrors `math-core`'s `tests/earcut_test.rs`, vertex-for-vertex, so both
 * the Rust rewrite and this original Java/Scala `Earcut` are exercised
 * against the same fixtures.
 *
 * As in that file, the primary invariant checked isn't an exact
 * triangle-by-triangle match against some fixed reference output, but that
 * the unsigned sum of output triangle areas equals the input polygon's
 * area (outer minus holes) - true for any valid triangulation regardless
 * of which diagonals earcut happens to pick.
 */
class EarcutTriangulationTest extends AnyFunSuite {

  def region(points: Seq[(Double, Double)]): Seq[V2] = points.map { case (x, y) => V2(x, y) }

  def assertValidTriangulation(polygon: Seq[(Double, Double)], holes: Seq[Seq[(Double, Double)]], expectedTriangleCount: Int): Unit = {
    val poly = region(polygon)
    val holeRegions = holes.map(region)

    val result = Triangulation.triangulate(poly, holeRegions)
    val vertices = result.vertices
    val indices = result.triangulationIndices

    assert(indices.size == expectedTriangleCount, s"unexpected triangle count: ${indices.size}")

    val expectedArea = PolygonRegion(poly).area - holeRegions.map(h => PolygonRegion(h).area).sum

    var areaSum = 0.0
    for ((a, b, c) <- indices) {
      assert(a >= 0 && a < vertices.size && b >= 0 && b < vertices.size && c >= 0 && c < vertices.size, "index out of bounds")
      val tri = TrianglePlanar(vertices(a), vertices(b), vertices(c))
      assert(tri.area > 1e-9, s"degenerate triangle in output: $tri")
      areaSum += tri.area
    }

    assert(math.abs(areaSum - expectedArea) < 1e-6, s"triangle area sum $areaSum != polygon area $expectedArea")
  }

  test("convex square, two triangles") {
    val square = Seq((0.0, 0.0), (10.0, 0.0), (10.0, 10.0), (0.0, 10.0))
    assertValidTriangulation(square, Seq(), 2)
  }

  test("concave seven-vertex polygon") {
    val poly = Seq(
      (-300.0, 200.0), (-200.0, 100.0), (100.0, 100.0), (100.0, 200.0),
      (0.0, 200.0), (-100.0, 200.0), (-200.0, 300.0),
    )
    assertValidTriangulation(poly, Seq(), poly.size - 2)
  }

  test("comb eleven-vertex polygon") {
    val poly = Seq(
      (-200.0, 300.0), (-100.0, 200.0), (0.0, 200.0), (0.0, 300.0), (100.0, 300.0),
      (100.0, 200.0), (100.0, 100.0), (0.0, 100.0), (-100.0, 100.0), (-200.0, 100.0), (-300.0, 200.0),
    )
    assertValidTriangulation(poly, Seq(), poly.size - 2)
  }

  test("square with one square hole") {
    val outer = Seq((-20.0, -20.0), (20.0, -20.0), (20.0, 20.0), (-20.0, 20.0))
    val hole = Seq((-10.0, -10.0), (10.0, -10.0), (10.0, 10.0), (-10.0, 10.0))
    // V = 4 (outer) + 4 (hole) = 8, h = 1 => V + 2h - 2 = 8
    assertValidTriangulation(outer, Seq(hole), 8)
  }

  test("square with two square holes") {
    // Asymmetric hole placement so each hole's bridge doesn't land exactly
    // collinear with a neighboring hole edge - see the matching Rust test
    // for why that coincidence would (harmlessly) change the vertex/triangle
    // count away from the plain `V + 2h - 2` Euler count below.
    val outer = Seq((-40.0, -10.0), (40.0, -10.0), (40.0, 10.0), (-40.0, 10.0))
    val holeA = Seq((-32.0, -6.0), (-11.0, -4.0), (-13.0, 6.0), (-29.0, 5.0))
    val holeB = Seq((9.0, -5.0), (31.0, -7.0), (28.0, 4.0), (12.0, 7.0))
    // V = 4 + 4 + 4 = 12, h = 2 => V + 2h - 2 = 14
    assertValidTriangulation(outer, Seq(holeA, holeB), 14)
  }

  test("off-center hole near outer boundary") {
    val outer = Seq((-50.0, -50.0), (50.0, -50.0), (50.0, 50.0), (-50.0, 50.0))
    val hole = Seq((-45.0, -5.0), (-15.0, -5.0), (-15.0, 5.0), (-45.0, 5.0))
    assertValidTriangulation(outer, Seq(hole), 8)
  }

  test("large polygon exercises z-order hashing") {
    // Earcut only builds the z-order curve index once the vertex count
    // exceeds 80 - this is the smallest regular polygon that crosses that
    // threshold, specifically to exercise the hashed ear search instead of
    // the plain linear-scan one every other test above takes.
    val n = 96
    val poly = (0 until n).map { i =>
      val t = 2.0 * math.Pi * i / n
      (100.0 * math.cos(t), 100.0 * math.sin(t))
    }
    assertValidTriangulation(poly, Seq(), n - 2)
  }

  test("triangle degenerates to single triangle") {
    val tri = Seq((0.0, 0.0), (10.0, 0.0), (5.0, 10.0))
    assertValidTriangulation(tri, Seq(), 1)
  }

  test("fewer than three vertices produces no triangles") {
    assert(Triangulation.triangulate(Seq(), Seq()).triangulationIndices.isEmpty)
    assert(Triangulation.triangulate(Seq(V2(0.0, 0.0)), Seq()).triangulationIndices.isEmpty)
    assert(Triangulation.triangulate(Seq(V2(0.0, 0.0), V2(1.0, 1.0)), Seq()).triangulationIndices.isEmpty)
  }
}
