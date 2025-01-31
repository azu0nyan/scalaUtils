package utils.math

import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.algo.polygonClipping.PolygonClipping
import utils.math.planar.{Polygon, PolygonRegion, V2}

class PolygonClipping extends AnyFunSuite{
  test("hole") {
    val p1 = PolygonRegion(Seq(V2(0, 0), V2(100, 0), V2(100, 100), V2(0, 100)))
    val h1 = PolygonRegion(Seq(V2(40, 40), V2(60, 40), V2(60, 60), V2(40, 60)))
    assert(p1.isCcw)
    assert(h1.isCcw)
    val poly = Polygon(Seq(p1))
    val hole = Polygon(Seq(h1))

    val diff1 = PolygonClipping.difference(poly, Polygon(Seq()))
    println(diff1)
    assert(diff1.regions.forall(_.isCcw))

    val diff2 = PolygonClipping.difference(poly, hole)

    assert(diff2.regions.maxBy(_.area).isCcw)
    assert(diff2.regions.minBy(_.area).isCw)

  }
}
