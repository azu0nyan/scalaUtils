package utils.math

import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.{PolygonRegion, V2}

class PolygonOpsTest extends AnyFunSuite {


  def testP(s: Seq[(Double, Double)], po: (Double, Double), res: Int) = {
    val p: PolygonRegion = PolygonRegion(s.map { case (x, y) => V2(x, y) })
    assert(p.classify(V2(po._1, po._2)) == res)
  }
  test("bug"){
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (0d, 1d), PolygonRegion.BORDER)
  }
  test("bug2"){
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-2d, -1d), PolygonRegion.OUTSIDE)
  }

  test("inside tests") {
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (0d, 0d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (.9d, 0d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-.9d, 0d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (0d, .9d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (0d, -.9d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (.9d, .9d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-.9d, .9d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (.9d, -.9d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-.9d, -.9d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (0d, 1d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (0d, -1d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (1d, 0d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-1d, 0d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (1d, 1d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-1d, 1d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (1d, -1d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-1d, -1d), PolygonRegion.BORDER)

    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (2d, 0d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-2d, 0d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-0d, 2d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-0d, -2d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-2d, -1d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (2d, -1d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-2d, 1d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (2d, 1d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (2d, 2d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (2d, -2d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-2d, 2d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-2d, -2d), PolygonRegion.OUTSIDE)

  }


}
