package utils.math

import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.*

class PolygonOpsTest extends AnyFunSuite {


  def testP(s: Seq[(Double, Double)], po: (Double, Double), res: Int) = {
    val p: PolygonRegion = PolygonRegion(s.map { case (x, y) => V2(x, y) })
    assert(p.classify(V2(po._1, po._2)) == res)
  }

  test("bug") {
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (0d, 1d), PolygonRegion.BORDER)
  }
  test("bug2") {
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (-1d, 1d)), (-2d, -1d), PolygonRegion.OUTSIDE)
  }
  test("bug3") {
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (3d, 1.5d), PolygonRegion.OUTSIDE)
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

  test("harder tests") {
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (0d, 0d), (-1d, 1d)), (0d, 0d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (0d, 0d), (-1d, 1d)), (1d, 0d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (0d, 0d), (-1d, 1d)), (-1d, 0d), PolygonRegion.BORDER)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (0d, 0d), (-1d, 1d)), (-.5d, 0d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (0d, 0d), (-1d, 1d)), (.5d, 0d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (0d, 0d), (-1d, 1d)), (0d, -.5d), PolygonRegion.INSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (0d, 0d), (-1d, 1d)), (1.5d, 0d), PolygonRegion.OUTSIDE)
    testP(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (0d, 0d), (-1d, 1d)), (-1.5d, 0d), PolygonRegion.OUTSIDE)
  }

  test(" harder 2 ") {
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (0, 0), PolygonRegion.OUTSIDE)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (-1, 1), PolygonRegion.OUTSIDE)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (-1, 2), PolygonRegion.OUTSIDE)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (-1, 3), PolygonRegion.OUTSIDE)

    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (1, 0), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (0, 1), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (0, 2), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (0, 3), PolygonRegion.BORDER)

    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (2, 0), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (3, 1), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (3, 2), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (3, 3), PolygonRegion.BORDER)

    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (1.5, .5), PolygonRegion.INSIDE)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (1.5, 1d), PolygonRegion.INSIDE)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (0.5, 2), PolygonRegion.INSIDE)
    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (2.5, 2), PolygonRegion.INSIDE)

    testP(Seq((1d, 0d), (2d, 0d), (2d, 1d), (3d, 1d), (3d, 3d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 3d), (0d, 1d), (1d, 1d)),
      (0.5, 3.0), PolygonRegion.BORDER)


  }


  test(" harder 3 ") {
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (0, 0), PolygonRegion.OUTSIDE)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (0d, 1d), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (0d, 2d), PolygonRegion.OUTSIDE)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (0d, 1.5d), PolygonRegion.OUTSIDE)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (1d, 1.5d), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (1.5d, 1.5d), PolygonRegion.INSIDE)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (2d, 1.5d), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (3d, 1.5d), PolygonRegion.OUTSIDE)

    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (0d, 3d), PolygonRegion.OUTSIDE)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (1d, 3d), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (1.5d, 3d), PolygonRegion.OUTSIDE)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (2d, 3d), PolygonRegion.BORDER)
    testP(Seq((1d, 0d), (2d, 1.5d), (3d, 1d), (2d, 3d), (2d, 2d), (1d, 2d), (1d, 3d), (0d, 1d), (1d, 1.5d)),
      (3d, 3d), PolygonRegion.OUTSIDE)


  }

  test("cw-ccw") {
    assert(PolygonRegion(List(V2(330.0, -310.0), V2(330.0, -320.0), V2(320.0, -320.0))).isCw)
    assert(PolygonRegion(List(V2(330.0, -310.0), V2(320.0, -320.0), V2(330.0, -320.0))).isCcw)
  }

  test("Polygon contains bug") {
    assert(Polygon(List(PolygonRegion(List(V2(330.0, -310.0), V2(330.0, -320.0), V2(320.0, -320.0))))).contains(V2(322.0, -315.0)))
  }

  test("isConvex bug") {
    val p = PolygonRegion(List(V2(200.0, 200.0), V2(400.0, 0.0), V2(500.0, 100.0), V2(400.0, 200.0), V2(500.0, 300.0), V2(400.0, 400.0)))
    println(p.sideAngles)
//    println(p.sideAngles.map({ case AngleCCWPlanar(l, c, r) => AngleOps.turnAngleCCW(l, c, r) }))
    println(p.sideAngles.map({ case AngleCCWPlanar(l, c, r) => AngleOps.turnAngleCCW02PI(l, c, r) }))
    assert(!p.isConvex)
  }

}
