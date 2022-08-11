package utils.math.planar.algo
import utils.math._
import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.TrianglePlanar.toPoly
import utils.math.planar.{Polygon, PolygonRegion, TrianglePlanar, V2}


class PolygonTriangulationTests extends AnyFunSuite {
  test("bug1") {
    val poly = Seq(Seq(V2(-300.0, 300.0), V2(-100.0, 300.0), V2(-100.0, 200.0),
      V2(0.0, 200.0), V2(0.0, 400.0), V2(0.0, 500.0), V2(-100.0, 500.0), V2(-200.0, 500.0), V2(-300.0, 400.0)))
    val res = PolygonTriangulation.monotonePartitionNonHole(poly)

  }

  test("bug2") {
    val poly =List(V2(-170.0, 249.0), V2(-87.0, 116.0), V2(-233.0, 41.0), V2(-157.0, -67.0), V2(93.0, 14.0))
//    val poly =List(V2(-213.0, 414.0), V2(-273.0, 353.0), V2(-358.0, 361.0), V2(-393.0, 310.0), V2(-389.0, 254.0), V2(-343.0, 230.0), V2(-170.0, 249.0), V2(-87.0, 116.0), V2(-233.0, 41.0), V2(-157.0, -67.0), V2(93.0, 14.0))
//    val poly = List(V2(-213.0, 414.0), V2(-273.0, 353.0), V2(-358.0, 361.0), V2(-393.0, 310.0), V2(-389.0, 254.0), V2(-343.0, 230.0), V2(-170.0, 249.0), V2(-87.0, 116.0), V2(-233.0, 41.0), V2(-157.0, -67.0), V2(93.0, 14.0), V2(277.0, 164.0), V2(91.0, 173.0), V2(183.0, 330.0), V2(145.0, 535.0))
    PolygonTriangulation.monotonePartitionNonHole(Seq(poly))
  }

  test("bu31") {
    val poly = List(V2(0.0, 200.0), V2(200.0, -100.0), V2(500.0, 200.0), V2(400.0, 500.0), V2(200.0, 100.0), V2(100.0, 300.0))
    val res = PolygonTriangulation.monotonePartitionNonHole(Seq(poly))

  }

  def testTriag(s: Seq[Seq[V2]]): Unit = {
    val poly = Polygon(s.map(PolygonRegion(_)))
    val pArea = poly.area
    assert(pArea > 0) //algo works only for bounded polygons

    val res = PolygonTriangulation.triangulateNonHoles(s)
    for(t <- res) assert(t.size == 3)
    val area = res.map(TrianglePlanar.fromSeq).map(_.area).sum
    println(s"$pArea $area")
    assert(pArea ~= area)


  }



  test("bug3") {
    val poly = List(List(V2(4.0, 47), V2(367.0, 222), V2(289.0, 158), V2(231.0, 73), V2(229.0, 9), V2(246.0, -37), V2(282.0, -71)).reverse)
    testTriag(poly)
  }

  test("bug4") {
    val poly = List(List(V2(200.0, 300.0), V2(300.0, 200.0), V2(500.0, 400.0), V2(400.0, 500.0)))
    testTriag(poly)
  }

  test("bug5") {
    val poly = List(List(V2(-100.0, 100.0), V2(100.0, 100.0), V2(100.0, 300.0), V2(-100.0, 300.0), V2(0.0, 200.0)))
    testTriag(poly)
  }
  test("bug6") {
    val poly = List(List(V2(300.0, -100.0), V2(300.0, 400.0), V2(0.0, 200.0), V2(200.0, 100.0), V2(0.0, 0.0)))
    testTriag(poly)
  }
  test("sainity") {
    val t = TrianglePlanar(V2(0, 0), V2(0, 100), V2(100, 0))
    assert(t.ccw == toPoly(t).isCcw)
  }

  test("bug7") {
    val poly = List((List(V2(-200.0, 300.0), V2(-100.0, 200.0), V2(0.0, 200.0), V2(0.0, 300.0), V2(100.0, 300.0), V2(100.0, 200.0), V2(100.0, 100.0), V2(0.0, 100.0), V2(-100.0, 100.0), V2(-200.0, 100.0), V2(-300.0, 200.0))).reverse)
    testTriag(poly)
  }

  test("bug8") {
    val poly = List(List(V2(-300.0, 200.0), V2(-200.0, 100.0), V2(100.0, 100.0), V2(100.0, 200.0), V2(0.0, 200.0), V2(-100.0, 200.0), V2(-200.0, 300.0)))
    testTriag(poly)
  }
  test("bug9") {
    val poly = List(List(V2(600.0, 200.0), V2(500.0, 200.0), V2(400.0, 300.0), V2(400.0, 100.0)))
    testTriag(poly)
  }
  test("bug10") {
    val poly = List(List(V2(0.0, 200.0), V2(200.0, -100.0), V2(500.0, 200.0), V2(400.0, 500.0), V2(200.0, 100.0), V2(100.0, 300.0)))
    testTriag(poly)
  }
  test("lineSideBug") {
    val poly = List(List(V2(200.0, 100.0), V2(200.0, 0.0)), List(V2(0.0, 0.0), V2(400.0, 0.0), V2(400.0, 300.0), V2(0.0, 300.0)))
    testTriag(poly)
  }
//  test("bug7") {
//    val poly = List((List(V2(-200.0, 300.0), V2(-100.0, 200.0), V2(0.0, 200.0), V2(0.0, 300.0), V2(100.0, 300.0), V2(100.0, 200.0), V2(100.0, 100.0), V2(0.0, 100.0), V2(-100.0, 100.0), V2(-200.0, 100.0), V2(-300.0, 200.0))))
//    testTriag(poly)
//  }

}
