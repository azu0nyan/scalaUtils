package utils.math.planar.algo

import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.{Polygon, PolygonRegion, V2}

class PolygonTriangulationTests extends AnyFunSuite {
  test("bug1") {
    val poly = Seq(Seq(V2(-300.0, 300.0), V2(-100.0, 300.0), V2(-100.0, 200.0),
      V2(0.0, 200.0), V2(0.0, 400.0), V2(0.0, 500.0), V2(-100.0, 500.0), V2(-200.0, 500.0), V2(-300.0, 400.0)))
    val res = PolygonTriangulation.monotonePartition(poly)

  }

  test("bug2") {
    val poly =List(V2(-170.0, 249.0), V2(-87.0, 116.0), V2(-233.0, 41.0), V2(-157.0, -67.0), V2(93.0, 14.0))
//    val poly =List(V2(-213.0, 414.0), V2(-273.0, 353.0), V2(-358.0, 361.0), V2(-393.0, 310.0), V2(-389.0, 254.0), V2(-343.0, 230.0), V2(-170.0, 249.0), V2(-87.0, 116.0), V2(-233.0, 41.0), V2(-157.0, -67.0), V2(93.0, 14.0))
//    val poly = List(V2(-213.0, 414.0), V2(-273.0, 353.0), V2(-358.0, 361.0), V2(-393.0, 310.0), V2(-389.0, 254.0), V2(-343.0, 230.0), V2(-170.0, 249.0), V2(-87.0, 116.0), V2(-233.0, 41.0), V2(-157.0, -67.0), V2(93.0, 14.0), V2(277.0, 164.0), V2(91.0, 173.0), V2(183.0, 330.0), V2(145.0, 535.0))
    PolygonTriangulation.monotonePartition(Seq(poly))
  }

}
