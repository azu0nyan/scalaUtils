package utils.math.planar.algo

import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.{Polygon, PolygonRegion, V2}

class PolygonTriangulationTests extends AnyFunSuite{
 test("bug1") {
   val poly = Seq(Seq(V2(-300.0, 300.0), V2(-100.0, 300.0), V2(-100.0, 200.0), V2(0.0, 200.0), V2(0.0, 400.0), V2(0.0, 500.0), V2(-100.0, 500.0), V2(-200.0, 500.0), V2(-300.0, 400.0)))
   val res = PolygonTriangulation.monotonePartition(poly)

 }

}
