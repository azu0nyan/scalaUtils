package utils.math

import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.{PolygonRegion, V2}

class PolygonOpsTest  extends AnyFunSuite{

  test("bug1"){
    val p = PolygonRegion(Seq((-1d, -1d), (1d, -1d), (1d, 1d), (1d, -1d)))
    println(p.sides)
    assert(p.classify(V2(0, 0)) == p.INSIDE)
  }

}
