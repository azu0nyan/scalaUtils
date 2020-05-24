package old

import org.scalatest.FunSuite
import org.scalatest.funsuite.AnyFunSuite
import utils.math.planar.V2
import utils.math.planar.algo.ConvexHull

class ConvexHullTest extends AnyFunSuite {
  test("hullTest"){
    println(ConvexHull.convexHullCCWImpl(Set(
      V2(0, 0),
      V2(1, 0),
      V2(0, 3),
      V2(-1, 2),
      V2(2, 2),
      V2(2, 0),
      V2(1, 2),
      V2(0, 1),
      V2(1, 2),
      V2(0, 2)
    )))
  }

}
