package utils.math
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.math.planar.{SegmentPlanar, V2}

class SegmentPlanar extends AnyFunSuite {
  test("Same direction bug???"){
    val body1 = SegmentPlanar(V2(10.0, 30.0), V2(60.0, 30.0)).body
    val body2 = SegmentPlanar(V2(0.0, 0.0), V2(100.0, 0.0)).body
    println(s"$body1 ${body1.normalize}")
    println(s"$body2 ${body2.normalize}")
    assert(body1.sameDirection(body2))
  }
}
