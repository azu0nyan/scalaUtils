package utils.math
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.math.planar.{SegmentPlanar, V2}

class V2 extends AnyFunSuite {
  test("Same direction bug???"){
    val body1 = SegmentPlanar(V2(10.0, 30.0), V2(60.0, 30.0)).body
    val body2 = SegmentPlanar(V2(0.0, 0.0), V2(100.0, 0.0)).body
    println(s"$body1 ${body1.normalize}")
    println(s"$body2 ${body2.normalize}")
    assert(body1.sameDirection(body2))
  }

  test("AngleCCWto"){
    import utils.math._
    assert(V2(1, 0).angleCCW0to2PI(V2(1, 0)) == 0)
    assert(V2(1, 1).angleCCW0to2PI(V2(1, 1)) == 0)
    assert(V2(-1, -1).angleCCW0to2PI(V2(-1, -1)) == 0)
    assert(V2(1, 1).angleCCW0to2PI(V2(-1, -1)) ~= PI)
    assert(V2(-1, -1).angleCCW0to2PI(V2(1, 1)) ~= PI)

    assert(V2(1, 0).angleCCW0to2PI(V2(0, 1)) ~= HALF_PI)
    assert(V2(0, 1).angleCCW0to2PI(V2(1, 0)) ~= ONE_AND_HALF_PI)

    for(i <- 0 until 32) {
      val a = i * TWO_PI / 32
      assert(V2(1, 0).rotate(a).angleCCW0to2PI(V2(0, 1).rotate(a)) ~= HALF_PI)
      assert(V2(0, 1).rotate(a).angleCCW0to2PI(V2(1, 0).rotate(a)) ~= ONE_AND_HALF_PI)
      assert(V2(1, 0).rotate(a).angleCCW0to2PI(V2(1, 1).rotate(a)) ~= QUARTER_PI)
      assert(V2(1, 1).rotate(a).angleCCW0to2PI(V2(1, 0).rotate(a)) ~= PI * 1.75)
    }
  }
}
