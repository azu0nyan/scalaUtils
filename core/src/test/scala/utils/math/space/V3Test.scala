package utils.math.space

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.math._

class V3Test extends AnyFlatSpec with Matchers {
  "V3.rotate" should "work" in {
  }

  it should "work1" in testRotate(V3.x, V3.z, HALF_PI, V3.y)

  it should "work2" in testRotate(V3.x, V3.z, PI, -V3.x)

  it should "work3" in testRotate(V3.x, V3.z, THREE_QUARTER_PI, -V3.y)

  it should "work for non 90 degrees " in
    testRotate(V3(1.0, 0.0, 0.0), V3(0.0, 1.0, 0.0), `PI/4`, (`sqrt(2)` / 2, 0.0, -`sqrt(2)` / 2))

  it should "work for non unit vectors" in
    testRotate(V3(5, 4, 3), V3(3, 4, 5), `PI/6`, V3(4.13421148, 5.088498979, 2.648673929))

  def testRotate(v: V3, axis: V3, angle: Scalar, expected: V3): Unit = {
    val rotated = v.rotate(axis, angle)
    val back = rotated.rotate(axis, -angle)
    println(s"$rotated == $expected | $back == $v")
    assert(back ~= v)
    assert(rotated ~= expected)
  }
}
