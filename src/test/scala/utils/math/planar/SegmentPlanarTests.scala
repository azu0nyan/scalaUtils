package utils.math.planar
import org.scalatest.funsuite.AnyFunSuite

class SegmentPlanarTests extends AnyFunSuite {
  test("xFromY"){
    val s = SegmentPlanar(V2(2, 1), V2(4, 2))

    assert(s.xFromY(0).contains(0))
    assert(s.xFromY(1).contains(2))
    assert(s.xFromY(2).contains(4))
    val s1 = SegmentPlanar(V2(1,1), V2(2,1))
    assert(s1.xFromY(1).isEmpty)
  }
}
