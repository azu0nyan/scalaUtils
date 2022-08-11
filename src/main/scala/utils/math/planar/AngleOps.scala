package utils.math.planar

import utils.math._

object AngleOps {


  def offsetLeft(l: V2, c: V2, r: V2, offset: Scalar): V2 = {
    val cr: V2 = r - c
    val halved = ccwAngleFromTo02PI(r, c, l) * .5f
    val res = c + (cr.normalize.rotate(halved) * offset)
    res
  }

  /** Angle between v1->v2 and v2->v3. Returns ccw-turn value at v2  if we going from v1 to v2, and later to v3.*/
//  @inline def turnAngleCCW(v1: V2, v2: V2, v3: V2): Scalar = angleFromTo(v2 - v1, v3 - v2) //todo clamp to -PI PI

  /** Angle between v1->v2 and v2->v3. Returns ccw-turn value at v2  if we going from v1 to v2, and later to v3.*/
  @inline def turnAngleCCW02PI(v1: V2, v2: V2, v3: V2): Scalar = ccwAngleFromTo02PI(v2 - v1, v3 - v2)

  //  @inline def isCCWSmallest(v1: V2, v2: V2): Boolean = v1.x * v2.y - v2.x * v1.y >= 0
  //todo optimize if needed
  @inline def isCCW(v1: V2, v2: V2, v3: V2): Boolean = TrianglePlanar(v1, v2, v3).ccw

  @inline def ccwBisectorOfAngle(v1: V2, v2: V2, v3: V2): V2 = ccwBisectorOfAngleNotNormalized(v1, v2, v3).normalize

  /**returns normalized bisector between v1 and v2*/
  @inline def ccwBisector(v1: V2, v2: V2): V2 = ccwBisectorNotNormalized(v1, v2).normalize

  @inline def ccwBisectorOfAngleNotNormalized(v1: V2, v2: V2, v3: V2): V2 = {
    val s1 = v1 - v2
    val s2 = v3 - v2
    ccwBisectorNotNormalized(s1, s2)
  }

  /**returns bisector between v1 and v2*/
  @inline def ccwBisectorNotNormalized(v1: V2, v2: V2): V2 = {
    val a = angleFromTo(v1, v2)
    //    v1.rotate(a / 2d )
    if (a < 0) {
      v1.rotate(a * HALF)
    } else {
      v1.rotate(-(TWO_PI - a) * HALF)
      //      v1.rotate((TWO_PI - a) * HALF)
    }
  }

  /** clamps angle to  0 to 2PI range */
  @inline def to0twoPi(a: Scalar): Scalar = {
    if (a < 0) a % (TWO_PI) + TWO_PI
    else if (a >= TWO_PI) a % TWO_PI
    else a
  }

  /** from (f - c) to (t - c) in range from 0 to 2PI */
  @inline def ccwAngleFromTo02PI(f: V2, c: V2, t: V2): Scalar = ccwAngleFromTo02PI(f - c, t - c)

  /** from v1 to v2 in range from 0 to 2PI */
  @inline def ccwAngleFromTo02PI(v1: V2, v2: V2): Scalar = {
    to0twoPi(v2.angleToOX - v1.angleToOX)
  }

  /** from v1 to v2 */
  @inline def angleFromTo(v1: V2, v2: V2): Scalar = {
    v2.angleToOX - v1.angleToOX
  }

}
