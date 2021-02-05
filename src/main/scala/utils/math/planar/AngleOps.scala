package utils.math.planar

import utils.math._

object AngleOps {
//  @inline def isCCWSmallest(v1: V2, v2: V2): Boolean = v1.x * v2.y - v2.x * v1.y >= 0
  //todo optimize if needed
  @inline def isCCW(v1: V2, v2: V2, v3: V2): Boolean = TrianglePlanar(v1, v2, v3).ccw

  @inline def ccwBisectorPath(v1: V2, v2: V2, v3: V2): V2 = ccwBisectorPathNotNormalized(v1, v2, v3).normalize

  @inline def ccwBisector(v1: V2, v2: V2): V2 = ccwBisectorNotNormalized(v1, v2).normalize

  @inline def ccwBisectorPathNotNormalized(v1: V2, v2: V2, v3: V2): V2 = {
    val s1 = v1 - v2
    val s2 = v3 - v2
    ccwBisectorNotNormalized(s1, s2)
  }


  @inline def ccwBisectorNotNormalized(v1: V2, v2: V2): V2 = {
    val a = angleFromTo(v1, v2)
//    v1.rotate(a / 2d )
    if (a < 0) {
      v1.rotate(a * HALF)
    } else {
      v1.rotate(-(TWO_PI-a) * HALF)
//      v1.rotate((TWO_PI - a) * HALF)
    }
  }

  /** from v1 to v2 */
  @inline def angleFromTo(v1: V2, v2: V2): Scalar = {
    v2.angleToOX - v1.angleToOX
  }

}
