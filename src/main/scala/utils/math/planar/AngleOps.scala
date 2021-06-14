package utils.math.planar

import utils.math._

object AngleOps {



  def offsetLeft(l: V2, c: V2, r: V2, offset:Scalar): V2 = {
    val cr: V2 = r - c
    val halved = ccwAngleFromTo(r, c, l) *.5f
    val res = c + (cr.normalize.rotate(halved) * offset)
    println(l, c, r, res, halved)
    res
  }

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

  @inline def to0twoPi(a:Scalar) :Scalar = {
    if(a < 0) a % (TWO_PI) + TWO_PI
    else if(a >= TWO_PI) a % TWO_PI
    else a
  }


  @inline def ccwAngleFromTo(f:V2, c:V2, t:V2):Scalar = ccwAngleFromTo(f - c, t - c)

  /** from v1 to v2 */
  @inline def ccwAngleFromTo(v1: V2, v2: V2): Scalar = {
    to0twoPi(v2.angleToOX - v1.angleToOX)
  }

  /** from v1 to v2 */
  @inline def angleFromTo(v1: V2, v2: V2): Scalar = {
    v2.angleToOX - v1.angleToOX
  }

}
