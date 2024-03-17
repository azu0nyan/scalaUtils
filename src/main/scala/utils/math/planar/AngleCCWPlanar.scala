package utils.math.planar

import utils.math._
//todo use AngleOps for implementation
object AngleCCWPlanar {
  implicit def toScalar(angle: AngleCCWPlanar): Scalar = angle.rads
}

case class AngleCCWPlanar(l: V2, c: V2, r: V2) {

  @inline def CW: Scalar = -AngleCCWPlanar.toScalar(this)

  @inline def cl: V2 = l - c
  @inline def cr: V2 = r - c

  @inline def rads: Scalar = {
    val a = cl.angle(cr)
    if(a < 0){
      (TWO_PI  + a)
    } else {
      a
    }
  }

  @inline def halved: Scalar = rads / 2f

  @inline def doubled: Scalar = rads * 2f

  @inline def offsetIn(offset: Scalar): V2 = c + (cl.normalize.rotate(halved) * offset)

  @inline def offsetInProjectionLength(offset: Scalar): Scalar = offset * sin(Math.PI / 2f - halved)

  @inline def offsetFromSideDistanceFromCenter(offsetFromSide: Scalar): Scalar = if(sin(halved) == 0f) 0f else offsetFromSide / sin(halved)

  @inline def offsetFromSideProjectionLength(offsetFromSide: Scalar): Scalar = offsetFromSideDistanceFromCenter(offsetFromSide) * sin(HALF_PI / 2f - halved)

  @inline def offsetFromSide(offset: Scalar): V2 = c + (cl.normalize.rotate(halved) * offsetFromSideDistanceFromCenter(offset))

  @inline def degrees: Scalar = Math.toDegrees(rads)

  @inline def bisector = LinePlanar(c, AngleOps.ccwBisectorOfAngle(l, c, r))

}

class AngleCW2(l: V2, c:V2, r:V2) extends AngleCCWPlanar(r, c, l)
