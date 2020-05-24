package utils.math.planar

import utils.math._

object AngleCCWPlanar {
  implicit def toScalar(angle: AngleCCWPlanar): Scalar = angle.rads
}

case class AngleCCWPlanar(l: V2, c: V2, r: V2) {

  def CW: Scalar = -AngleCCWPlanar.toScalar(this)

  def cl: V2 = l - c
  def cr: V2 = r - c

  def rads: Scalar = {
    val a = cl.angle(cr)
    if(a < 0){
      (TWO_PI  + a)
    } else {
      a
    }
  }

  def halved: Scalar = rads / 2f

  def doubled: Scalar = rads * 2f

  def offsetIn(offset: Scalar): V2 = c + (cl.normalize.rotate(halved) * offset)

  def offsetInProjectionLength(offset: Scalar): Scalar = offset * sin(Math.PI / 2f - halved)

  def offsetFromSideDistanceFromCenter(offsetFromSide: Scalar): Scalar = if(sin(halved) == 0f) 0f else offsetFromSide / sin(halved)

  def offsetFromSideProjectionLength(offsetFromSide: Scalar): Scalar = offsetFromSideDistanceFromCenter(offsetFromSide) * sin(HALF_PI / 2f - halved)

  def offsetFromSide(offset: Scalar): V2 = c + (cl.normalize.rotate(halved) * offsetFromSideDistanceFromCenter(offset))

  def degrees: Scalar = Math.toDegrees(rads)

}

class AngleCW2(l: V2, c:V2, r:V2) extends AngleCCWPlanar(r, c, l)
