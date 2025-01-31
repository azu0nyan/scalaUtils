package utils.datastructures.spatial

import utils.datastructures.{IntV2, spatial}

object AAIntRectangle{
  // def fromAARectangle(r:Rectangle): AARectangle = AARectangle(r.min, r.max)

  implicit def toAARectangle(r:AAIntRectangle): AARectangle = r.toAARectangle
}

case class AAIntRectangle(min:IntV2, max:IntV2) {
  //BUILDS rectangle on side
  def xpRectangle(w:Int = 1) = spatial.AAIntRectangle((max.i, min.j), (max.i + w, max.j))

  def xmRectangle(w:Int = 1) = spatial.AAIntRectangle((min.i - w, min.j), (min.i, max.j))

  def ypRectangle(h:Int = 1) = spatial.AAIntRectangle((min.i, max.j), (max.i, max.j + h))

  def ymRectangle(h:Int = 1) = spatial.AAIntRectangle((min.i, min.j - h), (max.i, min.j))

  def toAARectangle:AARectangle = new AARectangle(min.toV2, max.toV2)

  def width: Int = max.i - min.i

  def height: Int = max.j - min.j

  def area:Int = width * height

  def wh:IntV2 = (width, height)

  //def clampPoint(point:IntV2): IntV2 = MathUtils.clamp(point, min, max)

  def intersects(ot: AAIntRectangle): Boolean = max.i > ot.min.i && min.i < ot.max.i && max.j > ot.min.j && min.j < ot.max.j
  def intersectsOrTouches(ot: AAIntRectangle): Boolean = max.i >= ot.min.i && min.i <= ot.max.i &&
    max.j >= ot.min.j && min.j <= ot.max.j

  def combine(ot: AAIntRectangle): AAIntRectangle = AAIntRectangle(
    IntV2(math.min(min.i, ot.min.i), math.min(min.j, ot.min.j)),
    IntV2(math.max(max.i, ot.max.i), math.max(max.j, ot.max.j))
  )

  def intersection(ot: AAIntRectangle): AAIntRectangle = AAIntRectangle(
    IntV2(math.max(min.i, ot.min.i), math.max(min.j, ot.min.j)),
    IntV2(math.min(max.i, ot.max.i), math.min(max.j, ot.max.j))
  )

}
