package utils.math.planar

import utils.math._
import utils.math.space.Quad

class QuadPlanar(val bl: V2, val br: V2, val tl: V2, val tr: V2)
  extends PolygonRegion(
    Seq(bl, tl, tr, br)
  ) {

  def toQuad3(height: Scalar): Quad = new Quad(bl.planarToV3(height), br.planarToV3(height), tl.planarToV3(height), tr.planarToV3(height))

}

@Deprecated(since = "Use AARectangle instead")
class Rectangle(val min: V2, val max: V2)
  extends QuadPlanar(
    min,
    V2(max.x, min.y),
    V2(min.x, max.y),
    max
  ) {
  def this(sideX: Scalar, sideY: Scalar) = this(V2(-sideX / 2f, -sideY / 2f), V2(sideX / 2f, sideY / 2f))

  def width: Scalar = max.x - min.x

  def height: Scalar = max.y - min.y

  def wh:V2 = (width, height)

  def clampPoint(point:V2): V2 = clamp(point, min, max)
}

class Square(val side: Scalar) extends Rectangle(side, side)
