package utils.datastructures.spatial

import utils.math.planar.{PolygonRegion, SegmentPlanar, V2}
import utils.math._

object AARectangle {
  val ZERO_EMPTY: AARectangle = AARectangle(V2.ZERO, V2.ZERO)

  def fromSize(size: V2): AARectangle = AARectangle(-size / 2d, size / 2d)

  def fromCenterSize(center:V2, size:V2):AARectangle = AARectangle(center - size /2d, center + size / 2d)
}

case class AARectangle(min: V2, max: V2) {

  @Deprecated(since = "not implemented yet")
  @inline def distanseTo(ot: AARectangle): Scalar =
    if (intersects(ot)) 0 else 0

  @inline def area: Scalar = width * height

  @inline def this(sideX: Scalar, sideY: Scalar) = this(V2(-sideX / 2f, -sideY / 2f), V2(sideX / 2f, sideY / 2f))

  @inline def width: Scalar = max.x - min.x

  @inline def height: Scalar = max.y - min.y

  @inline def wh: V2 = V2(width, height)

  @inline def clampPoint(point: V2): V2 = utils.math.clamp(point, min, max)
  //(x1min < x2max AND x2min < x1max AND y1min < y2max AND y2min < y1max
  @inline def intersects(ot: AARectangle): Boolean = min.x < ot.max.x && ot.min.x < max.x && min.y < ot.max.y && ot.min.y < max.y

  //max.x > ot.min.x && min.x < ot.max.x && max.y > ot.min.x && min.y < ot.max.y

  @inline def contains(p: V2): Boolean = p.x >= min.x && p.x <= max.x && p.y >= min.y && p.y <= max.y

  /** minimal AARectangle containing both AARectangles */
  @inline def combine(ot: AARectangle): AARectangle = AARectangle(
    V2(math.min(min.x, ot.min.x), math.min(min.y, ot.min.y)),
    V2(math.max(max.x, ot.max.x), math.max(max.y, ot.max.y))
  )

  @inline def intersection(ot: AARectangle): AARectangle = AARectangle(
    V2(math.max(min.x, ot.min.x), math.max(min.y, ot.min.y)),
    V2(math.min(max.x, ot.max.x), math.min(max.y, ot.max.y))
  )

  /** segment between min and max */
  @inline def diagonal: SegmentPlanar = SegmentPlanar(min, max)

  @inline def secondDiagonal: SegmentPlanar = SegmentPlanar(vertices(1), vertices(3))

  @inline def vertices: Seq[V2] = Seq(
    min,
    V2(min.x, max.y),
    max,
    V2(max.x, min.y)
  )
  lazy val toPolygon:PolygonRegion = PolygonRegion(vertices)

}


