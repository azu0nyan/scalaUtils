package utils.math.planar

import utils.math._
import utils.math.{space}
import utils.math.space.{Triangle, V3}

object TrianglePlanar {
  implicit def toPoly(t: TrianglePlanar): PolygonRegion = PolygonRegion(Seq(t.v1, t.v2, t.v3))
}

case class TrianglePlanar(v1: V2, v2: V2, v3: V2) {

  def toTriangle3(f: V2 => V3): Triangle = space.Triangle(f(v1), f(v2), f(v3))

  def degenerate: Boolean = area ~= 0f

  def nonDegenerate: Boolean = !degenerate

  def side1: V2 = v2 - v1

  def side2: V2 = v3 - v2

  def signedArea: Scalar = (side1.x * side2.y - side1.y * side2.x) * HALF

  def area: Scalar = Math.abs(signedArea)

  def orientation: Int = if (degenerate) 0 else if (signedArea > 0) 1 else -1

  def cw: Boolean = signedArea < 0

  def ccw: Boolean = signedArea > 0
}
