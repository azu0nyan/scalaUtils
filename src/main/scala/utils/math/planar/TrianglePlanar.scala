package utils.math.planar

import utils.math._
import utils.math.{space}
import utils.math.space.{Triangle, V3}

object TrianglePlanar {
  def fromSeq(s:Seq[V2]): TrianglePlanar = TrianglePlanar(s(0), s(1), s(2))

  implicit def toPoly(t: TrianglePlanar): PolygonRegion = PolygonRegion(Seq(t.v1, t.v2, t.v3))

  def toBarycentric(a:V2, b:V2, c:V2, p:V2):V3 = {
    val ab = b - a
    val ac = c - a
    val pa = a - p
    val x = V3(ab.x, ac.x, pa.x)
    val y = V3(ab.y, ac.y, pa.y)
    val uv_ = x ^ y
    val uv = uv_ * (1d / uv_.z)
    V3(1d - uv.x - uv.y, uv.x, uv.y)
  }

}

case class TrianglePlanar(v1: V2, v2: V2, v3: V2) {



  @inline def toTriangle3(f: V2 => V3): Triangle = space.Triangle(f(v1), f(v2), f(v3))

  @inline def degenerate: Boolean = area ~= 0f

  @inline def nonDegenerate: Boolean = !degenerate

  @inline def side1: V2 = v2 - v1

  @inline def side2: V2 = v3 - v2

  @inline def signedArea: Scalar = (side1.x * side2.y - side1.y * side2.x) * HALF

  @inline def area: Scalar = Math.abs(signedArea)

  @inline def orientation: Int = if (degenerate) 0 else if (signedArea > 0) 1 else -1

  @inline def cw: Boolean = signedArea <= 0

  @inline def ccw: Boolean = signedArea >= 0
}
