package utils.math.planar

import utils.math.*
import utils.math.space.{Triangle, V3}

object TrianglePlanar {
  def fromSeq(s: Seq[V2]): TrianglePlanar = TrianglePlanar(s(0), s(1), s(2))

  implicit def toPoly(t: TrianglePlanar): PolygonRegion = PolygonRegion(Seq(t.v1, t.v2, t.v3))

  def toBarycentric(a: V2, b: V2, c: V2, p: V2): V3 = {
    val ab = b - a
    val ac = c - a
    val pa = a - p
    val x = V3(ab.x, ac.x, pa.x)
    val y = V3(ab.y, ac.y, pa.y)
    val uv_ = x ^ y
    val uv = uv_ * (1d / uv_.z)
    V3(1d - uv.x - uv.y, uv.x, uv.y)
  }

  
  implicit val sumMullV3: ((V3, Scalar) => V3 , (V3, V3) => V3) = ((a, b) => a * b, (a, b) => a + b)
  implicit val sumMullV2: ((V2, Scalar) => V2 , (V2, V2) => V2) = ((a, b) => a * b, (a, b) => a + b)
  implicit val sumMullScalar: ((Scalar, Scalar) => Scalar , (Scalar, Scalar) => Scalar) = ((a, b) => a * b, (a, b) => a + b)
  
  @inline def interpolate[T](v1: V2, v2: V2, v3: V2, pos: V2,
                             val1: T, val2: T, val3: T)(implicit sumMull:((T, Scalar) => T, (T, T) => T)): T = {
    val mul = sumMull._1
    val sum = sumMull._2
    val bar = toBarycentric(v1, v2, v3, pos)
    sum(
      mul(val1, bar.x),
      sum(
        mul(val2, bar.y),
        mul(val3, bar.z))
    )
  }

//  @inline def interpolateV2(v1: V2, v2: V2, v3: V2, pos: V2,
//                             val1: V2, val2: V2, val3: V2): V2 = interpolate(v1 ,v2 ,v3, pos, val1, val2, val3)((a, b) => a * b, (a, b) => a + b)
//  @inline def interpolateScalar(v1: V2, v2: V2, v3: V2, pos: V2,
//                            val1: Scalar, val2: Scalar, val3: Scalar): Scalar = interpolate(v1, v2, v3, pos, val1, val2, val3)((a, b) => a * b, (a, b) => a + b)
}

case class TrianglePlanar(v1: V2, v2: V2, v3: V2) {
  @inline def interpolate[T](pos: V2, val1: T, val2: T, val3: T)(implicit sumMull: ((T, Scalar) => T, (T, T) => T)): T =
    TrianglePlanar.interpolate(v1, v2 ,v3, pos, val1, val2, val3)(sumMull)
  
  @inline def toBarycentric(pos: V2): V3 = TrianglePlanar.toBarycentric(v1, v2, v3, pos)

  @inline def toTriangle3(f: V2 => V3): Triangle = space.Triangle(f(v1), f(v2), f(v3))

  @inline def degenerate: Boolean = area ~= 0f

  @inline def nonDegenerate: Boolean = !degenerate

  @inline def side1: V2 = v2 - v1

  @inline def side2: V2 = v3 - v2

  @inline def signedArea: Scalar = (side1.x * side2.y - side1.y * side2.x) * HALF

  def classify(pos: V2): Int = {
    def orientation(px: Scalar, py: Scalar, qx: Scalar, qy: Scalar, rx: Scalar, ry: Scalar): Int = {
      val or = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)
      if (or ~= 0) 0
      else if (or > 0) 1
      else 2
    }
    val t1 = orientation(v1.x, v1.y, v2.x, v2.y, pos.x, pos.y)
    val t2 = orientation(v2.x, v2.y, v3.x, v3.y, pos.x, pos.y)
    val t3 = orientation(v3.x, v3.y, v1.x, v1.y, pos.x, pos.y)
    if (t1 == t2 && t2 == t3) PolygonRegion.INSIDE
    else if (t1 == 0 && t2 == t3 || t2 == 0 && t1 == t3 || t3 == 0 && t1 == t2) PolygonRegion.BORDER
    else if (t1 == 0 && t2 == 0 || t1 == 0 && t3 == 0 || t2 == 0 && t3 == 0) PolygonRegion.BORDER
    else PolygonRegion.OUTSIDE
  }

  def notContains(p: V2): Boolean = classify(p) == PolygonRegion.OUTSIDE
  def onBorder(p: V2): Boolean = classify(p) == PolygonRegion.BORDER
  def containsInside(p: V2): Boolean = classify(p) == PolygonRegion.INSIDE
  def contains(p: V2): Boolean = classify(p) >= 0


  @inline def area: Scalar = Math.abs(signedArea)

  @inline def orientation: Int = if (degenerate) 0 else if (signedArea > 0) 1 else -1

  @inline def cw: Boolean = signedArea <= 0

  @inline def ccw: Boolean = signedArea >= 0
}
