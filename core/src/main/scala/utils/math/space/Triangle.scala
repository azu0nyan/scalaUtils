package utils.math.space

import utils.datastructures.spatial.TriangleSoup
import utils.math.*
import utils.math.space.Shape.PointShape
import utils.math.space.intersections.{Intersects, TriangleToTriangle}

object Triangle {
  def averageNormal(tris: Seq[Triangle]): V3 = tris.map(t => t.normal).reduce((x, y) => x + y).normalize
}

case class Triangle(a: V3, b: V3, c: V3) extends NormalAndClosest with Shape {

  @inline def apply(i: Int): V3 = i match {
    case 0 => v1
    case 1 => v2
    case 2 => v3
    case _ => throw new IndexOutOfBoundsException(s"$i out of triangle vertices")
  }

  @inline def v1: V3 = a

  @inline def v2: V3 = b

  @inline def v3: V3 = c

  @inline def degenerate: Boolean = area ~= 0f

  @inline def nonDegenerate: Boolean = !degenerate

  @inline def ab: V3 = b - a

  @inline def ac: V3 = c - a

  @inline def bc: V3 = c - b

  @inline def normal: Normal = (ab ^ ac).normalize

  @inline def area: Scalar = (ab ^ ac).length / 2f

  @inline def onPositiveSide(otherNormal: V3): Boolean = normal ** otherNormal > 0

  @inline def flip = Triangle(a, c, b)

  @inline def flipToMatchNormal(otherNormal: V3): Triangle = if (onPositiveSide(otherNormal)) this else flip

  @inline def toPlane: Plane = new Plane(a, ab, bc)

  @inline def center: V3 = (a + b + c) * THIRD

  override def closestPoint(p: V3): V3 = {
    // Check if P in vertex region outside A
    val ap = p - a
    val d1: Scalar = ab ** ap
    val d2: Scalar = ac ** ap
    if (d1 <= 0.0f && d2 <= 0.0f) return a; // barycentric coordinates (1,0,0)
    // Check if P in vertex region outside B
    val bp = p - b
    val d3: Scalar = ab ** bp
    val d4: Scalar = ac ** bp
    if (d3 >= 0.0f && d4 <= d3) return b; // barycentric coordinates (0,1,0)
    // Check if P in edge region of AB, if so return projection of P onto AB
    val vc = d1 * d4 - d3 * d2
    if (vc <= 0.0f && d1 >= 0.0f && d3 <= 0.0f) {
      val v = d1 / (d1 - d3)
      return a + v * ab; // barycentric coordinates (1-v,v,0)
    }
    // Check if P in vertex region outside C
    val cp = p - c
    val d5 = ab ** cp
    val d6 = ac ** cp
    if (d6 >= 0.0f && d5 <= d6) return c; // barycentric coordinates (0,0,1)
    // Check if P in edge region of AC, if so return projection of P onto AC
    val vb = d5 * d2 - d1 * d6
    if (vb <= 0.0f && d2 >= 0.0f && d6 <= 0.0f) {
      val w = d2 / (d2 - d6)
      return a + w * ac; // barycentric coordinates (1-w,0,w)
    }
    // Check if P in edge region of BC, if so return projection of P onto BC
    val va = d3 * d6 - d5 * d4
    if (va <= 0.0f && (d4 - d3) >= 0.0f && (d5 - d6) >= 0.0f) {
      val w = (d4 - d3) / ((d4 - d3) + (d5 - d6));
      return b + w * (c - b); // barycentric coordinates (0,1-w,w)
    }
    // P inside face region. Compute Q through its barycentric coordinates (u,v,w)
    val denom = 1.0f / (va + vb + vc)
    val v = vb * denom
    val w = vc * denom
    return a + ab * v + ac * w; // = u*a + v*b + w*c, u = va * denom = 1.0f - v - w
  }

  override def intersects(other: Shape): Boolean = other match {
    case o@Triangle(_, _, _) => Intersects.triangleToTriangle(this, o)
    case o@AABox(_, _) => Intersects.aaboxToTriange(o, this)
    case o@OBox(_, _, _) => Intersects.oboxToTriange(o, this)
    case o@TriangleSoup(_) => o.intersects(this)
    case o@PointShape(p2) => Intersects.pointToTriangle(p2, this)
    case _ => false
  }

  def points: Seq[V3] = Seq(a, b, c)

  override def aabb: AABox = AABox.fromPoints(points)

  //todo test
  override def boundingSphere: Sphere = {
    val radius = (ab.length + bc.length + ac.length) * .5f / ((v1 - v2) ^ (v2 - v3)).length
    val divisor = (((v1 - v2) ^ (v2 - v3)).length squared) * 2
    val aa = ((v2 - v3).length squared) * ((v1 - v2) ** (v1 - v3)) / divisor
    val bb = ((v1 - v3).length squared) * ((v2 - v1) ** (v2 - v3)) / divisor
    val cc = ((v1 - v2).length squared) * ((v3 - v1) ** (v3 - v2)) / divisor

    val center = v1 * aa + v2 * bb + v3 * cc

    Sphere(center, radius)
  }

  override def transform(t: Transform): Triangle = Triangle(t.transformPosition(a), t.transformPosition(b), t.transformPosition(c))

  override def normalAt(point: V3): Normal = normal
}
