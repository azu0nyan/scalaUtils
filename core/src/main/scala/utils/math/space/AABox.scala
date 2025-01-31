package utils.math.space

import utils.datastructures.spatial.TriangleSoup
import utils.math.*
import utils.math.space.Shape.PointShape
import utils.math.space.intersections.Intersects

object AABox {
  //implicit def toArea(b: AABox): Area = Area.AABoxArea(b)
  def fromCenterAndHalfExtends(c: V3, e: V3) = AABox(c - e, c + e)

  def fromPoints(p: Seq[V3]): AABox = {
    if(p.isEmpty){
      return AABox(V3.ZERO, V3.ZERO)
    }
    var minx = p(0).x
    var miny = p(0).y
    var minz = p(0).z
    var maxx = p(0).x
    var maxy = p(0).y
    var maxz = p(0).z
    p.foreach { v =>
      minx = math.min(minx, v.x)
      maxx = math.max(maxx, v.x)
      miny = math.min(miny, v.y)
      maxy = math.max(maxy, v.y)
      minz = math.min(minz, v.z)
      maxz = math.max(maxz, v.z)
    }
    return AABox(V3(minx, miny, minz), V3(maxx, maxy, maxz))
  }

  def fromSize(size:V3): AABox = new AABox(-size * HALF, size * HALF)
  def fromHalfExtents(halfExtents:V3): AABox = new AABox(- halfExtents, halfExtents)

}

case class AABox(min: V3, max: V3) extends Shape with NormalAndClosest {

  def this(size: V3) = this(-size * HALF, size * HALF)

  def orientedBox: OBox = OBox(center, (V3.x, V3.y, V3.z), halfExtents)

  @inline def volume: Scalar = width * depth * height

  @inline def depth: Scalar = max.z - min.z

  @inline def width: Scalar = max.x - min.x

  @inline def height: Scalar = max.y - min.y

  @inline def whd: V3 = max - min

  @inline def halfExtents: V3 = whd * HALF

  override def closestPoint(point: V3): V3 = clampPoint(point)

  //TODO test
  override def distanceSquared(p: V3): Scalar = {
    // For each axis count any excess distance outside box extents
    var sqDist: Scalar = 0
    if (p.x < min.x) sqDist += (min.x - p.x) * (min.x - p.x)
    if (p.x > max.x) sqDist += (p.x - max.x) * (p.x - max.x)
    if (p.y < min.y) sqDist += (min.y - p.y) * (min.y - p.y)
    if (p.y > max.y) sqDist += (p.y - max.y) * (p.y - max.y)
    if (p.z < min.z) sqDist += (min.z - p.z) * (min.z - p.z)
    if (p.z > max.z) sqDist += (p.z - max.z) * (p.z - max.z)
    return sqDist
  }

  @inline def clampPoint(point: V3): V3 = clamp(point, min, max)


  @inline def intersectsWith(ot: AABox): Boolean =
    max.x > ot.min.x && min.x < ot.max.x &&
      max.y > ot.min.y && min.y < ot.max.y &&
      max.z > ot.min.z && min.z < ot.max.z

  @inline def contains(p: V3): Boolean = p.x >= min.x && p.x <= max.x && p.y >= min.y && p.y <= max.y && p.z >= min.z && p.z <= max.z

  @inline def contains(ot: AABox): Boolean = ot.min.x >= min.x && ot.max.x <= max.x && ot.min.y >= min.y && ot.max.y <= max.y && ot.min.z >= min.z && ot.max.z <= max.z

  @inline def combine(ot: AABox): AABox = AABox(
    V3(math.min(min.x, ot.min.x), math.min(min.y, ot.min.y), math.min(min.z, ot.min.z)),
    V3(math.max(max.x, ot.max.x), math.max(max.y, ot.max.y), math.max(max.z, ot.max.z))
  )

  @inline def intersection(ot: AABox): AABox = AABox(
    V3(math.max(min.x, ot.min.x), math.max(min.y, ot.min.y), math.max(min.z, ot.min.z)),
    V3(math.min(max.x, ot.max.x), math.min(max.y, ot.max.y), math.min(max.z, ot.max.z))
  )

  @inline def center: V3 = (min + max) * HALF

  def splitAllAxis: Seq[AABox] = split(Seq(0, 1, 2))

  def split(axises: Seq[Int]): Seq[AABox] = axises.headOption match {
    case Some(axis) => split(axis).flatMap(box => box.split(axises.tail))
    case None => Seq(this)
  }


  def split(axis: Int): Seq[AABox] = axis % 3 match {
    case 0 => Seq(AABox(min, max.replaceX(center.x)), AABox(min.replaceX(center.x), max))
    case 1 => Seq(AABox(min, max.replaceY(center.y)), AABox(min.replaceY(center.y), max))
    case 2 => Seq(AABox(min, max.replaceZ(center.z)), AABox(min.replaceZ(center.z), max))
  }


  override def intersects(other: Shape): Boolean = other match {
    case box2@AABox(_, _) => intersectsWith(box2)
    case PointShape(p2) => Intersects.pointToAABox(p2, this)
    case o@OBox(_, _, _) => Intersects.aaBoxToOBox(this, o)
    case o@Triangle(_, _, _) => Intersects.aaboxToTriange(this, o)
    case o@Sphere(_, _) => Intersects.aaboxToSphere(this, o)
    case o@TriangleSoup(_) => Intersects.triangleSoupToAABox(o, this)
    case _ => false
  }

  override def aabb: AABox = this

  override def boundingSphere:Sphere = Sphere(center, halfExtents.length)

  override def transform(t: Transform): AABox = {
    val newExtents = whd * t.scale * HALF
    val newCenter = t.transformPosition(center)
    return AABox(newCenter - newExtents, newCenter + newExtents)
  }

  /*if(t.isIdRotation){
    val newExtents = whd * t.scale / 2f
    val newCenter = t.transformPosition(center)
    return  AABox(newCenter - newExtents, newCenter + newExtents)
  }else {
    return orientedBox.transform(t)
  }*/
  lazy val angles: Seq[V3] = Seq(
    V3(min.x, min.y, min.z),
    V3(max.x, min.y, min.z),
    V3(min.x, min.y, max.z),
    V3(max.x, min.y, max.z),
    V3(min.x, max.y, min.z),
    V3(max.x, max.y, min.z),
    V3(min.x, max.y, max.z),
    V3(max.x, max.y, max.z),
  )
  /*
    *     v6________ max
    *    /|          /|
    *   / |         / |
    * v4__|________v5 |
    * |   |        |  |    Y
    * |   v2_______| v3   /\  Z
    * |  /         | /    |  /
    * | /          |/     |/
    * min___________v1    -----> X
*/
  lazy val edges: Seq[Segment] = Seq(
    Segment(min, V3(max.x, min.y, min.z)),
    Segment(V3(max.x, min.y, min.z), V3(max.x, min.y, max.z)),
    Segment(V3(max.x, min.y, max.z), V3(min.x, min.y, max.z)),
    Segment(V3(min.x, min.y, max.z), min),
    Segment(V3(min.x, max.y, min.z), V3(max.x, max.y, min.z)),
    Segment(V3(max.x, max.y, min.z), V3(max.x, max.y, max.z)),
    Segment(V3(max.x, max.y, max.z), V3(min.x, max.y, max.z)),
    Segment(V3(min.x, max.y, max.z), V3(min.x, max.y, min.z)),
    Segment(V3(min.x, min.y, min.z), V3(min.x, max.y, min.z)),
    Segment(V3(min.x, min.y, max.z), V3(min.x, max.y, max.z)),
    Segment(V3(max.x, min.y, min.z), V3(max.x, max.y, min.z)),
    Segment(V3(max.x, min.y, max.z), V3(max.x, max.y, max.z)),
  )

  override def normalAt(point: V3): Normal = angles.find(_ ~= point).map(a => V3.toNormal(a - center))
    .getOrElse(edges.find(_.distanceToSquared(point) ~= 0f).map(s => V3.toNormal(s.center - center))
      .getOrElse {
        if (point.x ~= min.x) {
          V3(-1, 0, 0)
        } else if (point.x ~= max.x) {
          V3(1, 0, 0)
        } else if (point.y ~= min.y) {
          V3(0, -1, 0)
        } else if (point.y ~= max.y) {
          V3(0, 1, 0)
        } else if (point.z ~= min.z) {
          V3(0, 0, -1)
        } else if (point.z ~= max.z) {
          V3(0, 0, 1)
        } else {
          //should'nt be happened if input correct
          V3(0, 0, 0)
        }
      })
}