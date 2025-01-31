package utils.math.space

import utils.datastructures.spatial.TriangleSoup
import utils.math.*
import utils.math.space.Shape.PointShape
import utils.math.space.intersections.Intersects




case class Sphere(center: V3, r: Scalar) extends Shape with NormalAndClosest {

  /**
    * @param inclination 0 to PI
    * @param azimuth 0 to 2PI
    */
  def fromSphericalCoordinates(inclination:Angle, azimuth:Angle):V3 = utils.math.fromSphericalCoordinates(r, inclination, azimuth) + center

  override def intersects(other: Shape): Boolean = other match {
    case o@AABox(_, _) => Intersects.aaboxToSphere(o, this)
    case o@Sphere(_, _) => Intersects.sphereToSphere(o, this)
    case o@OBox(_,_,_) => Intersects.oboxToSphere(o, this)
    case o@Triangle(_, _, _) =>  Intersects.triangleToSphere(o, this)
    case o@PointShape(p2) => Intersects.pointToSphere(p2, this)
    case o@TriangleSoup(_) => Intersects.triangleSoupToSphere(o, this)
    case _ => false
  }

  override def aabb: AABox = AABox.fromCenterAndHalfExtends(center, r)

  override def boundingSphere:Sphere = this

  override def closestPoint(point: V3): V3 = {
    val centerToPoint = point - center
    //if (centerToPoint.length <= r) return point
    return center + (centerToPoint.normalize * r)
  }

  def contains(p:V3):Boolean = p.distanceSquared(center) <= r*r

  override def distanceSquared(point: V3): Scalar = super.distanceSquared(point)

  def intersects(ot: Sphere): Boolean = center.distance(ot.center) < r + ot.r

  override def transform(t: Transform): Sphere = Sphere(t.transformPosition(center), r * t.scale.x)

  override def normalAt(point: V3): V3 = V3.toNormal( point - center)
}
