package utils.math.space

import utils.datastructures.spatial.TriangleSoup
import utils.math.Normal
import utils.math.space.Transform.Transformable
import utils.math.space.intersections.Intersects
import utils.math._

object Shape {


  case class PointShape(p: V3) extends Shape with ClosestPoint {

    override def intersects(other: Shape): Boolean = other match {
      case o@AABox(_, _) => Intersects.pointToAABox(p, o)
      case o@Sphere(_, _) => Intersects.pointToSphere(p, o)
      case o@OBox(_, _, _) => Intersects.pointToOBox(p, o)
      case o@Triangle(_, _, _) => Intersects.pointToTriangle(p, o)
      case o@PointShape(p2) => Intersects.pointToPoint(p, p2)
      case o@TriangleSoup(_) => Intersects.triangleSoupToPoint(o, p)
      case _ => false
    }

    override def boundingSphere: Sphere = Sphere(p, 0)

    override def aabb: AABox = AABox(p, p)

    override def closestPoint(point: V3): V3 = p

    override def transform(t: Transform): PointShape = PointShape(t.transformPosition(p))

  }

}

trait WithAABox {
  def aabb: AABox
}

trait WithBoundingSphere{
  def boundingSphere: Sphere
}

trait Shape
  extends Transformable[Shape]
    with WithAABox
    with WithBoundingSphere {

  override def transform(t: Transform): Shape

  def intersects(other: Shape): Boolean

  //def toTrianglesList:Seq[Triangle]

  //  def contains(point: V3): Boolean





}

trait ClosestPoint {

  def closestPoint(point: V3): V3

  def distanceSquared(point: V3): Scalar = point.distanceSquared(closestPoint(point))

  def distanceTo(point: V3): Scalar = sqrt(distanceSquared(point))

}

trait NormalsAtPoint {
  def normalAt(point: V3): Normal
}

trait NormalAndClosest extends ClosestPoint with NormalsAtPoint {
  def closestPointNormal(p: V3): Normal = normalAt(closestPoint(p))
}


case class TriangleList(triangles:Seq[Triangle]) extends Shape {
  override def transform(t: Transform): TriangleList = TriangleList(triangles.map(_.transform(t)))

  override def intersects(other: Shape): Boolean = ???

  override def boundingSphere: Sphere = aabb.boundingSphere

  override def aabb: AABox = triangles.map(_.aabb).reduceOption(_ combine _ ).getOrElse(AABox(V3.ZERO, V3.ZERO))
}


