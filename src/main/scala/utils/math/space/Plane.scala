package utils.math.space

import utils.datastructures.IntV2
import utils.math.planar._
import utils.math._

object Plane {

  /** cartesian form: ax + by + cz = d */
  def fromCartesianForm(a: Scalar, b: Scalar, c: Scalar, d: Scalar): Plane = {
    if (a == 0) { // todo check correctness
      Plane(V3.ZERO, V3(0, 1, 0), V3(0, 0, 1))
    } else {
      Plane(
        V3(d / a, 0, 0),
        V3(-b / a, 1, 0),
        V3(-c / a, 0, 1),
      )
    }
  }

  /* (x - p)*n == 0   todo check correctness */
  def fromPointAndNormal(point: V3, normal: Normal): Plane = fromCartesianForm(point.x, point.y, point.z, normal ** point)

  //todo test
  def fromAxisIntersections(x: Scalar, y: Scalar, z: Scalar): Plane = {
    val origin = V3(x, 0, 0)
    val xx = V3(0, y, 0)
    val yy = V3(0, 0, z)
    from3Points(origin, xx - origin, yy - origin)
  }

  //todo test
  def from3Points(p1: V3, p2: V3, p3: V3): Plane = Plane(p1, p2 - p1, p3 - p1)

  /**
    * x = o + x * x" + u * y"
    * x",y" <- R
    * o, x, y <- V3   */
  def fromParametricForm(origin: V3, x: V3, y: V3): Plane = Plane(origin, x, y)
}


case class Plane(origin: V3, x: V3, y: V3) extends NormalAndClosest {

  def offset(offset: V3): Plane = Plane(origin + offset, x, y)

  def offset(planeCords: IntV2): Plane = Plane(toWorldCords(planeCords.toV2), x, y)

  def fromOrigin(point: V3): V3 = point - origin

  def normal: UnitV3 = x ^ y

  def dot: Scalar = normal ** origin

  override def distanceTo(point: V3): Scalar = normal ** (point - origin)

  override def closestPoint(point: V3): V3 = point - (distanceTo(point) * normal)

  override def normalAt(point: V3): Normal = if ((point - origin) ** normal > 0) normal else -normal

  def contains(point: V3): Boolean = distanceTo(point) ~= 0f

  def inPlaneCords(point: V3): V2 = V2(closestPoint(point - origin) ** x, closestPoint(point - origin) ** y)

  def toWorldCords(planeCords: V2): V3 = origin + (x * planeCords.x) + (y * planeCords.y)

  def toWorldCords(planeCords: V2, height: Scalar): V3 = origin + (x * planeCords.x) + (y * planeCords.y) + (normal * height)

  def clampPoint(point: V3, planeCords: Rectangle): V3 = toWorldCords(inPlaneCords(point).clampSnap(planeCords.min, planeCords.max))

  def clampPoint(point: V3, clampType: ClampType): V3 = clampType match {
    case NoClamp() => point
    case RectangularClamp(rectangle) => clampPoint(point, rectangle)
  }

  def projectOnPlane(point: V3, projectionType: ProjectionType): V3 = projectionType match {
    case ProjectOnPlane() => closestPoint(point)
    case InverseProject(projectionDirection) => Line(point, projectionDirection).intersection(this) match {
      case PointLineIntersection(p) => p
      case LineLineIntersection(_) => point
      case NoLineIntersection() => closestPoint(point)
    }
  }

}

class ProjectionPlane(val plane: Plane, projectionType: ProjectionType, clampType: ClampType) {
  def projectionOf(v: V3): V3 = plane.clampPoint(plane.projectOnPlane(v, projectionType), clampType)

  def inPlaneCords(point: V3): V2 = plane.inPlaneCords(point)
}

sealed abstract class ProjectionType

case class ProjectOnPlane() extends ProjectionType

case class InverseProject(projectionDirection: V3) extends ProjectionType

sealed abstract class ClampType

case class NoClamp() extends ClampType

case class RectangularClamp(rectangle: Rectangle) extends ClampType