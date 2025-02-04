package utils.math.space

import utils.datastructures.IntV2
import utils.datastructures.spatial.AARectangle
import utils.math.planar._
import utils.math._
import utils.math.space.Line.LinesIntersection._

object Plane {

  /** cartesian form: ax + by + cz = d */
  inline def fromCartesianForm(a: Scalar, b: Scalar, c: Scalar, d: Scalar): Plane = {
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
  inline def fromPointAndNormal(point: V3, normal: Normal): Plane = fromCartesianForm(point.x, point.y, point.z, normal ** point)

  //todo test
  inline def fromAxisIntersections(x: Scalar, y: Scalar, z: Scalar): Plane = {
    val origin = V3(x, 0, 0)
    val xx = V3(0, y, 0)
    val yy = V3(0, 0, z)
    from3Points(origin, xx - origin, yy - origin)
  }

  //todo test
  inline def from3Points(p1: V3, p2: V3, p3: V3): Plane = Plane(p1, p2 - p1, p3 - p1)

  /**
   * x = o + x * x" + u * y"
   * x",y" <- R
   * o, x, y <- V3 */
  inline def fromParametricForm(origin: V3, x: V3, y: V3): Plane = Plane(origin, x, y)
}


case class Plane(origin: V3, x: V3, y: V3) extends NormalAndClosest {

  inline def offset(offset: V3): Plane = Plane(origin + offset, x, y)

  inline def offset(planeCords: IntV2): Plane = Plane(toWorldCords(planeCords.toV2), x, y)

  inline def offsetUp(offset: Scalar): Plane = Plane(origin + normal * offset, x, y)

  inline def fromOrigin(point: V3): V3 = point - origin

  inline def normal: UnitV3 = x ^ y

  inline def dot: Scalar = normal ** origin

  /**flips x and y axis, efficiently inverts plane normal.
   *  you have to swap plane coordinates, to get same world position on inverted plane (x, y) -> (y, x)*/
  inline def invert: Plane = Plane(origin, y, x)

  override inline def distanceTo(point: V3): Scalar = normal ** (point - origin)

  override inline def closestPoint(point: V3): V3 = point - (distanceTo(point) * normal)

  override inline def normalAt(point: V3): Normal = if ((point - origin) ** normal > 0) normal else -normal

  inline def contains(point: V3): Boolean = distanceTo(point) ~= 0f

  inline def inPlaneCords(point: V3): V2 = V2(closestPoint(point - origin) ** x, closestPoint(point - origin) ** y)

  inline def toWorldCords(planeCords: V2): V3 = toWorldCords(planeCords.x, planeCords.y)

  inline def toWorldCords(planeCords: V2, height: Scalar): V3 = toWorldCords(planeCords.x, planeCords.y, height)

  inline def toWorldCords(planeCords: V3): V3 = toWorldCords(planeCords.x, planeCords.y, planeCords.z)

  inline def toWorldCords(planeCordsX: Scalar, planeCordsY: Scalar, planeCordsZ: Scalar = 0d): V3 =
    origin + (x * planeCordsX) + (y * planeCordsY) + (normal * planeCordsZ)

  inline def toWorldCordsVector(planeCords: V2): V3 = toWorldCordsVector(planeCords.x, planeCords.y)

  inline def toWorldCordsVector(planeCords: V3): V3 = toWorldCordsVector(planeCords.x, planeCords.y, planeCords.z)

  /** ignores origin */
  inline def toWorldCordsVector(planeCordsX: Scalar, planeCordsY: Scalar, planeCordsZ: Scalar = 0d): V3 =
    (x * planeCordsX) + (y * planeCordsY) + (normal * planeCordsZ)

  inline def clampPoint(point: V3, planeCords: AARectangle): V3 = toWorldCords(inPlaneCords(point).clampSnap(planeCords.min, planeCords.max))

  inline def clampPoint(point: V3, clampType: ClampType): V3 = clampType match {
    case NoClamp() => point
    case RectangularClamp(rectangle) => clampPoint(point, rectangle)
  }

  inline def projectOnPlane(point: V3, projectionType: ProjectionType): V3 = projectionType match {
    case ProjectOnPlane() => closestPoint(point)
    case InverseProject(projectionDirection) => Line(point, projectionDirection).intersection(this) match {
      case PointLineIntersection(p) => p
      case LineLineIntersection(_) => point
      case NoLineIntersection() => closestPoint(point)
    }
  }

}

class ProjectionPlane(val plane: Plane, projectionType: ProjectionType, clampType: ClampType) {
  inline def projectionOf(v: V3): V3 = plane.clampPoint(plane.projectOnPlane(v, projectionType), clampType)

  inline def inPlaneCords(point: V3): V2 = plane.inPlaneCords(point)
}

sealed abstract class ProjectionType

case class ProjectOnPlane() extends ProjectionType

case class InverseProject(projectionDirection: V3) extends ProjectionType

sealed abstract class ClampType

case class NoClamp() extends ClampType

case class RectangularClamp(rectangle: AARectangle) extends ClampType