package utils.math.planar

import utils.math._
import utils.math.linalg.LinearSystemSolver

case class LinePlanar(origin: V2, direction: UnitV2) {
  def intersection(s: LinePlanar): Option[V2] = {
    LinearSystemSolver.solve(Seq(
      Seq(direction.x, -s.direction.x, s.origin.x - origin.x),
      Seq(direction.y, -s.direction.y, s.origin.y- origin.y),
    )) map {
      case Seq(a, b) => V2(a,b)
    }
  }

  def parallel(other:LinePlanar):Boolean = direction.collinear(other.direction)

  def inLineCoordinates(f:Scalar): V2 = origin + f * direction

  def normal:UnitV2 = direction.rotate(HALF_PI)

  def distanceTo(point:V2):Scalar = normal ** (point - origin)

  def projectOnLine(point:V2):V2 = point - (distanceTo(point) * normal)

  def toSegment:SegmentPlanar = SegmentPlanar(origin, origin + direction)

  def tan:Option[Scalar] = Option.when(direction.x != 0 )(direction.y / direction.x)

  def freeCoeff:Option[Scalar] = tan.map(k => origin.y - k * origin.x)

  def y(x:Scalar) :Option[Scalar] = tan.flatMap(k => freeCoeff.map(b => k * x + b))

}
