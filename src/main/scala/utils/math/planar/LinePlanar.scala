package utils.math.planar

import utils.math._
import utils.math.linalg.LinearSystemSolver

case class LinePlanar(origin: V2, direction: UnitV2) {
  //todo optimize
  def intersection(s: LinePlanar): Option[V2] = {
    SegmentPlanar(origin - direction * KINDA_BIG_NUMBER, origin + direction * KINDA_BIG_NUMBER).intersection(
      SegmentPlanar(s.origin - s.direction * KINDA_BIG_NUMBER, s.origin + s.direction * KINDA_BIG_NUMBER)
    ).flatMap {
      case PointIntersection(p) => Some(p)
      case _ => None
    }
    /*LinearSystemSolver.solve(Seq(
      Seq(direction.x, -s.direction.x, s.origin.x - origin.x),
      Seq(direction.y, -s.direction.y, s.origin.y- origin.y),
    )) map {
      case Seq(a, b) => V2(a,b)
    }*/
  }

  def parallel(other: LinePlanar): Boolean = direction.collinear(other.direction)

  def inLineCoordinates(f: Scalar): V2 = origin + f * direction

  def normal: UnitV2 = direction.rotate(HALF_PI)

  def distanceTo(point: V2): Scalar = normal ** (point - origin)

  def contains(v: V2): Boolean = distanceTo(v) ~= 0 //todo mb do through line equation

  def contains(s: SegmentPlanar): Boolean = contains(s.v1) && contains(s.v2)

  def projectOnLine(point: V2): V2 = point - (distanceTo(point) * normal)

  def toLineProjectionCordinates(point: V2): Scalar = {
    val projected = projectOnLine(point)
    val dist = projected - origin
    if (dist.sameDirection(direction)) dist.length
    else -dist.length
  }

  def toSegment: SegmentPlanar = SegmentPlanar(origin, origin + direction)

  def tan: Option[Scalar] = Option.when(direction.x != 0)(direction.y / direction.x)

  def freeCoeff: Option[Scalar] = tan.map(k => origin.y - k * origin.x)

  def y(x: Scalar): Option[Scalar] = tan.flatMap(k => freeCoeff.map(b => k * x + b))

}

object LinePlanar {
  def fromTwoPoints(v1: V2, v2: V2): LinePlanar = LinePlanar(v1, v2 - v1)
}