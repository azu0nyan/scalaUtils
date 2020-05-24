package utils.math.space

import utils.math._
case class Line(origin: V3, direction: UnitV3) {

  def secondPoint:V3 = origin + direction

  //A + dot(AP,AB) / dot(AB,AB) * AB
  def projectPoint(p:V3):V3 = origin + direction * ((p - origin) ** direction / (direction ** direction))

  def parallel(other:Line):Boolean = direction.collinear(other.direction)

  def inLineCoordinates(f:Scalar): V3 = origin +  direction * f

  //TODO pick one
  def planeIntersection(p:Plane):Option[V3] = {
    // Compute the t value for the directed line ab intersecting the plane
    val divisor = p.normal **  direction

    if(divisor == 0f) {
      return None
    }
    val t = (p.dot - p.normal ** origin) / divisor

    Some(origin + t * direction)
  }


  //http://geomalgorithms.com/a05-_intersect-1.html
  def intersection(plane: Plane):LinesIntersection =
    if (direction ** plane.normal ~= 0f) {
      //line parallel to plane
      if((origin - plane.origin) ** plane.normal ~= 0f)LineLineIntersection(this)
      else NoLineIntersection()
    } else {
      val intersectionPoint = -(plane.normal  ** plane.fromOrigin(origin)) / (direction ** plane.normal)
      PointLineIntersection(inLineCoordinates(intersectionPoint))
    }
}

abstract class LinesIntersection()

case class NoLineIntersection() extends LinesIntersection

case class PointLineIntersection(point:V3) extends LinesIntersection

case class LineLineIntersection(l: Line) extends LinesIntersection
