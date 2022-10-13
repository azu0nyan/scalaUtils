package utils.math.space

case class Ray(origin: V3, direction: UnitV3) {
  def toLine: Line = Line(origin, direction)


  def planeIntersection(p:Plane):Option[V3] = {
    // Compute the t value for the directed line ab intersecting the plane

    val divisor = p.normal **  direction

    if(divisor == 0f) {
      return None
    }
    val t = (p.dot - p.normal ** origin) / divisor

    // If t in [0..1] compute and return intersection point
    if (t >= 0.0f && t <= 1.0f) {
      Some(origin + direction * t)
    }
    // Else no intersection
    None
  }


}




