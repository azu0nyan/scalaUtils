package utils.math.planar

case class RayPlanar(linePlanar: LinePlanar) extends AnyVal {
  def origin: V2 = linePlanar.origin
  def direction: UnitV2 = linePlanar.direction
  def line: LinePlanar = linePlanar

  def intersectionPoint(other: RayPlanar): Option[V2] = {
    line.intersection(other.line) filter { p =>
      linePlanar.toLineProjectionCordinates(p) >= 0 &&
        other.linePlanar.toLineProjectionCordinates(p) >= 0
    }
  }

}
