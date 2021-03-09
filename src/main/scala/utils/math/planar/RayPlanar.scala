package utils.math.planar

case class RayPlanar(origin: V2, direction: UnitV2)  {
  def line:LinePlanar = LinePlanar(origin, direction)

}
