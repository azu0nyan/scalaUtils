package utils.math.planar.algo.polygonClipping

import utils.math.planar.{Polygon, PolygonRegion}

object PolygonClippingImplicits {


  implicit def polygonRegionToPolygon(p: PolygonRegion): Polygon = p.asPolygon

  implicit class PolyImplicits(val p: Polygon) extends AnyVal {
    def &(ot: Polygon): Polygon = PolygonClipping.intersection(p, ot)
    def |(ot: Polygon): Polygon = PolygonClipping.union(p, ot)
    def &~(ot: Polygon): Polygon = PolygonClipping.difference(p, ot)
    def ^(ot: Polygon): Polygon = PolygonClipping.xor(p, ot)
  }

}
