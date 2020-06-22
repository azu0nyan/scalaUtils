package utils.mesh.factory

import utils.math.Scalar
import utils.math.planar.patch._
import utils.math.planar.{PolygonRegion, V2}
import utils.math.space.Plane
import utils.mesh.MutableMeshSection

object Circle {
  def apply(
             radius: Scalar,
             sections: Int,
             plane: Plane,
             uvGenParams: UVGenParams = UVGenParams.defaultParams
           ):MutableMeshSection = Polygon(PolygonRegion.from(Arc.Circle(V2.ZERO, radius), sections), plane, uvGenParams)

}
