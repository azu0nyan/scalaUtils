package utils.mesh.factory

import utils.math.Scalar
import utils.math.planar.{PolygonRegion, V2}
import utils.math.space.{Plane, V3}
import utils.mesh.MutableMeshSection

import scala.collection.mutable.ArrayBuffer

object Polygon {
  def apply(p: PolygonRegion, height: Scalar, up: Boolean, uvParams: UVGenParams): MutableMeshSection =
    this (p, Plane(V3.upAxis * height, V3.forwardAxis, if (up) V3.leftAxis else V3.rightAxis), uvParams)

  def apply(p: PolygonRegion, plane: Plane, uvParams: UVGenParams = UVGenParams.defaultParams): MutableMeshSection = {
    val normal = plane.normal
    new MutableMeshSection(
      p.vertices.map(v => plane.toWorldCords(v)).to(ArrayBuffer),
      p.triangulationIndices.to(ArrayBuffer),
      (0 until p.verticesCount).map(_ => normal).to(ArrayBuffer),
      p.vertices.map(m => uvParams.transform(m)).to(ArrayBuffer)
    )
  }
}
