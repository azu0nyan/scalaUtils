package utils.mesh.factory

import utils.math.planar.PolygonRegion
import utils.math.space.{Plane, ProjectionPlane, V3}
import utils.mesh.{MeshSection, MutableMeshSection}

object ProjectPolygon {
  def apply(polygon: PolygonRegion,
            polygonPlane: Plane,
            toProjectOn: ProjectionPlane,
            inverseTriangles: Boolean = false,
            uvParams: UVGenParams = UVGenParams.defaultParams,
            overrideNormal: Option[V3] = None): MutableMeshSection = {
    val res = new MutableMeshSection()
    val normal = overrideNormal.getOrElse(polygonPlane.normal)
    val toProject = polygon.projectOn(polygonPlane)
    val projectionClamped = toProject
      .map(v => toProjectOn.projectionOf(v))
    //.map(v => toProjectOn.clampPoint(v, clamp))

    res.vertices ++= projectionClamped
    ////
    if (inverseTriangles) {
      res.triangles ++= polygon.triangulationIndices.map(x => (x._1, x._3, x._2))
    } else {
      res.triangles ++= polygon.triangulationIndices
    }
    ///
    val vertsNormal = normal
    res.normals ++= (0 until polygon.verticesCount).map(_ => vertsNormal)


    res.uvs ++= projectionClamped.map(v => toProjectOn.inPlaneCords(v)).map(v => uvParams.transform(v))
    res
  }


}
