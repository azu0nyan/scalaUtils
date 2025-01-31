package utils.mesh.factory

import utils.math.planar.V2
import utils.math.space.V3
import utils.mesh.MutableMeshSection

import scala.collection.mutable.ArrayBuffer

object Triangle {


  def apply(t: utils.math.space.Triangle/*, uvParams: UVGenParams = UVGenParams()*/): MutableMeshSection = {
    val normal = ((t.v2 - t.v1) ^ (t.v3 - t.v1)).normalize
    new MutableMeshSection(
      ArrayBuffer(t.v1, t.v2, t.v3),
      ArrayBuffer((0, 1, 2)),
      ArrayBuffer(normal, normal, normal),
      ArrayBuffer(V2(0d, 0d), V2(1d, 0d), V2(0d, 1d))
    )
  }

//  def  MeshSectionTriangulatedPoly(poly:Seq[V3]) extends MeshSection {
//    for(i <- 1 to (poly.length - 2)) {
//      this ++= new MeshSectionTriangle(poly(0), poly(i), poly(i + 1))
//    }
//  }


}
