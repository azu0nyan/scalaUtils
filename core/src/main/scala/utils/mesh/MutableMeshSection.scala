package utils.mesh

import utils.datastructures.IndexedTriangle
import utils.math.*
import utils.math.planar.V2
import utils.math.space.{Triangle, V3}

import java.nio.FloatBuffer
import scala.collection.mutable.ArrayBuffer

class MutableMeshSection(
                          override val vertices: ArrayBuffer[V3],
                          override val triangles: ArrayBuffer[(Int, Int, Int)],
                          override val normals: ArrayBuffer[V3],
                          override val uvs: ArrayBuffer[V2]
                        ) extends MeshSection {

  def this() = {
    this(ArrayBuffer(),
      ArrayBuffer(),
      ArrayBuffer(),
      ArrayBuffer())
  }

  def this(meshSection: MeshSection) = {
    this()
    addOther(meshSection)

  }
  def addOther(ot: MeshSection): MutableMeshSection = this ++= ot


  def ++=(ot: MeshSection): MutableMeshSection = {
    //combining
    val vCount = vertices.length
    vertices ++= ot.vertices
    triangles ++= ot.triangles.map(v => (v._1 + vCount, v._2 + vCount, v._3 + vCount))
    normals ++= ot.normals
    uvs ++= ot.uvs
    this
  }

  def addAndMap(ot: MeshSection,
                posMap: V3 => V3 = v3 => v3,
                uvMap: V2 => V2 = v2 => v2,
                normalMap: V3 => V3 = v3 => v3): MutableMeshSection = {
    val vCount = vertices.length
    vertices ++= ot.vertices.map(posMap)
    triangles ++= ot.triangles.map(v => (v._1 + vCount, v._2 + vCount, v._3 + vCount))
    normals ++= ot.normals.map(normalMap)
    uvs ++= ot.uvs.map(uvMap)
    this
  }

  def invertSidesInPlace: MutableMeshSection = {
    for (i <- triangles.indices) {
      val tri = triangles(i)
      triangles(i) = (tri._1, tri._3, tri._2)
    }
    for (i <- normals.indices) {
      normals(i) = -normals(i)
    }
    this
  }

  def flipTrianglesToMatchNormal(otherNormal: V3): MutableMeshSection = {
    for {
      i <- triangles.indices;
      (i1, i2, i3) = triangles(i)
      triangle = Triangle(vertices(i1), vertices(i2), vertices(i3))
      if triangle.flipToMatchNormal(otherNormal) != triangle
    }
      triangles(i) = (i1, i3, i2)

    this
  }

}

