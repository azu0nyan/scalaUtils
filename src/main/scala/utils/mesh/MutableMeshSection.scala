package utils.mesh

import java.nio.FloatBuffer

import utils.datastructures.IndexedTriangle
import utils.math._
import utils.math.planar.V2
import utils.math.space.{Triangle, V3}

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
  def addOther(ot:MeshSection):MutableMeshSection = this ++= ot


  def ++=(ot: MeshSection): MutableMeshSection = {
    //combining
    val vCount = vertices.length
    vertices ++= ot.vertices
    triangles ++= ot.triangles.map(v => (v._1 + vCount, v._2 + vCount, v._3 + vCount))
    normals ++= ot.normals
    uvs ++= ot.uvs
    this
  }

  def invertSidesInPlace:MutableMeshSection = {
    for(i <- triangles.indices){
      val tri = triangles(i)
      triangles(i) = (tri._1, tri._3, tri._2)
    }
    for(i <- normals.indices){
      normals(i) = -normals(i)
    }
    this
  }





  }

