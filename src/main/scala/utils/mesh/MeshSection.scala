package utils.mesh

import utils.datastructures.IndexedTriangle
import utils.datastructures.containers.{IntSet, IntToIntBucketMap}
import utils.math.planar.V2
import utils.math.space.{Triangle, V3}

import scala.collection.immutable.IntMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait MeshSection {

  def vertices: collection.IndexedSeq[V3]
  def triangles: collection.IndexedSeq[(Int, Int, Int)]
  def normals: collection.IndexedSeq[V3]
  def uvs: collection.IndexedSeq[V2]

  def inverted: MeshSection = new MeshSection {
    override val vertices: collection.IndexedSeq[V3] = MeshSection.this.vertices
    override val triangles: collection.IndexedSeq[(Int, Int, Int)] =
      MeshSection.this.triangles.map(tri => (tri._1, tri._3, tri._2))
    override val normals: collection.IndexedSeq[V3] = MeshSection.this.normals.map(-_)
    override val uvs: collection.IndexedSeq[V2] = MeshSection.this.uvs
  }

  def verticesAsFloatSeq: Seq[Float] = {
    vertices.flatMap(v => Seq(v.x, v.y, v.z)).map(_.toFloat).toSeq
  }

  def trianglesAsIntSeq: Seq[Int] = {
    triangles.flatMap(v => Seq(v._1, v._2, v._3)).toSeq
  }

  def trianglesSeq: Seq[Triangle] = {
    triangles.map(tr => IndexedTriangle(tr, vertices).triangle).toSeq
  }

  def triangleCount(): Int = {
    triangles.size
  }

  override def toString = s"MeshSection[verts:${vertices.size}, tris:${triangles.size}, uvs:${uvs.size}, normals:${normals.size}])"

  def splitByParts(maxVertices: Int): Seq[MeshSection] = {
    if (vertices.size <= maxVertices) Seq(this)
    else {
      var res:Seq[MutableMeshSection] = Seq()
      val newMapping: mutable.Map[Int, Int] = new IntToIntBucketMap()
      var current: MutableMeshSection = new MutableMeshSection()
      triangles.foreach { case (v1, v2, v3) =>
        val newVerts: Int =
          (if (newMapping.contains(v1)) 0 else 1) +
            (if (newMapping.contains(v2)) 0 else 1) +
            (if (newMapping.contains(v3)) 0 else 1)
        if (newVerts + current.vertices.size > maxVertices) { //finish old and create new
          res = res :+ current
          newMapping.clear()
          current = new MutableMeshSection()
        }
        def addIfNotContains(v: Int): Unit = {
          if (!newMapping.contains(v)) {
            val newId = newMapping.size
            newMapping += v -> newId
            current.vertices += vertices(v)
            current.uvs += uvs(v)
            current.normals += normals(v)
          }
        }
        addIfNotContains(v1)
        addIfNotContains(v2)
        addIfNotContains(v3)
        current.triangles.addOne(newMapping(v1), newMapping(v2), newMapping(v3))
      }
      return res :+ current
    }
  }

  def toMultiMaterialMesh[MATERIAL](material: MATERIAL): MultiMaterialMesh[MATERIAL] = {
    val mmm = new MultiMaterialMesh[MATERIAL]()
    mmm += (material, this)
    mmm
  }

}
