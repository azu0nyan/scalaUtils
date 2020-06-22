package utils.mesh

import utils.datastructures.IndexedTriangle
import utils.math.planar.V2
import utils.math.space.{Triangle, V3}

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


  def toMultiMaterialMesh[MATERIAL](material: MATERIAL): MultiMaterialMesh[MATERIAL] = {
    val mmm = new MultiMaterialMesh[MATERIAL]()
    mmm += (material, this)
    mmm
  }

}
