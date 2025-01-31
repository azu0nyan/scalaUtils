package utils.mesh.factory

import utils.math.*
import utils.math.planar.V2
import utils.math.space.V3
import utils.mesh.MutableMeshSection

import scala.collection.mutable.ArrayBuffer


object Sphere {
  def apply(s: utils.math.space.Sphere, meridians: Int = 8, parallels: Int = 8): MutableMeshSection = {
    val vertices = new ArrayBuffer[V3]()
    val uvs = new ArrayBuffer[V2]()
    val normals = new ArrayBuffer[V3]()
    val triangles = new ArrayBuffer[(Int, Int, Int)]()
    for (
      i <- 0 to meridians;
      j <- 0 until parallels
    ) {
      val inc: Scalar = i * PI / meridians
      val azi: Scalar = j * 2f * PI / parallels
      vertices.addOne(s.fromSphericalCoordinates(inc, azi))
      normals.addOne(fromSphericalCoordinates(1, inc, azi))
      uvs.addOne(V2(i / meridians, j / parallels))
      if (i < meridians) {
        triangles.addOne(tr1(i, j, meridians, parallels))
        triangles.addOne(tr2(i, j, meridians, parallels))
      }
    }
    new MutableMeshSection(vertices, triangles, normals, uvs)
  }

  def tr1(i: Int, j: Int, meredians: Int, parallels: Int): (Int, Int, Int) = (
    toVertexId(i, j, meredians, parallels),
    toVertexId(i + 1, j + 1, meredians, parallels),
    toVertexId(i, j + 1, meredians, parallels),
  )

  def tr2(i: Int, j: Int, meredians: Int, parallels: Int): (Int, Int, Int) = (
    toVertexId(i, j, meredians, parallels),
    toVertexId(i + 1, j, meredians, parallels),
    toVertexId(i + 1, j + 1, meredians, parallels),
  )
  def toVertexId(i: Int, j: Int, meredians: Int, parallels: Int): Int = (i % (meredians + 1)) * parallels + (j % parallels)

}
