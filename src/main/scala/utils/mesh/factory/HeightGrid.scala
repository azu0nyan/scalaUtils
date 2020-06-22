package utils.mesh.factory

import utils.datastructures.{IndexedTriangle, IntV2}
import utils.heightmap.HeightGrid
import utils.math.space.{Plane, Triangle, V3}
import utils.mesh.MutableMeshSection

import scala.collection.mutable.ArrayBuffer

object HeightGrid {

  def apply(heightGrid: HeightGrid,
            origin:V3,
            x:V3,
            y:V3,
            up: V3 = V3.upAxis): MutableMeshSection = {

    val vertices = heightGrid.indices.map( i => origin + x * i.x + y * i.y + up * heightGrid.heightAt(i)).iterator.to(ArrayBuffer)

    val uvs = heightGrid.indices.map(_.toV2).to(ArrayBuffer)

    def triangle1(ij: IntV2): IndexedTriangle = IndexedTriangle((
      heightGrid.toFlatHeightIndex(IntV2(ij.i, ij.j)),
      heightGrid.toFlatHeightIndex(IntV2(ij.i + 1, ij.j + 1)),
      heightGrid.toFlatHeightIndex(IntV2(ij.i, ij.j + 1))), vertices)

    def triangle2(ij: IntV2): IndexedTriangle = IndexedTriangle((
      heightGrid.toFlatHeightIndex(IntV2(ij.i, ij.j)),
      heightGrid.toFlatHeightIndex(IntV2(ij.i + 1, ij.j)),
      heightGrid.toFlatHeightIndex(IntV2(ij.i + 1, ij.j + 1))), vertices)

    val normals = heightGrid.indices.map(v =>
      Seq(
        triangle1(v + (-1, -1)),
        triangle2(v + (-1, -1)),
        triangle1(v),
        triangle2(v),
        triangle2(v + (0, -1)),
        triangle1(v + (-1, 0)),
      ).map(t => t.flipToMatchNormal(up).triangle)
    ).map(s => Triangle.averageNormal(s)).to(ArrayBuffer)

    val triangles = heightGrid.cellIndices.flatMap(
      i => Seq(triangle1(i).flipToMatchNormal(up).indices, triangle2(i).flipToMatchNormal(up).indices)).to(ArrayBuffer)

    new MutableMeshSection(vertices, triangles, normals, uvs)

  }

}
