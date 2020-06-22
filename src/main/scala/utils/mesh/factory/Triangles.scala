package utils.mesh.factory

import utils.datastructures.spatial.TriangleSoup
import utils.math.space.TriangleList
import utils.mesh.MutableMeshSection

object Triangles {
    def apply(t: TriangleSoup): MutableMeshSection = {
      t.triangles.map(Triangle(_)).foldLeft(new MutableMeshSection())((x, y) => x.addOther(y) )
    }

    def apply(t:TriangleList):MutableMeshSection = {
      t.triangles.map(Triangle(_)).foldLeft(new MutableMeshSection())((x, y) => x.addOther(y) )
    }

}
