package utils.mesh.obj

import utils.datastructures.IndexedTriangle
import utils.mesh.obj.Obj.ObjSection
import utils.mesh.{MeshSection, MutableMeshSection, MutableMultiMaterialMesh}


object ObjConverter {
  def objToMultiMaterialMesh[M](obj: Obj, materialMapper: String => M)(crushOnError: Boolean = true): MutableMultiMaterialMesh[M] = {
    val res = new MutableMultiMaterialMesh[M]()
    for (s <- obj.sections)
      res += materialMapper(s.name) -> objSectionToMeshSection(obj, s.obj)(crushOnError)
    res
  }

  def objSectionToMeshSection(obj: Obj, objSection: ObjSection)(crushOnError: Boolean = true): MutableMeshSection = {
    val res = new MutableMeshSection()
    for ((face, id) <- objSection.faces.zipWithIndex)
      if (face.vs.size == 3) {
        res.triangles += ((id * 3, id * 3 + 1, id * 3 + 2))
        for (i <- 0 until 3) yield {
          res.normals += obj.normals(face.vs(i).nId)
          res.uvs += obj.uvs(face.vs(i).uvId)
          res.vertices += obj.vertices(face.vs(i).vId)
        }
      } else if (crushOnError)
        throw new NonTriangularFaceException
    res
  }

  class NonTriangularFaceException extends Exception
}