package utils.mesh.obj

import utils.mesh.MutableMultiMaterialMesh

object ObjLoader {
  def loadObjFromResource(path: String, crushOnError: Boolean = false, debugOutput: Boolean = false): Obj = {
    val strs = io.Source.fromResource(path).getLines()
    ObjReader.readFrom(strs)(crushOnError, debugOutput)
  }

  def loadMeshFromResource[M](path: String, materialMapper: String => M, crushOnError: Boolean = false, debugOutput: Boolean = false): MutableMultiMaterialMesh[M] = {
    val obj = loadObjFromResource(path, crushOnError, debugOutput)
    val mmmm = ObjConverter.objToMultiMaterialMesh(obj, materialMapper)(crushOnError)
    mmmm
  }
}
