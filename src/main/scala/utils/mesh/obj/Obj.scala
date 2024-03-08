package utils.mesh.obj

import utils.math.planar.V2
import utils.math.space.V3
import utils.mesh.obj.Obj.NamedObjSection

import scala.collection.mutable

class Obj(
           val vertices: mutable.ArrayBuffer[V3] = mutable.ArrayBuffer(),
           val uvs: mutable.Buffer[V2] = mutable.ArrayBuffer(),
           val normals: mutable.ArrayBuffer[V3] = mutable.ArrayBuffer(),
           val sections: mutable.ArrayBuffer[NamedObjSection] = mutable.ArrayBuffer(),
         )


object Obj {

  class NamedObjSection(
                         val name: String,
                         val obj: ObjSection,
                       )
  case class Vertex(
                     vId: Int,
                     uvId: Int,
                     nId: Int,
                   )

  class Face(val vs: mutable.ArrayBuffer[Vertex] = mutable.ArrayBuffer()) extends AnyVal

  class ObjSection(
                    val faces: mutable.ArrayBuffer[Face] = mutable.ArrayBuffer(),
                  ) extends AnyVal


}
