package utils.mesh.factory

import utils.math.space.OBox
import utils.mesh.MutableMeshSection

object Box  {
  def apply(box:OBox, invertNormals:Boolean = false): MutableMeshSection = {
    box.sides.map(Quad.apply).foldLeft(new MutableMeshSection())(_ ++= _)
  }

}
