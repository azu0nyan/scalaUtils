package utils.mesh

import utils.math.planar.V2
import utils.math.space.V3

import scala.collection.mutable

class MutableMultiMaterialMesh[M](
                                   val sections: mutable.HashMap[M, MutableMeshSection]
                                 ) {
  def this() = {
    this(new mutable.HashMap[M, MutableMeshSection]())
  }

  def +=(materialAndMesh: (M, MeshSection)): this.type = {
    if (sections.contains(materialAndMesh._1)) {
      sections(materialAndMesh._1) ++= materialAndMesh._2
    } else {
      sections += (materialAndMesh._1 -> new MutableMeshSection(materialAndMesh._2))
    }
    this
  }

  def ++=(ot: MutableMultiMaterialMesh[M]): this.type = {
    ot.sections.foreach(this.+=)
    this
  }

  def addAndMap(ot: MutableMultiMaterialMesh[M],
                posMap: V3 => V3 = v3 => v3,
                uvMap: V2 => V2 = v2 => v2,
                normalMap: V3 => V3 = v3 => v3,
               ): this.type = {
    ot.sections.foreach(addAndMapSection(_, posMap, uvMap, normalMap))
    this
  }

  def addAndMapSection(materialAndMesh: (M, MeshSection),
                posMap: V3 => V3 = v3 => v3,
                uvMap: V2 => V2 = v2 => v2,
                normalMap: V3 => V3 = v3 => v3,
               ) : this.type  = {
    if (sections.contains(materialAndMesh._1)) {
      sections(materialAndMesh._1).addAndMap(materialAndMesh._2, posMap, uvMap, normalMap)
    } else {
      sections += (materialAndMesh._1 -> new MutableMeshSection().addAndMap(materialAndMesh._2, posMap, uvMap, normalMap))
    }
    this
  }

  def toSingleSection: MeshSection = {
    sections.values.reduce((a: MutableMeshSection, b: MutableMeshSection) => a ++= b)
  }


  override def toString: String = {
    s"MultiMaterialMesh(${sections.map(v => v._1.toString + " : )" + v._2.toString).reduceOption((x, y) => x + y)})"
  }
}
