package utils.mesh

import scala.collection.mutable

class MultiMaterialMesh[MATERIAL_TYPE](
                         val sections: mutable.HashMap[MATERIAL_TYPE, MutableMeshSection]
                       )  {
  def this() {
    this(new mutable.HashMap[MATERIAL_TYPE, MutableMeshSection]())
  }

  def +=(materialAndMesh: (MATERIAL_TYPE, MeshSection)): this.type = {
    if (sections.contains(materialAndMesh._1)) {
      sections(materialAndMesh._1) ++= materialAndMesh._2
    } else {
      sections += (materialAndMesh._1 -> materialAndMesh._2)
    }
    this
  }

  def ++=(ot: MultiMaterialMesh[MATERIAL_TYPE]): this.type = {
    ot.sections.foreach(this.+=)
    this
  }



  def toSingleSection:MeshSection = {
    sections.values.reduce((a:MutableMeshSection , b:MutableMeshSection) => a ++= b)
  }


  override def toString: String = {
    s"MultiMaterialMesh(${sections.map(v=>v._1.toString + " : )" + v._2.toString).reduceOption((x, y) => x + y) })"
  }
}
