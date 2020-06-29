package utils.mesh.factory

import utils.mesh.MutableMeshSection

object Quad {

  def apply(q:utils.math.space.Quad): MutableMeshSection = {
    val t1 = utils.math.space.Triangle(q.bl, q.tl, q.tr)
    val t2 = utils.math.space.Triangle(q.bl, q.tr, q.br)
    Triangle(t1) ++= Triangle(t2)
  }

}
