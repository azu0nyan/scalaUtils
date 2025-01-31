package utils.math.planar

import utils.math.*
case class NGon(verts:Int, center:V2 = V2.ZERO,  radius: Scalar = 1.0, firstAngleOffset: Scalar = 0d) {

  def innerAngle: Scalar = Math.PI * 2 / verts

  def angleFromId(id: Int): Scalar = innerAngle * id + firstAngleOffset

  def vertices: Seq[V2] = (0 until verts).map(i => center + V2(radius, 0).rotate(angleFromId(i)))

  def toPoly: PolygonRegion = PolygonRegion(vertices)
  
}

