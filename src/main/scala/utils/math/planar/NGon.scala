package utils.math.planar

import utils.math._
case class NGon(center:V2, verts:Int, radius: Scalar, firstAngleOffset: Scalar = 0d) {

  def innerAngle: Scalar = Math.PI * 2 / verts

  def angleFromId(id: Int): Scalar = innerAngle * id + firstAngleOffset

  def vertices: Seq[V2] = (0 until verts).map(i => center + V2(radius, 0).rotate(angleFromId(i)))

  def toPoly:PolygonRegion = PolygonRegion(vertices)
  
}

