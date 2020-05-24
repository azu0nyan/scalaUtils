package utils.math.planar

import utils.math._
class NGon(verts:Int, radius: Scalar) extends PolygonRegion(
  {
    val innerAngle: Float = (Math.PI * 2 / verts).toFloat
    (0 until verts).map(i => V2(radius, 0).rotate(-innerAngle * i))
  }
)

