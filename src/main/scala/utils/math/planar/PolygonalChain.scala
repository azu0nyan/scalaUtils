package utils.math.planar

import utils.datastructures.{CircullarOps}
import utils.math._
import utils.math.planar._
import utils.math.planar.PolygonalChain.PolygonalChainOps
import utils.math.planar.PointList.{PointList, PointListOps}


object PolygonalChain {
  implicit def toPolygon(c: PolygonalChain): PolygonRegion = PolygonRegion(c.vertices)

  /*Closed or unclosed polygonal chain*/
  trait PolygonalChainOps[MYTYPE <: PolygonalChainOps[MYTYPE]] extends PointListOps[MYTYPE] {

    def closed: Boolean

    def combineWith(ot: PolygonalChainOps[_]): PolygonalChainOps[_] = new PolygonalChain(
      if (vertices.nonEmpty && ot.vertices.nonEmpty &&   (vertices.last ~= ot.vertices.head)) {
        vertices ++ ot.vertices.tail
      } else {
        vertices ++ ot.vertices
      }, closed && ot.closed
    )

    def sides: Seq[SegmentPlanar] = if (closed)
      CircullarOps.toCyclicPairs(vertices).map(p => SegmentPlanar(p._1, p._2)).toSeq
    else {
      for (i <- 1 until vertices.size) yield SegmentPlanar(vertex(i - 1), vertex(i))
    }

    def sideTriangles: Seq[SideTriangle] =
      for (i <- 0 until (if (closed) vertices.size else vertices.size - 2))
        yield new SideTriangle(
          vertex(i), vertex(i + 1), vertex(i + 2),
          (i, (i + 1) % verticesCount, (i + 2) % verticesCount)
        )

    def distanceToFromSides(point: V2): Scalar = sides.map(s => s.distanceTo(point)).min

    def sideAngles: Seq[AngleCCWPlanar] = CircullarOps.toCyclicPairs(sides).map(s12 => AngleCCWPlanar(s12._1.v1, s12._1.v2, s12._2.v2)).toSeq

    def length:Scalar = sides.map(_.length).sum

  }

  class SideTriangle(v1: V2, v2: V2, v3: V2, val indices: (Int, Int, Int)) extends TrianglePlanar(v1, v2, v3)

}

case class PolygonalChain(override val vertices: Seq[V2], closed: Boolean) extends PolygonalChainOps[PolygonalChain] {
  override def replacePoints(newVertices: Seq[V2]): PolygonalChain = PolygonalChain(newVertices, closed)
}
