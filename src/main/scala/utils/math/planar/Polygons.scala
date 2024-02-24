package utils.math.planar

import utils.datastructures.CircullarOps
import utils.datastructures.spatial.AARectangle
import utils.math._
object Polygons {

  implicit  def toSeq(p:Polygons): Seq[PolygonRegion] = p.polys

  implicit def fromSeq(s:Seq[PolygonRegion]): Polygons = new Polygons(s)

  def rectGrid(size:(Int,Int), offset: V2 = V2.ZERO, cell: AARectangle): Polygons = {
    grid(size, offset, cell.wh, cell.toPolygon)
  }

  def grid(size: (Int, Int), offset: V2 = V2.ZERO, spacing: V2, cell: PolygonRegion): Polygons =
    for (
      i <- 0 until size._1;
      j <- 0 until size._2
    ) yield cell + offset + spacing * V2(i, j)

  def pie(verts:Int, radius: Scalar):Seq[PolygonRegion] = CircullarOps.toCyclicPairs(NGon(verts, V2.ZERO, radius, 0d).vertices).map(v12 => new PolygonRegion(Seq(v12._1, v12._2, V2(0,0)) )).toSeq
}

case class Polygons(polys:Seq[PolygonRegion]) {

  def area:Scalar = polys.map(p => p.area).sum


}
