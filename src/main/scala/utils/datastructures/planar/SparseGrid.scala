package utils.datastructures.planar

import utils.datastructures.IntV2
import utils.datastructures.spatial.AARectangle
import utils.math.planar.V2

import scala.collection.mutable

class SparseGrid(
                  cellsSize: V2
                ) {
  val cellsMap: mutable.Map[IntV2, Seq[V2]] = mutable.Map()

  def cell(v: V2): IntV2 = {
    (v / cellsSize).toIntV2
  }

  def add(v: V2): Unit = {
    val c:IntV2 = cell(v)
    cellsMap.updateWith(c) {
      case Some(vertices) => Some(v +: vertices)
      case None => Some(Seq(v))
    }
  }

  def remove(v: V2): Unit = {
    val c:IntV2 = cell(v)
    cellsMap.updateWith(c) { x =>
      val res = x match {
        case Some(vertices) => vertices.filter(_ != v)
        case None => Seq()
      }
      Option.when(res.nonEmpty)(res)
    }
  }

  def pointsInArea(a: AARectangle): Seq[V2] = {
    val min = cell(a.min)
    val max = cell(a.max)
    val res:mutable.Buffer[V2] = mutable.Buffer()
    for(i <- min.x to max.x; j <- min.y to max.y){
      cellsMap.get(IntV2(i, j)).foreach(c => res ++= c.filter(a.contains))
    }
    res.toSeq
  }
}


