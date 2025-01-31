package utils.math.planar.algo

import utils.math.planar.{PolygonRegion, V2}

import java.util.Comparator
import scala.collection.mutable.ArrayBuffer

object ConvexHull {

  private def orientation(p: V2, q: V2, r: V2): Int = {
    val det = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
    if (det == 0) 0
    else if (det > 0) 1
    else 2
  }

  /** Graham scan O(n*log(n))*/
  def convexHull(points:Iterable[V2]):Option[PolygonRegion] = {
    convexHullCCWImpl(points.toSet).map(ccwVs => PolygonRegion(ccwVs.toSeq.reverse))
  }

  /** Graham scan O(n*log(n)) returns CCW */
  def convexHullCCWImpl(points: Set[V2]): Option[collection.Seq[V2]] = {
    if (points.size < 3) return None
    val botLeft: V2 = points.min(Ordering.by((v:V2) => (v.y, v.x)))

    val comparator: Comparator[V2] = (p1: V2, p2: V2) => {
      val o = orientation(botLeft, p1, p2)
      if (o == 0) {
        if (botLeft.distance(p2) >= botLeft.distance(p1)) -1 else 1
      } else {
        if (o == 2) -1 else 1
      }
    }
    import scala.math.Ordering.comparatorToOrdering
    val sorted = points.filter(_ != botLeft).toSeq.sorted(comparatorToOrdering(comparator))

    val pointsFiltered:ArrayBuffer[V2] = new ArrayBuffer[V2]()
    pointsFiltered += sorted(0)
    var last = pointsFiltered(0)
    for(i <- 1 until sorted.size){
      val cur = sorted(i)
      if(orientation(botLeft, last, cur) == 0){
        pointsFiltered(pointsFiltered.size - 1) = cur
      } else {
        pointsFiltered += cur
      }
      last = cur
    }
     if(pointsFiltered.size < 2 ){
       return None
     }
    val stack = new ArrayBuffer[V2]()
    stack += botLeft
    stack += pointsFiltered(0)
    stack += pointsFiltered(1)

    for(i <- 2 until pointsFiltered.size){
      while (orientation(stack(stack.size - 2), stack(stack.size - 1), pointsFiltered(i))!= 2){
        stack.remove(stack.size - 1)
      }
      stack += pointsFiltered(i)
    }
    return Some(stack)

  }

}
