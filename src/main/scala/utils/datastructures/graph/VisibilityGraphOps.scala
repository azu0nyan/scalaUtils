package utils.datastructures.graph

import utils.math.Scalar
import utils.math.WithAlmostEquals
import utils.math.planar.{AngleOps, PointIntersection, PolygonRegion, SegmentIntersection, SegmentPlanar, TrianglePlanar, V2}

import scala.collection.mutable

object VisibilityGraphOps {

  def bodyIntersection(vis1: V2, vis2: V2, side1: V2, side2: V2): Boolean = {
    val seg1 = SegmentPlanar(vis1, vis2)
    val side = SegmentPlanar(side1, side2)
    val res = seg1.intersection(side) match {
      case Some(PointIntersection(p)) => !(p ~= vis1) && !(p ~= vis2)
      case Some(SegmentIntersection(_)) => true
      case None => false
    }
    res
  }

  /** outerFace in CW order polys shouldn't touch eachother */
  def buildVisibilityGraph(polys: Seq[PolygonRegion], additionalPoints: Seq[V2]): ArrayBufferGraph[V2, Scalar] = {
    val goodPolys = polys.filter(p => p.area > 0)
    val res = new ArrayBufferGraph[V2, Scalar]()
    val addedNodes: mutable.Set[(V2, V2)] = mutable.Set()
    for (p <- polys; v <- p.vertices) res.addNode(v)
    for (v <- additionalPoints) res.addNode(v)
    val verticesTotal: Seq[V2] = res.nodes.toSeq

    val sides = polys.flatMap(_.sides)

    def checkForIntersectionsAndAdd(from: V2, to: V2): Unit = {
      val seg = SegmentPlanar(from, to)
      val intersectionExists = sides.exists(side => side != seg && side != seg.flip && bodyIntersection(seg.start, seg.end, side.start, side.end))
      println(from, to, intersectionExists)
      if (!intersectionExists) {
        res.addEdge(from, to, to.distance(from))
        res.addEdge(to, from, to.distance(from))
      }
    }

    for (
      p <- goodPolys;
      Seq(prevFrom, from, nextFrom) <- (p.vertices :+ p.vertices(0) :+ p.vertices(1)).sliding(3);
      ot <- goodPolys;
      Seq(prevTo, to, nextTo) <- (ot.vertices :+ ot.vertices(0) :+ ot.vertices(1)).sliding(3) if to != from
    ) {
      println(from, to)
      if (to == nextFrom || to == prevFrom) {
        if (res.findEdge(from, to).isEmpty) {
          res.addEdge(from, to, to.distance(from))
          res.addEdge(to, from, to.distance(from))
        }
      } else if (res.findEdge(from, to).isEmpty) {
        //todo fix
        val s1From = prevFrom - from
        val s2From = nextFrom - from
        val toOtFrom = to - from
        val betweenWallsFrom = AngleOps.ccwAngleFromTo(s2From, s1From)
        val betweenOtherFrom = AngleOps.ccwAngleFromTo(toOtFrom, s1From)

        val s1To = prevTo - to
        val s2To = nextTo - to
        val toOtTo = from - to
        val betweenWallsTo = AngleOps.ccwAngleFromTo(s2To, s1To)
        val betweenOtherTo = AngleOps.ccwAngleFromTo(toOtTo, s1To)
        //println(from, to, betweenWallsFrom, betweenOtherFrom, betweenWallsTo, betweenOtherTo)
        if(betweenWallsFrom >= betweenOtherFrom && betweenWallsTo >= betweenOtherTo){
          checkForIntersectionsAndAdd(from, to)
        }
        //        val a1 = TrianglePlanar(prevFrom, from, to).ccw
        //        val a2 = TrianglePlanar(from, nextFrom, to).ccw
        //
        //        val a3 = TrianglePlanar(prevTo, to, from).ccw
        //        val a4 = TrianglePlanar(to, nextTo, from).ccw
        //        println(from, to, a1, a2, a3, a4)
        //if edge going outside both polys
        //        if (!a1 && !a2 && !a3 && !a4) {
        //!!! double checked
        //          checkForIntersectionsAndAdd(from, to)
        //        }
      }
    }

    for (
      p <- goodPolys;
      Seq(vfrom1, from, vfrom3) <- (p.vertices :+ p.vertices(0) :+ p.vertices(1)).sliding(3);
      to <- additionalPoints if to != from
    ) {

      //if point on polygon side
      val seg = SegmentPlanar(from, vfrom3)
      if (seg.contains(to)) {
        //add connection to both sides
        if (res.findEdge(from, to).isEmpty) {
          res.addEdge(from, to, to.distance(from))
          res.addEdge(to, from, to.distance(from))
        }
        if (res.findEdge(vfrom3, to).isEmpty) {
          res.addEdge(vfrom3, to, to.distance(vfrom3))
          res.addEdge(to, vfrom3, to.distance(vfrom3))
        }
      } else if (res.findEdge(from, to).isEmpty) {
        val a1 = TrianglePlanar(vfrom1, from, to).ccw
        val a2 = TrianglePlanar(from, vfrom3, to).ccw

        //if edge going outside
        if (!a1 && !a2) {
          //!!! double checked
          checkForIntersectionsAndAdd(from, to)
        }
      }
    }

    for (
      from <- additionalPoints;
      to <- additionalPoints if to != from
    ) {

      if (res.findEdge(from, to).isEmpty) {
        checkForIntersectionsAndAdd(from, to)
      }
    }
    res

  }

}
