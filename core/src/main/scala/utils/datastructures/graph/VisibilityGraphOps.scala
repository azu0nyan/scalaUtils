package utils.datastructures.graph

import utils.math.{Scalar, WithAlmostEquals}
import utils.math.planar.*

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
//      println(from, to, intersectionExists)
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
//      println(from, to)
      if (to == nextFrom || to == prevFrom) {
        if (res.findEdge(from, to).isEmpty) {
          res.addEdge(from, to, to.distance(from))
          res.addEdge(to, from, to.distance(from))
        }
      } else if (res.findEdge(from, to).isEmpty) {
        val fromGood = {
          val fp = to - from
          val a = prevFrom - from
          val b = nextFrom - from
          if(TrianglePlanar(prevFrom, from, nextFrom).ccw) a ** fp <= 0 || b ** fp <= 0
          else  a ** fp >= 0 && b ** fp >= 0
        }
        val toGood = {
          val fp = from - to
          val a = prevTo - to
          val b = nextTo - to
          if(TrianglePlanar(prevTo, to, nextTo).ccw) a ** fp <= 0 || b ** fp <= 0
          else  a ** fp >= 0 && b ** fp >= 0
        }
        if(fromGood && toGood)
          checkForIntersectionsAndAdd(from, to)
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
