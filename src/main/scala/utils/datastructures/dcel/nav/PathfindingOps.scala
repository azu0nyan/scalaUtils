package utils.datastructures.dcel.nav

import utils.datastructures.dcel.nav.DCELPath._
import utils.datastructures.dcel.nav.NavigableDCEL.NavigableFace
import utils.math.planar.V2

class PathfindingOps {
/*
  def findInside(area: NavigableFace, from: V2, to: V2): Seq[V2] = {
    val path = area.navData.findPathInsideLowest(from, to, Int.MaxValue)
    path.map(expandPath).getOrElse(Seq(from, to))
  }

  def expandPath(p: DCELPath): Seq[V2] = {
    if (p.waypoints.size > 1)
      p.waypoints.sliding(2).toSeq flatMap { case Seq(a, b) => expandPathNeighbourNodes(a, b) }
    else p.waypoints map (_.point)
  }


  def expandPathNeighbourNodes(fromNode: PathNode, toNode: PathNode): Seq[V2] =
    (fromNode, toNode) match {
      case (p1@PointNode(v1, _), p2@PointNode(v2, _)) =>
        Seq(v1, v2)
      case (PointNode(v1, _), bn@FreeBorderNode(border)) =>
        bn.border.area.navData.findPathOnVisibilityGraph(v1, bn.point)
          .map(_.waypoints.map(_.point)).getOrElse(Seq(v1, bn.point))
      case (PointNode(v1, _), pn@PortalNode(from, portal)) =>
        pn.from.area.navData.findPathOnVisibilityGraph(v1, pn.point)
          .map(_.waypoints.map(_.point)).getOrElse(Seq(v1, pn.point))
      case (bn@FreeBorderNode(border), PointNode(v1, _)) =>
        bn.border.area.navData.findPathOnVisibilityGraph(bn.point, v1)
          .map(_.waypoints.map(_.point)).getOrElse(Seq(bn.point, v1))
      case (pn@PortalNode(_, _), PointNode(v1, _)) =>
        pn.from.area.navData.findPathOnVisibilityGraph(pn.point, v1)
          .map(_.waypoints.map(_.point)).getOrElse(Seq(pn.point, v1))
      case (b1, b2) if b1.area == b2.area =>
        findInside(b1.area, b1.point, b2.point)
      //same node on different borders
      case _ => Seq()


    }
*/
}
