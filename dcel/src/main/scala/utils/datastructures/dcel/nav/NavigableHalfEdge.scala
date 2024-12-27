package utils.datastructures.dcel.nav

import utils.datastructures.dcel.HierarchicalDCEL._
import utils.datastructures.dcel.nav.DCELPath.BorderNode
import utils.datastructures.dcel.nav.NavigableDCEL._
import utils.math.planar.V2
import utils.math._
import utils.math.misc.IntervalOps

trait NavigableHalfEdge {
  var hierarchicalEdge: HierarchicalEdge[NavigableDCELOwnData] = _
  def setHalfEdge[D <: NavigableDCELOwnData](he: HierarchicalEdge[D]): Unit = hierarchicalEdge = he.asInstanceOf[HierarchicalEdge[NavigableDCELOwnData]]

  def edgeNodeTwin: NavigableHalfEdge = hierarchicalEdge.edge.twin.data.ownData

  def area: NavigableFace = hierarchicalEdge.face.data.ownData

  //    def isFake: Boolean = area.hierarchicalFace.parent.contains(hierarchicalEdge.face.data)


  /** true if nav agent can pass edge */
  def passable: Boolean


  def borderNodes: Seq[BorderNode] =
    if (passable) {
      val mySeg = hierarchicalEdge.asSegment
      val blocked = (hierarchicalEdge.parents.filter(!_.ownData.passable) ++
        hierarchicalEdge.allLevelChilds.filter(!_.ownData.passable)).map { he =>
        val heSeg = he.asSegment
        val p1 = mySeg.getFractionAt(heSeg.v1)
        val p2 = mySeg.getFractionAt(heSeg.v2)
        //if(p1 < p2) (p1, p2) else (p2, p1) //check not needed as parents and childs goes at the same direction
        (p1, p2)
      }
      //todo fake edges impassable or smth
      // todo split where blocking wall going ortogonal to edge
       IntervalOps.cutFrom((0d, 1d), blocked).map { case (l, r) => BorderNode(this, l, r) }
    } else Seq()


  @deprecated
  def linkNodesTo(ot: NavigableHalfEdge, minLengthToLink: Scalar): Map[BorderNode, Seq[BorderNode]] = {
    val my = borderNodes
    val other = ot.borderNodes
    my.map(myNode => (myNode, other.filter { otherNode =>
      val int = myNode.commonNonemptyInterval(otherNode)
      int.nonEmpty &&
        hierarchicalEdge.asSegment.getFractionAt(int.get._1).distance(hierarchicalEdge.asSegment.getFractionAt(int.get._2)) >= minLengthToLink
    })).toMap
  }


}
