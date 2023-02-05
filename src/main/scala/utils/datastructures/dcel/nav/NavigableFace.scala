package utils.datastructures.dcel.nav

import utils.datastructures.dcel.DCEL.DCELData
import utils.datastructures.dcel.HierarchicalDCEL._
import utils.datastructures.dcel.nav.DCELPath.{BorderNode, DCELPath, NavMeshPosition, PathNode}
import utils.datastructures.dcel.nav.NavigableDCEL._
import utils.datastructures.dcel.nav.NavigableFace.FaceNavData
import utils.datastructures.graph.{ArrayBufferGraph, GraphOps}
import utils.math.planar.V2
import utils.math._

object NavigableFace {

  case class NavigationParams(
                               minBorderEdgeLength: Scalar = 0d
                             )

  class FaceNavData(face: NavigableFace, params: NavigationParams) {
    /** Border nodes through you can leave area (and it's childs) */
    val borderWaysOut: Seq[BorderNode] = face.hierarchicalFace.fullBorder.flatMap(_.data.ownData.borderNodes)

    val borderOwnAreaWaysOut: Seq[BorderNode] = borderWaysOut.filter(_.existsNonChildSegment(params.minBorderEdgeLength))

    /** inner dcel edges  through you can enter own area (from child to ownArea) */
    val innerDcelToOwnAreaWays: Seq[BorderNode] =
      face.hierarchicalFace.innerDCEL.halfEdges.toSeq
        .filter(_.data.isOuterEdge)
        .flatMap(_.data.ownData.borderNodes)
        .filter(bn => bn.oppositeNodes(params.minBorderEdgeLength).exists(on =>
          face.hierarchicalFace.innerDCEL.innerFaces.contains(on.border.area.hierarchicalFace.face) //on.border.area.hierarchicalFace != area.hierarchicalFace not needed
        ))

    /** Ways to leave own area */
    val ownAreaExits: Seq[BorderNode] = borderOwnAreaWaysOut ++ innerDcelToOwnAreaWays

    val navMesh: ArrayBufferGraph[PathNode, Scalar] = NavMesh.buildGraph(face)

    def findPathOnNavMesh(from: NavMeshPosition, to: NavMeshPosition): Option[DCELPath] ={
      GraphOps.shortestPath()
    }

  }
}

trait NavigableFace {
  var navData: FaceNavData = _
  var hierarchicalFace: HierarchicalFace[NavigableDCELOwnData] = _
  def setFace[D <: NavigableDCELOwnData](face: HierarchicalFace[D]): Unit = hierarchicalFace = face.asInstanceOf[HierarchicalFace[NavigableDCELOwnData]]
}
