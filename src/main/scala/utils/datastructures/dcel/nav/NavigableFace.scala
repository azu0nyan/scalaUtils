package utils.datastructures.dcel.nav

import utils.datastructures.dcel.DCEL.DCELData
import utils.datastructures.dcel.HierarchicalDCEL._
import utils.datastructures.dcel.nav.DCELPath._
import utils.datastructures.dcel.nav.NavigableDCEL._
import utils.datastructures.dcel.nav.NavigableFace.FaceNavData
import utils.datastructures.graph.Graph.Graph
import utils.datastructures.graph.{ArrayBufferGraph, GraphOps}
import utils.math.planar.V2
import utils.math._

object NavigableFace {

  case class NavigationParams(
                               minBorderEdgeLength: Scalar = 0d,
                               maxPathLength: Scalar = 0d
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
        .filter(bn => bn.twinNodes(params.minBorderEdgeLength).exists(on =>
          face.hierarchicalFace.innerDCEL.innerFaces.contains(on.border.area.hierarchicalFace.face) //on.border.area.hierarchicalFace != area.hierarchicalFace not needed
        ))

    /** Ways to leave own area */
    val ownAreaExits: Seq[BorderNode] = borderOwnAreaWaysOut ++ innerDcelToOwnAreaWays

    val navMesh: ArrayBufferGraph[PathNode, Scalar] = NavMesh.buildGraph(face)

    def findPathOnNavMesh(from: NavMeshPosition, to: NavMeshPosition): Option[DCELPath] = {
      if (from.navMeshFace == to.navMeshFace) { //since polygons are convex
        Some(DCELPath(Seq(GoBetweenPoints(from, to))))
      } else {
        val starts = from.distancesToBorders
        val ends = to.distancesToBorders
        val path = GraphOps.shortestPath(navMesh, starts, ends, (e: Scalar) => e, (n: PathNode) => n.point.distance(to.point), Some(params.maxPathLength))
        path match {
          case Some(foundPath) =>
            if (foundPath.nodes.length == 1) {
              Some(DCELPath(Seq(GoBetweenPoints(from, to))))
            } else {
              val pathEdges = foundPath.nodes.sliding(2).map {
                case Seq(f: NavMeshPosition, t: NavMeshPosition) => GoBetweenPoints(f, t)

                case Seq(f: BorderNode, t: BorderNode) if f.border.edgeNodeTwin == t.border => GoTroughBorder(f, t) //hanging edge
                case Seq(f: BorderNode, t: BorderNode) if f.border.edgeNodeTwin != t.border => GoTroughFaceOnNavMesh(f, t) //todo ??? shouldn't be possible or?

                case Seq(f: NavMeshEdge, t: NavMeshEdge) => GoBetweenNavEdges(f, t)

                case Seq(f: NavMeshPosition, t: NavMeshEdge) => GoFromPointToNavEdge(f, t)
                case Seq(f: NavMeshEdge, t: NavMeshPosition) => GoFromNavEdgeToPoint(f, t)

                case Seq(f: NavMeshPosition, t: BorderNode) => GoFromPointToBorder(f, t)
                case Seq(f: BorderNode, t: NavMeshPosition) => GoFromBorderToPoint(f, t)

                case Seq(f: BorderNode, t: NavMeshEdge) => GoFromBorderToNavEdge(f, t)
                case Seq(f: NavMeshEdge, t: BorderNode) => GoFromNavEdgeToBorder(f, t)
              }
              Some(DCELPath(pathEdges.toSeq))
            }
          case None => None
        }
      }
    }

    /** Graph for finding patches on face level, contains inner DCEL edges and border edges
      * should be calculated and updated recursively for all childs first
      * */
    val edgeGraph: Graph[BorderNode, PathEdge[_, _]] = {
      val res = new ArrayBufferGraph[BorderNode, PathEdge[_, _]]()

      val edges = face.hierarchicalFace.fullBorder ++ face.hierarchicalFace.innerDCEL.halfEdges
      for (e <- edges;
           bn <- e.data.ownData.borderNodes) {
        res.addNode(bn)
      }

      //edges between borders
      for (e <- edges;
           bn <- e.data.ownData.borderNodes;
           bntwin <- bn.twinNodes(params.minBorderEdgeLength)) {
        res.addOrUpdateEdge(bn, bntwin, GoTroughBorder(bn, bntwin))
      }

      //add nodes from this dcel to innerDcel and vise versa
      for (e <- face.hierarchicalFace.fullBorder;
           bn <- e.data.ownData.borderNodes;
            child <- bn.directChildTwinNodes(params.minBorderEdgeLength)
           ) {
        res.addOrUpdateEdge(bn, child, GoToChild(bn, child))
        res.addOrUpdateEdge(child, bn, GoToParent(child, bn))
      }

      //nodes for moving trough ownArea's nav mesh
      for(Seq(f, t) <- ownAreaExits.combinations(2);
          p <- findPathOnNavMesh()
          ){

      }



      res
    }


  }
}

trait NavigableFace {
  var navData: FaceNavData = _
  var hierarchicalFace: HierarchicalFace[NavigableDCELOwnData] = _
  def setFace[D <: NavigableDCELOwnData](face: HierarchicalFace[D]): Unit = hierarchicalFace = face.asInstanceOf[HierarchicalFace[NavigableDCELOwnData]]
}
