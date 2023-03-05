package utils.datastructures.dcel.nav

import utils.datastructures.dcel.DCEL.DCELData
import utils.datastructures.dcel.HierarchicalDCEL._
import utils.datastructures.dcel.nav.DCELPath._
import utils.datastructures.dcel.nav.NavMesh.{NavMesh, NavMeshGraph}
import utils.datastructures.dcel.nav.NavigableDCEL._
import utils.datastructures.dcel.nav.NavigableFace.FaceNavData
import utils.datastructures.graph.Graph.Graph
import utils.datastructures.graph.{ArrayBufferGraph, Graph, GraphOps}
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

    val (navMesh, navMeshGraph): (NavMesh, NavMeshGraph) = NavMesh.buildNavMeshAndGraph(face)

    def nodesToEdge(nodes: (PathNode, PathNode)): PathEdge[PathNode, PathNode] = nodes match {
      case (f: NavMeshPosition, t: NavMeshPosition) => GoBetweenPoints(f, t)

      case (f: BorderNode, t: BorderNode) if f.border.edgeNodeTwin == t.border => GoTroughBorder(f, t) //hanging edge
      case (f: BorderNode, t: BorderNode) if f.border.edgeNodeTwin != t.border => GoTroughFaceOnNavMesh(f, t, 0d, None) //todo ??? shouldn't be possible or?

      case (f: NavMeshEdge, t: NavMeshEdge) => GoBetweenNavEdges(f, t)

      case (f: NavMeshPosition, t: NavMeshEdge) => GoFromPointToNavEdge(f, t)
      case (f: NavMeshEdge, t: NavMeshPosition) => GoFromNavEdgeToPoint(f, t)

      case (f: NavMeshPosition, t: BorderNode) => GoFromPointToBorder(f, t)
      case (f: BorderNode, t: NavMeshPosition) => GoFromBorderToPoint(f, t)

      case (f: BorderNode, t: NavMeshEdge) => GoFromBorderToNavEdge(f, t)
      case (f: NavMeshEdge, t: BorderNode) => GoFromNavEdgeToBorder(f, t)
    }
    private def toPath(foundPath: Graph.Path[PathNode, Scalar]): Option[DCELPath] = {
      val pathEdges = foundPath.nodes.sliding(2).map { case Seq(a, b) => (a, b) }.map(nodesToEdge)
      Some(DCELPath(pathEdges.toSeq))
    }
    def findPathOnNavMeshBetweenBorders(from: BorderNode, to: BorderNode): Option[DCELPath] = {
      if (from == to) {
        ???
      } else {
        (navMesh.boundMeshEdge(from), navMesh.boundMeshEdge(to)) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(fEdge), Some(tEdge)) =>
            if (fEdge.leftFace == tEdge.leftFace) {
              ???
            } else {
              val starts = Seq((from, 0d))
              val ends = Seq((to, 0d))
              val path = GraphOps.shortestPath[PathNode, Scalar](navMeshGraph, starts, ends, (e: Scalar) => e, (n: PathNode) => n.point.distance(to.point), Some(params.maxPathLength))
              path match {
                case Some(foundPath) =>
                  if (foundPath.nodes.length == 1) {
                    throw new Exception(s"Found path 1 node length $foundPath $from $to")
                  } else {
                    toPath(foundPath)
                  }
                case None => None
              }
            }
        }
      }
    }


    def findPathOnNavMeshBetweenPositionAndBorder(from: NavMeshPosition, to: BorderNode): Option[DCELPath] = {
      if (navMesh.boundMeshEdge(to).map(_._leftFace).contains(from.navMeshFace)) {
        Some(DCELPath(Seq(GoFromPointToBorder(from, to))))
      } else {
        val starts = from.distancesToBorders
        val ends = Seq((to, 0d))
        val path = GraphOps.shortestPath[PathNode, Scalar](navMeshGraph, starts, ends, (e: Scalar) => e, (n: PathNode) => n.point.distance(to.point), Some(params.maxPathLength))
        path match {
          case Some(foundPath) =>
            if (foundPath.nodes.length == 1) {
              throw new Exception(s"Found path 1 node length $foundPath $from $to")
            } else {
              toPath(foundPath)
            }
          case None => None
        }
      }
    }
    def findPathOnNavMeshBetweenPositions(from: NavMeshPosition, to: NavMeshPosition): Option[DCELPath] = {
      if (from.navMeshFace == to.navMeshFace) {
        Some(DCELPath(Seq(GoBetweenPoints(from, to)))) //since polygons are convex
      } else {
        val starts = from.distancesToBorders
        val ends = to.distancesToBorders
        val path = GraphOps.shortestPath(navMeshGraph, starts, ends, (e: Scalar) => e, (n: PathNode) => n.point.distance(to.point), Some(params.maxPathLength))
        path match {
          case Some(foundPath) =>
            if (foundPath.nodes.length == 1) {
              //              println(s"Found path 1 node length $foundPath $from $to") //todo remove
              //              Some(DCELPath(Seq(GoBetweenPoints(from, to))))
              throw new Exception(s"Found path 1 node length $foundPath $from $to")
            } else {
              toPath(foundPath)
            }
          case None => None
        }
      }
    }

    /** Graph for finding patches on face level, contains inner DCEL edges and border edges
      * should be calculated and updated recursively for all childs first
      * */
    def edgeGraph: Graph[BorderNode, PathEdge[_, _]] = {
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
      for (Seq(f, t) <- ownAreaExits.combinations(2);
           p <- findPathOnNavMeshBetweenBorders(f, t)
           ) {
        res.addOrUpdateEdge(f, t, GoTroughFaceOnNavMesh(f, t, p.length, Some(p))) //todo mb don't save path
        res.addOrUpdateEdge(t, f, GoTroughFaceOnNavMesh(t, f, p.length, Some(p.reverse))) //todo mb don't save path
      }

      //nodes for moving through inner DCEL faces(moving inside face but not through own area)
      for (
        f <- face.hierarchicalFace.innerDCEL.innerFaces;
        Seq(fn, tn) <- f.data.ownData.navData.borderWaysOut.combinations(2);
        p <- f.data.ownData.navData.borderGraph.findEdge(fn, tn)
      ) {
        res.addOrUpdateEdge(fn, tn, GoTroughFace(fn, tn, f.data.ownData, p.length, Some(p)))
        res.addOrUpdateEdge(tn, fn, GoTroughFace(tn, fn, f.data.ownData, p.length, Some(p.reverse)))
      }

      res
    }

    def findPathOnEdgeGraph(from: BorderNode, to: BorderNode): Option[DCELPath] = {
//      if (edgeGraph.nodes.contains(from) && edgeGraph.nodes.contains(to)) {
//        GraphOps
//          .shortestPath(edgeGraph, Seq((from, 0d)), Seq((to, 0d)), (x: PathEdge[_, _]) => x.length, (n: PathNode) => n.point.distance(to.point), Some(params.maxPathLength))
//          .map(p => fromPath(p))
//      } else {
//        println(s"No nodes in edge graph $edgeGraph $from $to") //todo remove
//        None
//      }
      ???
    }

    def borderGraph: Graph[BorderNode, DCELPath] = {
      //todo use O(n^3) Floyd Warshall for border graphs, and path cache for biggest if needed
      val res = new ArrayBufferGraph[BorderNode, DCELPath]()

      for (he <- borderWaysOut) res.addNode(he)
      for (Seq(fromNode, toNode) <- borderWaysOut.combinations(2);
           path <- findPathOnEdgeGraph(fromNode, toNode)) {
        res.addEdge(fromNode, toNode, path)
        res.addEdge(toNode, fromNode, path.reverse)
      }
      res
    }


    /** Assumes that from is inside own area*/
    def reachableFromOwnAreaOwnAreaExits(from: NavMeshPosition): Seq[DCELPath] =
      ownAreaExits.flatMap(e => findPathOnNavMeshBetweenPositionAndBorder(from, e))

    //todo is it needed
    //def reachableFromOwnAreaOwnWaysOut(from: NavMeshPosition): Seq[DCELPath] =
    //  borderWaysOut.flatMap(e => findPathOnNavMeshBetweenPositionAndBorder(from, e))

    /** Assumes that from inside outer border and some level deeper in hierarchy */
//    def reachableDeepWaysOut(from: NavMeshPosition): Seq[DCELPath] = ???

    def findPath(from: NavMeshPosition, to: NavMeshPosition): Option[DCELPath] = {
//      val lca =
      ???
    }

//    def findPathInside(from: NavMeshPosition, to: NavMeshPosition) = ???

  }
}

trait NavigableFace {
  var navData: FaceNavData = _
  var hierarchicalFace: HierarchicalFace[NavigableDCELOwnData] = _
  def setFace[D <: NavigableDCELOwnData](face: HierarchicalFace[D]): Unit = hierarchicalFace = face.asInstanceOf[HierarchicalFace[NavigableDCELOwnData]]
}
