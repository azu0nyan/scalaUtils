package utils.datastructures.dcel.nav

import utils.datastructures.dcel.nav.NavigableDCEL.NavigableFace
import utils.datastructures.dcel.nav.DCELPath._
import utils.datastructures.graph.ArrayBufferGraph
import utils.datastructures.graph.Graph.Graph

/*
**==============================================================**=========================================
||                                   e3                         ||
||      Area A                                                  ||           Area G
||                    *--------|d4|--------*-----------------*  ||
||                    |                    |                  | ||
||                    |                    |                  _ __
||                    |                    |                 d6
||                    |                    |                  _ __
||                    |                    |                  | ||
||                    |                    |                  | ||
||                    |                    |                  | || *---------------------*
||                    |                    |                  _ || _                     |
||                    |    Area C          |     Area D      d5 ||        Area F         |
||                    |                    |                  _ || _                     |
||e4                  |                    |                  | || |                     |
||                    |                    |                  | || |                     |
|| *------|d2|--------*---------|d3|-------*------------------* || *---------------------*
|| |            idE4      idE3             |                    ||
|| |                                       |                    ||
|| |                                       |                 e2 ||
|| |idE5          Area B                   |                    __
|| |                                  idE2 |                    d1
|| |                idE1                   |                    __
|| *-------------------------------------- *   e1               ||
**==============================================================**==============================================
                                               e1.twin
 Area A border ways out = d1
 Area A inner DCEL to me ways = d2, d4
 Area A inner DCEL ways out = d5, d6
 Area A ways out = d1, d6, d5
 Area A own area exits = d1, d2, d4
 */

class FaceNavData(area: NavigableFace) {

  //todo filter out edges filled with impassable childs

  /** Edges through you can leave area */
  val borderWaysOut: Seq[BorderNode] = area.hierarchicalFace.fullBorder
    .filter(_.data.ownData.passable)
    .map(e => BorderNode(e.data.ownData))

  /** inner dcel edges  through you can enter area */
  val innerDcelToMeWays: Seq[BorderNode] = area.hierarchicalFace.innerDCEL.halfEdges
    .filter(e => e.data.ownData.passable && e.leftFace == area
      .hierarchicalFace.innerDCEL.outerFace) //todo maybe this should be checked for twin???
    .map(e => BorderNode(e.data.ownData)).toSeq



  //Graphs


  /** Graph for finding patches on area level, contains inner DCEL edges and border edges
    * should be calculated and updated recursively for all childs first
    * */
  val edgeGraph: Graph[BorderNode, BetweenBordersEdge] = {
    val res = new ArrayBufferGraph[BorderNode, BetweenBordersEdge]()
    //    fullBorder.foreach(res.addNode)
    //    innerDcel.halfEdges.foreach(res.addNode)
    //    innerDcel.halfEdges.foreach(he => res.addEdge(he, he.twin, 0d))

    val edges = area.hierarchicalFace.fullBorder ++ area.hierarchicalFace.innerDCEL.halfEdges //todo mb map to BorderNode to reduce allocations
    edges.map(e => BorderNode(e.data.ownData)).foreach(res.addNode)

    //edges between free borders
    for (e <- edges if e.data.ownData.passable;
         fromNode = BorderNode(e.data.ownData);
         toNode = BorderNode(e.twin.data.ownData)) {
      res.addOrUpdateEdge(fromNode, toNode, GoTroughBorder(fromNode, toNode))
      res.addOrUpdateEdge(toNode, fromNode, GoTroughBorder(toNode, fromNode))
    }


    //todo maybe count connectivity here
    //add nodes from this dcel to innerDcel
    for (he <- area.hierarchicalFace.fullBorder if he.data.ownData.passable;
         child <- he.data.childs if child.ownData.passable) {
      //better map edges
      val fromNode = BorderNode(he.data.ownData)
      val toNode = BorderNode(child.edge.data.ownData) //should be added to graph since it is inner DCEL node
      res.addOrUpdateEdge(fromNode, toNode, GoToChild(fromNode, toNode))
      res.addOrUpdateEdge(toNode, fromNode, GoToParent(toNode, fromNode))
    }

    //todo
    //add nodes for inside area movement
//    for (Seq(fromNode, toNode) <- ownAreaExits.combinations(2);
//         p <- findPathOnVisibilityGraph(fromNode.point, toNode.point)) {
//      val len = p.length
//      res.addOrUpdateEdge(fromNode, toNode, GoTroughAreaOnVisGraph(fromNode, toNode, area, len, Some(p)))
//      res.addOrUpdateEdge(toNode, fromNode, GoTroughAreaOnVisGraph(toNode, fromNode, area, len, Some(p.reverse)))
//    }

    //same for all inner dcel faces
//    for (
//      face <- area.innerDcel.innerFaces;
//      Seq(fromNode, toNode) <- face.data.navData.waysOut.combinations(2);
//      p <- face.data.navData.borderGraph.findEdge(fromNode, toNode)
//    ) {
//      res.addOrUpdateEdge(fromNode, toNode, GoTroughArea(fromNode, toNode, face.data, p.length, Some(p)))
//      res.addOrUpdateEdge(toNode, fromNode, GoTroughArea(toNode, fromNode, face.data, p.length, Some(p.reverse)))
//    }

    res
  }


    def findPathOnEdgeGraph(from: BorderNode, to: BorderNode): Option[DCELPath] =
      if (edgeGraph.nodes.contains(from) && edgeGraph.nodes.contains(to)) {
        edgeGraph.shortestPath(
          from,
          to,
          x => x.length) map {
          path => DCELPath.fromPath(path)
        }
      } else None

    /** Precalculated graph of distances between outConnections build based on edge graph */
    var borderGraph: Graph[BorderNode, DCELPath] = {
      //todo use O(n^3) Floyd Warshall for border graphs, and path cache for biggest if needed

      val res = new ArrayBufferGraph[BorderNode, DCELPath]()

      for (he <- waysOut) res.addNode(he)
      for (Seq(fromNode, toNode) <- waysOut.combinations(2);
           path <- findPathOnEdgeGraph(fromNode, toNode)) {
        res.addEdge(fromNode, toNode, path)
        res.addEdge(toNode, fromNode, path.reverse)
      }
      res
    }
/*
    /** reachable ways to leave own area */
    def reachableFromOwnAreaOwnAreaExits(from: V2): Seq[DCELPath] =
      ownAreaExits.flatMap(e => findPathOnVisibilityGraph(from, e.point).map(x => x.appendNode(e)))

    /** Paths starts at from with wayOut as last node todo optimize */
    def reachableFromOwnAreaWaysOut(from: V2): Seq[DCELPath] =
      waysOut.flatMap(e => findPathOnVisibilityGraph(from, e.point).map(x => x.appendNode(e)))


    def reachableDeepWaysOut(from: V2, maxLength: Scalar): Seq[DCELPath] =
      if (area.containsPoint(from)) {
        //check if we need to go down
        val lowestArea = area.areaAt(from)
        if (lowestArea == area) {
          reachableFromOwnAreaWaysOut(from)
        } else {
          def twinOpt(p: PathNode): Option[(PathNode, PathEdge[PathNode, PathNode])] = p match {
            case _: PointNode => None
            case e: FreeBorderNode => e.twinNode.map(n => (n, GoTroughBorder(e, n)))
            case p: PortalNode => Some((p.twinNode, GoTroughPortal(p, p.twinNode)))
          }
          //we only use connection between innerAreas and this
          //since inner area is thils of this
          //connections with deeper child's processed in recursive
          //container.reachableDeepWaysOut call
          (for (
            container <- area.directChildAreas.find(_.containsPoint(from)).toSeq;
            deepWayOut <- container.navData.reachableDeepWaysOut(from, maxLength) if deepWayOut.length <= maxLength;
            (twinNode, toTwinEdge) <- twinOpt(deepWayOut.waypoints.last).toSeq;
            wayOut <- waysOut;
            path <- findPathOnEdgeGraph(twinNode.asInstanceOf[BorderNode], wayOut.asInstanceOf[BorderNode])
          ) yield deepWayOut.appendPath(path, Some(toTwinEdge))) //0d price of going to twin node todo increase for doors
            .groupBy(p => p.waypoints.last)
            .map { case (_, paths) => paths.minBy(_.length) } //todo maybe flatmap, since some path's can be lost
            .filter(_.length <= maxLength)
            .toSeq
        }
      } else
        Seq()


    /** from and to should be inside outer poly todo use maxLength */
    def findPathInsideLowest(from: V2, to: V2, maxLength: Scalar): Option[DCELPath] = {
      val fromFace = area.innerDcel.faceAt(from)
      val toFace = area.innerDcel.faceAt(to)
      game.Logs.pathfinding.trace(s"Finding path inside ${area.data.name} from  ${fromFace.data.data.name} to ${toFace.data.data.name}")


      val visGraphPath = findPathOnVisibilityGraph(from, to)
      game.Logs.pathfinding.trace(s"Path on visibility graph ${if (visGraphPath.nonEmpty) "found" else "not found"}.")
      val starts =
        if (fromFace == area.innerDcel.outerFace) reachableFromOwnAreaOwnAreaExits(from)
        else fromFace.data.navData.reachableDeepWaysOut(from, maxLength)
      val finishes =
        (if (toFace == area.innerDcel.outerFace) reachableFromOwnAreaOwnAreaExits(to) else
          toFace.data.navData.reachableDeepWaysOut(to, maxLength)
          ) map (p => p.reverse)
      game.Logs.pathfinding.trace(s"Found starts ${starts.map(s => (s.waypoints.last, s.waypoints.last.point, s.length))}")
      game.Logs.pathfinding.trace(s"Found finishes ${finishes.map(f => (f.waypoints.head, f.waypoints.head.point, f.length))}")
      val edgePath = GraphOps.shortestPath[BorderNode, BetweenBordersEdge](
        edgeGraph,
        starts.map(x => (x.waypoints.last.asInstanceOf[BorderNode], x.length)),
        finishes.map(x => (x.waypoints.head.asInstanceOf[BorderNode], x.length)),
        (e: BetweenBordersEdge) => e.length) map {
        path =>
          val head: DCELPath = starts.find(x => x.waypoints.last == path.nodes.head).get
          val body: DCELPath = DCELPath.fromPath(path)
          val tail: DCELPath = finishes.find(x => x.waypoints.head == path.nodes.last).get
          game.Logs.pathfinding.trace(head.waypoints.last.point.toString)
          game.Logs.pathfinding.trace(tail.waypoints.head.point.toString)
          val res = head.appendPath(body, None).appendPath(tail, None)
          res
      }

      (visGraphPath, edgePath) match {
        case (Some(p1), Some(p2)) if p1.length > p2.length => Some(p2)
        case (Some(shortest), Some(_)) => Some(shortest)
        case (Some(p1), _) => Some(p1)
        case (_, Some(p2)) => Some(p2)
        case _ => None //todo if fromFace == toDace && face has several connectivity components try search path in parent until maxLength hit
      }
    }
  */
}
