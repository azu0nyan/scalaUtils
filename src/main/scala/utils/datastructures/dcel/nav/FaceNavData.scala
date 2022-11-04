package utils.datastructures.dcel.nav

import utils.datastructures.dcel.nav.NavigableDCEL.NavigableFace
import utils.datastructures.dcel.nav.DCELPath._

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

  /*
  /** Portals on own edges that teleports outside area todo remove border to inside portal connections */
  val borderWaysOut: Seq[BorderNode] = area.fullBorder
    .flatMap(e => e.data.portals.filter(p =>
      p.from.edge != e && p.from.edge.leftFace.data != area || p.to.edge != e && p.to.edge.leftFace.data != area)
      .map(p => PortalNode(e.data, p))) ++
    area.fullBorder.filter(e => e.data.wall.isEmpty).map(e => FreeBorderNode(e.data))

  /** inner dcel edges that has connection to my own area, portals or free edges on innerDcel.outerFace border */
  val innerDcelToMeWays: Seq[BorderNode] =
    area.innerDcel.halfEdges.iterator.flatMap { e =>
      e.data.portals.filter { p =>
        val fFace = p.from.edge.leftFace
        val tFace = p.to.edge.leftFace
        fFace == area.face || tFace == area.face ||
          (fFace == area.innerDcel.outerFace && tFace != area.innerDcel.outerFace) || //prevents hanging edges that borders only outer poly from addition
          (fFace != area.innerDcel.outerFace && tFace == area.innerDcel.outerFace)
      }.map(p => PortalNode(e.data, p)) ++
        Option.when {
          !e.data.isFake && e.data.wall.isEmpty && {
            !e.twin.data.isFake && e.twin.leftFace == area.innerDcel.outerFace ||
              e.twin.data.isFake &&
                EdgeOps.lowestTwin(e, e.asSegment.center).data.wall.isEmpty
          }
        }(FreeBorderNode(e.data)).toSeq
    }.toSeq


  /** inner dcel edges that has portal to outside or share border with area  and have no wall */
  val innerDcelWaysOut: Seq[BorderNode] =
    area.innerDcel.halfEdges.iterator.flatMap(e => e.data.portals.filter { p =>
      !p.from.edge.leftFace.data.parent.contains(area) || !p.from.edge.leftFace.data.parent.contains(area)
    }.map(p => PortalNode(e.data, p))).toSeq

  val waysOut: Seq[BorderNode] = borderWaysOut ++ innerDcelWaysOut ++
    area.directChildAreas.flatMap(a => a.navData.waysOut).filter {
      //      case PointNode(point, _) => false
      case e@FreeBorderNode(border) => !area.isAncestorOf(EdgeOps.lowestTwin(border.edge, e.point).leftFace.data)
      case PortalNode(from, portal) => !area.isAncestorOf(portal.from.edge.leftFace.data) || !area.isAncestorOf(portal.to.edge.leftFace.data)
    }

  /** ways to leave own area */
  val ownAreaExits: Seq[BorderNode] = innerDcelToMeWays ++ borderWaysOut


  //Graphs


  /** For finding patches inside area, especially between own portals out and innerDCEL  to me connections
    * Visibility graph builds on Area &~ {Area.childs}
    * Last step of path node expansion */
  val visibilityGraph: Graph[V2, Scalar] = {
    Logs.pathDataGeneration.trace(s"Calculating visibility graph for ${area.data.name}")
    //cw, ccw
    val (containers, holes) = area.ownArea.regions.partition(_.areaSign >= 0)
    val addPoints = (borderWaysOut ++ innerDcelToMeWays map (_.point))
      .filter(p => area.ownArea.contains(p))
    VisibilityGraphOps.buildVisibilityGraph(containers ++ holes, addPoints)
  }

  def findPathOnVisibilityGraph(from: V2, to: V2): Option[DCELPath] = {
    if (area.visible(from, to))
      Some(DCELPath(Seq(GoBetweenPoints(PointNode(from, area), PointNode(to, area)))))
    else {
      val visFrom = visibilityGraph.nodes.filter(node => area.visible(node, from)).map(n => (n, n.distance(from))).toSeq
      val visTo = visibilityGraph.nodes.filter(node => area.visible(node, to)).map(n => (n, n.distance(to))).toSeq
      if (visFrom.nonEmpty && visTo.nonEmpty) {
        GraphOps.shortestPath(visibilityGraph, visFrom, visTo, (e: Scalar) => e, (n: V2) => n.distance(to)) map {
          path => {
            val nodes =
              (if (path.nodes.head != from) Seq(from) else Seq()) ++ path.nodes ++
                (if (path.nodes.last != to) Seq(to) else Seq())
            if (nodes.length == 1) {
              log.info(s"Found path one node length")
              DCELPath(Seq(GoBetweenPoints(PointNode(from, area), PointNode(to, area))))
            } else
              DCELPath(nodes.map(PointNode(_, area)).sliding(2).map { case Seq(f, s) => GoBetweenPoints(f, s) }.toSeq)
            //            DCELPath(nodes.sliding(2).map{case Seq(f, s) => (PointNode(f), f.distance(s))}.toSeq)
          }
        }
      } else None
    }
  }

  /** Graph for finding patches on area level, contains inner DCEL edges and border edges
    * should be calculated and updated recursively for all childs first
    * */
  val edgeGraph: Graph[BorderNode, BetweenBordersEdge] = {
    Logs.pathDataGeneration.trace(s"Calculating edge graph for ${area.data.name}")
    val res = new ArrayBufferGraph[BorderNode, BetweenBordersEdge]()
    //    fullBorder.foreach(res.addNode)
    //    innerDcel.halfEdges.foreach(res.addNode)
    //    innerDcel.halfEdges.foreach(he => res.addEdge(he, he.twin, 0d))

    val edges = area.fullBorder ++ area.innerDcel.halfEdges
    edges.flatMap(_.data.pathNodes).foreach(res.addNode)


    //edges between free borders
    for (e <- edges;
         fromNode <- e.data.pathNodes.collect { case e: FreeBorderNode => e };
         toNode <- fromNode.border.edge.twin.data.pathNodes.collect { case e: FreeBorderNode => e } //using twin instead of fromNode.reverseDirectionNode
         ) {
      res.addOrUpdateEdge(fromNode, toNode, GoTroughBorder(fromNode, toNode))
      res.addOrUpdateEdge(toNode, fromNode, GoTroughBorder(toNode, fromNode))
    }
    //edges for portals
    for (e <- edges;
         fromNode <- e.data.pathNodes.collect { case e: PortalNode => e }
         ) {
      if (fromNode.travelsToEdge.leftFace.data == area || area.directChildAreas.contains(fromNode.travelsToEdge.leftFace.data)) {
        val toNode = fromNode.twinNode
        res.addOrUpdateEdge(fromNode, toNode, GoTroughPortal(fromNode, toNode))
        res.addOrUpdateEdge(toNode, fromNode, GoTroughPortal(toNode, fromNode))
      }
    }


    //todo maybe count connectivity here
    //add nodes from this dcel to innerDcel
    for (he <- area.fullBorder if he.data.wall.isEmpty;
         child <- he.data.childs if child.wall.isEmpty) {
      //better map edges
      val fromNode = FreeBorderNode(he.data)
      val toNode = FreeBorderNode(child.edge.data) //should be added to graph since it is inner DCEL node
      res.addOrUpdateEdge(fromNode, toNode, GoToChildBorder(fromNode, toNode))
      res.addOrUpdateEdge(toNode, fromNode, GoToParentBorder(toNode, fromNode))
    }

    //todo maybe count connectivity here
    //    add fake edges between portals on same edge and edge
    for (e <- edges;
         p1 <- e.data.pathNodes;
         p2 <- e.data.pathNodes if p1 != p2) {
      res.addOrUpdateEdge(p1, p2, GoAlongSameBorder(p1, p2))
    }

    if (ownAreaExits.size > AreaNavData.combinedNodesTesholdWarning)
      log.warn(s"Calculating and storing combinations of ${ownAreaExits.size} nodes ${area.data.name}.ownAreaExits " +
        s"${ownAreaExits.size * (ownAreaExits.size - 1) / 2} paths will be stored, maybe it's too much.")

    //add nodes for inside area movement
    for (Seq(fromNode, toNode) <- ownAreaExits.combinations(2);
         p <- findPathOnVisibilityGraph(fromNode.point, toNode.point)) {
      val len = p.length
      res.addOrUpdateEdge(fromNode, toNode, GoTroughAreaOnVisGraph(fromNode, toNode, area, len, Some(p)))
      res.addOrUpdateEdge(toNode, fromNode, GoTroughAreaOnVisGraph(toNode, fromNode, area, len, Some(p.reverse)))
    }
    //same for all inner dcel faces
    for (
      face <- area.innerDcel.innerFaces;
      Seq(fromNode, toNode) <- face.data.navData.waysOut.combinations(2);
      p <- face.data.navData.borderGraph.findEdge(fromNode, toNode)
    ) {
      if (face.data.navData.waysOut.size > AreaNavData.combinedNodesTesholdWarning)
        log.warn(s"Calculating and storing combinations of ${face.data.navData.waysOut.size} nodes ${area.data.name}->${face.data.data.name}.waysOut " +
          s"${face.data.navData.waysOut.size * (face.data.navData.waysOut.size - 1) / 2} paths will be stored, maybe it's too much.")


      res.addOrUpdateEdge(fromNode, toNode, GoTroughArea(fromNode, toNode, face.data, p.length, Some(p)))
      res.addOrUpdateEdge(toNode, fromNode, GoTroughArea(toNode, fromNode, face.data, p.length, Some(p.reverse)))
    }

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
    Logs.pathDataGeneration.trace(s"Calculating border graphs for ${area.data.name}")
    if (waysOut.size > AreaNavData.combinedNodesTesholdWarning)
      log.warn(s"Calculating and storing combinations of ${waysOut.size} nodes of ${area.data.name}.waysOut. During border graph computation. Which is O(n^4) " +
        f"${waysOut.size * (waysOut.size - 1) / 2} paths max can be stored," +
        f" and ${waysOut.size * waysOut.size * waysOut.size * waysOut.size / 1000000000d}%.2f GOps. Maybe it's too much.")

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
