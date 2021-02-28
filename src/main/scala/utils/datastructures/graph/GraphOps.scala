package utils.datastructures.graph

import utils.datastructures.containers.{BinHeap, DisjointSet}
import utils.datastructures.graph.Graph.{Cost, Graph, NodeId, Path, PathNode}
import utils.math.Scalar

import scala.collection.mutable

object GraphOps {
  def minimumSpanningTree[ND, ED](g: Graph[ND, ED], length: ED => Scalar): Graph[ND, ED] = {
    val res = new ArrayBufferGraph[ND, ED](false)
    val set = new DisjointSet[ND]
    for (v <- g.nodes) {
      set.makeSet(v)
      res.addNode(v)
    }

    for ((f, w, t) <- g.edgesAndNeighbours.toSeq.sortBy(e => length(e._2))) {
      val n1 = set.findSet(f)
      val n3 = set.findSet(t)
      if (n1 != n3) {
        res.addEdge(f, t, w)
        set.union(n1, n3)
      }
    }

    res
  }

  def reachableFrom[ND, ED](g: Graph[ND, ED], vertex: ND): Seq[ND] = {
    g.traversal(vertex).toSeq
  }

  def strongConnectivityComponentsOnDirectedGraph[ND, ED](g: Graph[ND, ED]): Seq[Seq[ND]] = {
    var unvisited: Set[ND] = g.nodes.toSet
    var res: Seq[Seq[ND]] = Seq()
    while (unvisited.nonEmpty) {
      val comp = g.traversal(unvisited.head).toSeq
      res = comp +: res
      unvisited = unvisited &~ comp.toSet
    }
    res
  }

  def fromAdjacencyMatrix[ND, ED](nodes: IndexedSeq[ND], edges: IndexedSeq[IndexedSeq[Option[ED]]]): Graph[ND, ED] = {
    val g = new ArrayBufferGraph[ND, ED](false)
    nodes.foreach(n => g.addNode(n))

    for (i <- edges.indices; j <- edges(i).indices; e <- edges(i)(j)) {
      g.addEdge(nodes(i), nodes(j), e)
    }

    g
  }


  /**
   * Finds shortest path with A*
   *
   * @param from          start
   * @param to            finish
   * @param pathCost      , mapping of edge to it's cost
   * @param nodeHeuristic heuristic for distance left to finish from given node
   * @return path or None if not found
   */
  def shortestPath[NodeData, EdgeData](
                                        graph: Graph[NodeData, EdgeData],
                                        from: Seq[(NodeData, Cost)], to: Seq[(NodeData, Cost)],
                                        pathCost: EdgeData => Cost,
                                        nodeHeuristic: NodeData => Cost = (nd:NodeData) => 0d,
                                        maxCost: Option[Cost] = None
                                      ): Option[Path[NodeData, EdgeData]] = {
    // For node n, knownBest[n] is the cost of the cheapest path from start to n currently known.
    val knownBest: mutable.Map[NodeId, Cost] = mutable.Map[NodeId, Cost]()

    val fromIds: Seq[Int] = from.map {
      f =>
        graph.nodeId(f._1)
    }

    val toId = -1
    val toIdToCost: Map[Int, Cost] = to.map { f =>
      (graph.nodeId(f._1), f._2)
    }.toMap
    //   val fromNode = graph.nodeById(fromId)
    //   val toNode = nodeById(toId)


    // For node n, quenedBestGuesses[n] := knownBest[n] + h(n). quenedBestGuesses[n] represents our current best guess as to
    // how short a path from start to finish can be if it goes through n
    val quenedBestGuesses: mutable.Map[NodeId, Cost] = mutable.Map[NodeId, Cost]()
    //    quenedBestGuesses(fromId) = nodeHeuristic(fromNode.data)
    from.foreach { f =>
      val id = graph.nodeId(f._1)
      val node = graph.nodeById(id)
      knownBest(id) = f._2
      quenedBestGuesses(id) = f._2 + nodeHeuristic(node.data)
    }


    val openQueue: BinHeap[NodeId] = new BinHeap[NodeId]()(Ordering.by((n: NodeId) => quenedBestGuesses(n)))
    for (fromId <- fromIds) {
      openQueue.add(fromId)
    }

    val cameFrom: mutable.Map[NodeId, NodeId] = mutable.Map()
    /** empty for mock nodes */
    val cameBy: mutable.Map[NodeId, EdgeData] = mutable.Map()

    def reconstructPath(): Path[NodeData, EdgeData] = {
      var path: Seq[PathNode[NodeData, EdgeData]] = Seq()
      var currentNode = toId
      while (!fromIds.contains(currentNode)) {
        //skip mock node
        if (currentNode != toId) {
          path = PathNode[NodeData, EdgeData](
            graph.nodeById(currentNode).data,
            cameBy(currentNode)
          ) +: path
        }
        currentNode = cameFrom.getOrElse(currentNode, -1)
      }
      Path[NodeData, EdgeData](graph.nodeById(currentNode).data, path)
    }


    while (openQueue.nonEmpty) {
      val currentId = openQueue.poll()
      if (currentId == toId) {
        return Some(reconstructPath())
      } else if (toIdToCost.contains(currentId)) {
        val curNode = graph.nodeById(currentId)
        val curNodeScore = knownBest(currentId)
        val costWithCurrentEdge = curNodeScore + toIdToCost(currentId)
        val toNode = toId
        quenedBestGuesses(toNode) = costWithCurrentEdge //+ nodeHeuristic(graph.nodeById(toNode).data) == 0 since its end
        cameFrom(toNode) = currentId
        //        cameBy(toNode) = edge.data
        if (openQueue.contains(toNode)) openQueue.onOrderingChangedFor(toNode)
        else openQueue.add(toNode)

      } else {
        val curNode = graph.nodeById(currentId)
        val curNodeScore = knownBest(currentId)
        curNode.outEdges.foreach { edge =>
          val toNode = edge.to
          val costWithCurrentEdge = curNodeScore + pathCost(edge.data)
          // we encountered `toNode` first time || found better way, `>` filters paths with same cost
          if (!knownBest.contains(toNode) || knownBest(toNode) > costWithCurrentEdge) {
            knownBest(toNode) = costWithCurrentEdge
            quenedBestGuesses(toNode) = costWithCurrentEdge + nodeHeuristic(graph.nodeById(toNode).data)
            cameFrom(toNode) = currentId
            cameBy(toNode) = edge.data
            if (openQueue.contains(toNode)) openQueue.onOrderingChangedFor(toNode)
            else openQueue.add(toNode)
          }
        }
      }
    }

    return None

  }





}


