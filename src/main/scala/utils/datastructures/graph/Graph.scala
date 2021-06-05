package utils.datastructures.graph

import java.util.Comparator

import utils.datastructures.containers.BinHeap
import utils.math._

import scala.collection.mutable

object Graph {


  type NodeId = Int
  type Cost = Scalar
  type AdjacencyList[NodeData, EdgeData] = Seq[(NodeData, Seq[(EdgeData, NodeData)])]
  /** represents single part of path, with finish but without start */
  case class PathNode[NodeData, EdgeData](to: NodeData, by: EdgeData)

  case class Path[NodeData, EdgeData](start: NodeData, otherNodes: Seq[PathNode[NodeData, EdgeData]]) {
    def nodes: Seq[NodeData] = start +: otherNodes.map(_.to)
    def edges: Seq[EdgeData] = otherNodes.map(_.by)

    def toNextNode: Option[Path[NodeData, EdgeData]] = Option.when(otherNodes.nonEmpty)(Path(otherNodes.head.to, otherNodes.tail))
    def length(implicit edgeLength: EdgeData => Cost = ed => 1d): Cost = otherNodes.map(pathNode => edgeLength(pathNode.by)).reduceOption(_ + _).getOrElse(0d)
  }

  /** Graph data interface anf generic implementations, override any method for more efficient implementation
   * NodeData serves as unique id of node, provided by user
   * */
  trait Graph[NodeData, EdgeData] {

    protected case class Edge(data: EdgeData, from: NodeId, to: NodeId)

    protected case class Node(data: NodeData, outEdges: Seq[Edge]) {
      def toIds: Seq[NodeId] = outEdges.map(_.to)
    }


    @inline protected def nodeByData(node: NodeData): Node = nodeById(nodeId(node))

    /** return -1 if not found override for more efficient implementation */
    def nodeId(node: NodeData): NodeId = nodes.indexOf(node)
    /***/
    def nodeIds: Iterator[NodeId]

    def innerNodes:Iterator[Node] = nodeIds.map(nodeById)

    @inline def nodeById(id: Int): Node


    //PUBLIC interface
    def nodes: Iterator[NodeData] = for (i <- nodeIds) yield nodeById(i).data

    def edges: Iterator[EdgeData] =
      for (
        i <- nodeIds;
        e <- nodeById(i).outEdges iterator
      ) yield e.data

    def findEdge(from:NodeData, to:NodeData):Option[EdgeData] = {
      val toId = nodeId(to)
      val fromId = nodeId(from)
      if(toId >= 0 && fromId >= 0) {
        nodeById(fromId).outEdges.find(_.to == toId).map(_.data)
      } else None
    }





    def neighbours(n: NodeData): Seq[NodeData] = nodeByData(n).outEdges.map(e => nodeById(e.to).data)

    def edgesAndNeighbours: Iterator[(NodeData, EdgeData, NodeData)] = nodes.flatMap(nd => nodeById(nodeId(nd)).outEdges.map(e => (nd, e.data, nodeById(e.to).data)))

    def edgesAndNeighboursFor(n: NodeData): Seq[(EdgeData, NodeData)] = nodeByData(n).outEdges.map(e => (e.data, nodeById(e.to).data))

    def edgesFrom(n: NodeData): Seq[EdgeData] = nodeByData(n).outEdges.map(_.data)

    def edgesFromTo(from: NodeData, to:NodeData): Seq[EdgeData] = nodeByData(from).outEdges.filter(e => nodeById(e.to) == nodeByData(to)).map(_.data)

    /** default implementation highly inefficient */
    def edgesDirectedTo(to:NodeData):Seq[(EdgeData, NodeData)] = {
      val toId = nodeId(to)
      innerNodes.filter(_.outEdges.exists(_.to == toId)).map(x => (x.outEdges.find(_.to == toId).get.data, x.data)).toSeq
    }

    /*deep first traversal**/
    def dft(from: NodeData): Iterator[NodeData] = traversal(from, deepFirst = true)

    /*breadth first traversal**/
    def bft(from: NodeData): Iterator[NodeData] = traversal(from, deepFirst = false)

    def traversal(from: NodeData, deepFirst: Boolean = false): Iterator[NodeData] = new Iterator[NodeData] {
      val fromId: NodeId = nodeId(from)

      val processed: mutable.Set[NodeId] = mutable.Set()

      if (fromId >= 0) processed += fromId

      var queue: Seq[NodeId] = if (fromId < 0) Seq() else Seq(fromId)

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): NodeData = {
        val curId = queue.head
        val cur = nodeById(curId)
        val neighboursToVisit = for (e <- cur.outEdges if !processed.contains(e.to)) yield {
          processed += e.to
          e.to
        }
        queue = if (deepFirst) neighboursToVisit ++ queue.tail else queue.tail ++ neighboursToVisit
        cur.data
      }
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
    def shortestPath(
                      from: NodeData, to: NodeData,
                      pathCost: EdgeData => Cost = ed => 1d,
                      nodeHeuristic: NodeData => Cost = nd => 0d
                    ): Option[Path[NodeData, EdgeData]] = {
      val fromId = nodeId(from)
      val toId = nodeId(to)
      val fromNode = nodeById(fromId)
      //   val toNode = nodeById(toId)
      // For node n, knownBest[n] is the cost of the cheapest path from start to n currently known.
      val knownBest: mutable.Map[NodeId, Cost] = mutable.Map[NodeId, Cost]()
      knownBest(fromId) = 0
      // For node n, quenedBestGuesses[n] := knownBest[n] + h(n). quenedBestGuesses[n] represents our current best guess as to
      // how short a path from start to finish can be if it goes through n
      val quenedBestGuesses: mutable.Map[NodeId, Cost] = mutable.Map[NodeId, Cost]()
      quenedBestGuesses(fromId) = nodeHeuristic(fromNode.data)

      val openQueue: BinHeap[NodeId] = new BinHeap[NodeId]()(Ordering.by((n: NodeId) => quenedBestGuesses(n)))

      val cameFrom: mutable.Map[NodeId, NodeId] = mutable.Map()
      val cameBy: mutable.Map[NodeId, EdgeData] = mutable.Map()
      def reconstructPath(): Path[NodeData, EdgeData] = Path[NodeData, EdgeData](fromNode.data, {
        var path: Seq[PathNode[NodeData, EdgeData]] = Seq()
        var currentNode = toId
        while (currentNode >= 0 && currentNode != fromId) {
          path = PathNode[NodeData, EdgeData](
            nodeById(currentNode).data,
            cameBy(currentNode)
          ) +: path
          currentNode = cameFrom.getOrElse(currentNode, -1)
        }
        path
      })


      openQueue.add(fromId)
      while (openQueue.nonEmpty) {
        val current = openQueue.poll()
        if (current == toId) {
          return Some(reconstructPath())
        } else {
          val curNode = nodeById(current)
          val curNodeScore = knownBest(current)
          curNode.outEdges.foreach { edge =>
            val toNode = edge.to
            val costWithCurrentEdge = curNodeScore + pathCost(edge.data)
            // we encountered `toNode` first time || found better way, `>` filters paths with same cost
            if (!knownBest.contains(toNode) || knownBest(toNode) > costWithCurrentEdge) {
              knownBest(toNode) = costWithCurrentEdge
              quenedBestGuesses(toNode) = costWithCurrentEdge + nodeHeuristic(nodeById(toNode).data)
              cameFrom(toNode) = current
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

}
