package utils.datastructures.graph

import java.util.Comparator

import utils.math._

import scala.collection.mutable

object Graph {

  implicit class GraphOpsImpl[NodeData, EdgeData](g: Graph[NodeData, EdgeData]) extends GraphOps[NodeData, EdgeData] {
    override def data: Graph[NodeData, EdgeData] = g
  }

//  implicit def toGraphOps[NodeData, EdgeData](g: Graph[NodeData, EdgeData]): GraphOps[NodeData, EdgeData] = new GraphOps[NodeData, EdgeData] {
//    override def data: Graph[NodeData, EdgeData] = g
//  }
  type NodeId = Int
  type Cost = Scalar
  /** represents single part of path, with finish but without start */
  case class PathNode[NodeData, EdgeData](to: NodeData, by: EdgeData)

  case class Path[NodeData, EdgeData](start: NodeData, otherNodes: Seq[PathNode[NodeData, EdgeData]]) {
    def toNextNode: Option[Path[NodeData, EdgeData]] = Option.when(otherNodes.nonEmpty)(Path(otherNodes.head.to, otherNodes.tail))
    def length(implicit edgeLength: EdgeData => Cost): Cost = otherNodes.map(pathNode => edgeLength(pathNode.by)).reduceOption(_ + _).getOrElse(0d)
  }

  trait Graph[NodeData, EdgeData] {

    protected[GraphOps] case class Edge(data: EdgeData, from: NodeId, to: NodeId)

    protected[GraphOps] case class Node(data: NodeData, outEdges: Seq[Edge]) {
      def toIds: Seq[NodeId] = outEdges.map(_.to)
    }


    /** return -1 if not found */
    protected[GraphOps] def nodeId(node: NodeData): NodeId

    protected[GraphOps] def nodesCount: Int

    protected[GraphOps] def nodeById(id: Int): Node

  }

  trait GraphOps[NodeData, EdgeData] {
    def data: Graph[NodeData, EdgeData]

    def nodes: IterableOnce[NodeData] = for (i <- 0 until data.nodesCount) yield data.nodeById(i).data

    def edges: IterableOnce[EdgeData] =
      for (
        i <- 0 until data.nodesCount;
        e <- data.nodeById(i).outEdges
      ) yield e.data

    def dft(from: NodeData): Iterator[NodeData] = traversal(from, deepFirst = true)

    def bft(from: NodeData): Iterator[NodeData] = traversal(from, deepFirst = false)

    def traversal(from: NodeData, deepFirst: Boolean = false): Iterator[NodeData] = new Iterator[NodeData] {
      val fromId: NodeId = data.nodeId(from)

      val processed: Array[Boolean] = Array.ofDim(data.nodesCount)

      if (fromId >= 0) processed(fromId) = true

      var queue: Seq[NodeId] = if (fromId < 0) Seq() else Seq(fromId)

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): NodeData = {
        val curId = queue.head
        val cur = data.nodeById(curId)
        val neighboursToVisit = for (e <- cur.outEdges if !processed(e.to)) yield {
          processed(e.to) = true
          e.to
        }
        queue = if (deepFirst) neighboursToVisit ++ queue.tail else queue.tail ++ neighboursToVisit
        cur.data
      }
    }


    def shortestPath(
                      from: NodeData, to: NodeData,
                      pathCost: EdgeData => Cost = ed => 1d,
                      nodeHeuristic: NodeData => Cost = nd => 0d
                    ): Option[Path[NodeData, EdgeData]] = {
      val fromId = data.nodeId(from)
      val toId = data.nodeId(to)
      val fromNode = data.nodeById(fromId)
      val toNode = data.nodeById(toId)
      // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
      val gScore: mutable.Map[NodeId, Cost] = mutable.Map[NodeId, Cost]()
      gScore(fromId) = 0
      // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
      // how short a path from start to finish can be if it goes through n
      val fScore: mutable.Map[NodeId, Cost] = mutable.Map[NodeId, Cost]()
      fScore(fromId) = nodeHeuristic(fromNode.data)

      val open: mutable.SortedSet[NodeId] = mutable.SortedSet[NodeId]()((x: NodeId, y: NodeId) => math.signum(fScore(x) - fScore(y)).toInt)
      open += fromId

      val cameFrom: mutable.Map[NodeId, NodeId] = mutable.Map()
      val cameBy: mutable.Map[NodeId, EdgeData] = mutable.Map()
      def reconstructPath(): Path[NodeData, EdgeData] = Path[NodeData, EdgeData](fromNode.data, {
        var path: Seq[PathNode[NodeData, EdgeData]] = Seq()
        var currentNode = toId
        while (currentNode >= 0) {
          path = PathNode[NodeData, EdgeData](data.nodeById(currentNode).data, cameBy(currentNode)) +: path
          currentNode = cameFrom.getOrElse(currentNode, -1)
        }
        path
      })


      open += fromId
      while (open.nonEmpty) {
        val current = open.firstKey
        open -= current
        if (current == toId) {
          return Some(reconstructPath())
        } else {
          val curNode = data.nodeById(current)
          val curNodeScore = gScore(current)
          curNode.outEdges.foreach { edge =>
            val toNode = edge.to
            val tentativeGScore = curNodeScore + pathCost(edge.data)
            if (!gScore.contains(toNode)) { //newNode
              cameFrom(toNode) = current
              cameBy(toNode) = edge.data
              gScore(toNode) = tentativeGScore
              fScore(toNode) = tentativeGScore + nodeHeuristic(data.nodeById(toNode).data)
              open += toNode
            } else if (tentativeGScore < gScore(toNode)) { //found better way
              if (open.contains(toNode)) open -= toNode //remove before ordering change todo change to minheap or smth
              cameFrom(toNode) = current
              cameBy(toNode) = edge.data
              gScore(toNode) = tentativeGScore
              fScore(toNode) = tentativeGScore + nodeHeuristic(data.nodeById(toNode).data)
              open += toNode //addBack
            }
          }
        }
      }

      return None

    }
  }

}
