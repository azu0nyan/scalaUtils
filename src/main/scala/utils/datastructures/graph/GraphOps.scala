package utils.datastructures.graph

import utils.math.Scalar

import scala.collection.mutable

object GraphOps {
  type NodeId = Int

  trait Graph[NodeData, EdgeData] {

    private[graph] case class Edge(data: EdgeData, from: NodeId, to: NodeId)

    private[graph] case class Node(data: NodeData, outEdges: Seq[Edge]) {
      def toIds: Seq[NodeId] = outEdges.map(_.to)
    }

    /** return -1 if not found */
    private[graph] val nodeId: NodeData => NodeId

    private[graph] val nodesCount: Int

    private[graph] val node: Int => Node

  }

  trait GraphOps[NodeData, EdgeData] {
    def data: Graph[NodeData, EdgeData]

    def nodes: IterableOnce[NodeData] = for (i <- 0 until data.nodesCount) yield data.node(i).data

    def edges: IterableOnce[EdgeData] =
      for (
        i <- 0 until data.nodesCount;
        e <- data.node(i).outEdges
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
        val cur = data.node(curId)
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
                      pathCost: EdgeData => Scalar = ed => 1d,
                      nodeHeuristic: NodeData => Scalar = nd => 0d
                    ): (Seq[NodeData], Seq[EdgeData]) = {
      val fromId = data.node(from)
      val toId = data.node(to)

      val open:mutable.PriorityQueue[NodeId] = mutable.PriorityQueue()((firstId: Int, secondId: Int) => ???)

      val cameFrom:mutable.Map[NodeId, NodeId] = mutable.Map()


    }
  }

}
