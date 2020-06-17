package utils.datastructures.graph

import utils.datastructures.graph.Graph._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class ArrayBufferGraph[NodeData, EdgeData](
                                            val bidirectional: Boolean
                                          ) extends Graph[NodeData, EdgeData] {
  //Interface impl
  val nodeDatas: ArrayBuffer[Node] = ArrayBuffer()

  override protected def nodeIds: Iterator[NodeId] = nodeDatas.indices.iterator

  @inline override protected def nodeById(id: Int): Node = nodeDatas(id)

  protected val nodeDataToID: mutable.Map[NodeData, NodeId] = mutable.Map()

  override protected def nodeId(node: NodeData): NodeId = nodeDataToID(node)

  //inits
  /** ignores 'bidirectional' flag */
  def fillFastUnsafe(nodes: AdjacencyList[NodeData, EdgeData]): this.type = {
    nodeDatas.clearAndShrink(nodes.size)
    for (i <- nodes.indices) {
      nodeDatas += Node(
        nodes(i)._1,
        nodes(i)._2.map { case (ed, nd) => Edge(ed, i, nodes.indexWhere(n => n._1 == nd)) }
      )
      nodeDataToID += nodes(i)._1 -> i
    }
    this
  }

  def addNode(node: NodeData): Unit = {
    nodeDatas += Node(node, Seq())
    nodeDataToID += node -> (nodeDatas.size - 1)
  }

  def addEge(from: NodeData, to: NodeData, edge: EdgeData): Unit = {
    val fromId = nodeId(from)
    val toId = nodeId(to)
    addEdgeById(fromId, toId, edge)
    if (bidirectional) {
      addEdgeById(toId, fromId, edge)
    }
  }

  protected def addEdgeById(fromId: NodeId, toId: NodeId, edge: EdgeData): Unit = {
    val old = nodeDatas(fromId)
    nodeDatas(fromId) = old.copy(outEdges = old.outEdges :+ Edge(edge, fromId, toId))
  }

}



