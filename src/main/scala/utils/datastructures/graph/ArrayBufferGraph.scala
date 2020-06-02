package utils.datastructures.graph

import utils.datastructures.graph.Graph._

import scala.collection.mutable.ArrayBuffer


class ArrayBufferGraph[NodeData, EdgeData]() extends Graph[NodeData, EdgeData] {
  //Interface impl
  val nodeDatas: ArrayBuffer[Node] = ArrayBuffer()

  override protected def nodeIds: Iterator[NodeId] = nodeDatas.indices.iterator

  @inline override protected def nodeById(id: Int): Node = nodeDatas(id)

  //inits
  def this(nodes: AdjacencyList[NodeData, EdgeData]) = {
    this()
    nodeDatas.clearAndShrink(nodes.size)
    for (i <- nodes.indices) {
      nodeDatas += Node(
        nodes(i)._1,
        nodes(i)._2.map { case (ed, nd) => Edge(ed, i, nodes.indexWhere(n => n._1 == nd)) }
      )
    }
  }
}



