package utils.datastructures.graph

import utils.datastructures.IntV2
import utils.datastructures.graph.Graph.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class ArrayBufferGraph[NodeData, EdgeData](
//                                            val bidirectional: Boolean
                                          ) extends Graph[NodeData, EdgeData] {


  //Interface impl
  val nodeDatas: ArrayBuffer[Node] = ArrayBuffer()

  /**nodeid -> id of nodes having edges directed to given*/
  protected val nodesWithEdgesToMe:ArrayBuffer[ArrayBuffer[NodeId]] = ArrayBuffer()

  override  def nodeIds: Iterator[NodeId] = nodeDatas.indices.iterator

  @inline override  def nodeById(id: Int): Node = nodeDatas(id)

  protected val nodeDataToID: mutable.Map[NodeData, NodeId] = mutable.Map()



  override  def nodeId(node: NodeData): NodeId = nodeDataToID.getOrElse(node, -1)

  def getOrElseAddNodeId(node: NodeData): NodeId = {
    val id = nodeId(node)
    if(id < 0) addNode(node)
    else id
  }

  //inits
  /** ignores 'bidirectional' flag */
  def fillFastUnsafe(nodes: AdjacencyList[NodeData, EdgeData]): this.type = {
    nodeDatas.clearAndShrink(nodes.size)
    for (i <- nodes.indices) {
      nodeDatas += Node(
        nodes(i)._1,
        nodes(i)._2.map { case (ed, nd) => Edge(ed, i, nodes.indexWhere(n => n._1 == nd)) }
      )
      nodesWithEdgesToMe += ArrayBuffer()
      nodeDataToID += nodes(i)._1 -> i
    }
    this
  }

  def addNode(node: NodeData): NodeId = {
    nodeDatas += Node(node, Seq())
    nodesWithEdgesToMe += ArrayBuffer()
    val nodeId = nodeDatas.size - 1
    nodeDataToID += node -> nodeId
    nodeId
  }




  def addEdge(from: NodeData, to: NodeData, edge: EdgeData): Unit = {
    val fromId = getOrElseAddNodeId(from)
    val toId = getOrElseAddNodeId(to)
    addEdgeById(fromId, toId, edge)
  /*  if (bidirectional) {
      addEdgeById(toId, fromId, edge)
    }*/
  }

  def removeEdge(from: NodeData, to: NodeData, edge: EdgeData): Unit = {
    val fromId = nodeId(from)
    val toId = nodeId(to)
    removeEdgeById(fromId, toId, edge)
   /* if (bidirectional) {
      removeEdgeById(toId, fromId, edge)
    }*/
  }


  def addOrUpdateEdge(from: NodeData, to: NodeData, newData: EdgeData):Unit = {
    findEdge(from, to) match {
      case Some(value) =>
        val fromId = getOrElseAddNodeId(from)
        val toId = getOrElseAddNodeId(to)
        val old = nodeDatas(fromId)
        nodeDatas(fromId) = old.copy(outEdges = old.outEdges.map(e => if(e.to == toId) e.copy(data = newData) else e ))
      case None =>
        addEdge(from, to, newData)
    }
  }

  protected def addEdgeById(fromId: NodeId, toId: NodeId, edge: EdgeData): Unit = {
    val old = nodeDatas(fromId)
    nodeDatas(fromId) = old.copy(outEdges = old.outEdges :+ Edge(edge, fromId, toId))
    nodesWithEdgesToMe(toId) += fromId
  }


  protected def removeEdgeById(fromId: NodeId, toId: NodeId, edge: EdgeData): Unit = {
    val old = nodeDatas(fromId)
    nodeDatas(fromId) = old.copy(outEdges = old.outEdges.filter(e => e!= Edge(edge, fromId, toId)))
    nodesWithEdgesToMe(toId) -= fromId
  }

}



