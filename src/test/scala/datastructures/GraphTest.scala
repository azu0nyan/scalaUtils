package datastructures

import org.scalatest.funsuite.AnyFunSuite
import utils.datastructures.IntV2
import utils.datastructures.graph.GraphOps.Graph
import utils.datastructures.graph.GraphOps._
import utils.datastructures.graph._

class GraphTest extends AnyFunSuite {

  def grid(dims:IntV2):Graph[IntV2, Unit] = new Graph[IntV2, Unit] {
  override protected [GraphOps] def nodeId(node:  IntV2): NodeId = IntV2.toFlatIndex(node, dims)
  override protected [GraphOps] def nodesCount: Int = dims.area
  override protected [GraphOps] def nodeById(id:  Int): this.Node = Node()
}

}
