package datastructures

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._

import utils.datastructures.IntV2
import utils.datastructures.graph.{ArrayBufferGraph, GraphGenUtils}
class GraphTest extends AnyFunSuite {



  test("Array Buffer Graph traversal"){
    val nodes = (0 until 10) toSeq
    val g:ArrayBufferGraph[Int, Unit] = new ArrayBufferGraph[Int,Unit](false).fillFastUnsafe(GraphGenUtils.fullGraph(nodes))
    for(i<- nodes){
      val bft = g.bft(i).toSet
      assert(bft == nodes.toSet) withClue s"bft failed traversal from $i traversed ${bft} should travese ${nodes.toSet}"
      val dft = g.dft(i).toSet
      assert(dft == nodes.toSet) withClue s"dft failed traversal from $i traversed ${dft} should travese ${nodes.toSet}"
    }
  }

  test("Graph find path"){
    val nodes = (0 until 10) toSeq
    val g:ArrayBufferGraph[Int, Unit] = new ArrayBufferGraph[Int,Unit](false).fillFastUnsafe(GraphGenUtils.fullGraph(nodes))
    for(i<- nodes; j <- nodes){
      if(i != j){
        assert(g.shortestPath(i, j).nonEmpty) withClue s"$i $j empty"
        assert(g.shortestPath(i, j).get.length == 1d)
      } else {
        assert(g.shortestPath(i, i).nonEmpty)
        assert(g.shortestPath(i, i).get.length == 0d)
      }
    }
  }

  test("no path bug"){
    val nodes = (0 until 10) toSeq
    val g:ArrayBufferGraph[Int, Unit] = new ArrayBufferGraph[Int,Unit](false).fillFastUnsafe(GraphGenUtils.fullGraph(nodes))
    println(GraphGenUtils.fullGraph(nodes))
    assert(g.shortestPath(0, 2).nonEmpty)
  }

  test("find path on  cycle 1"){
    val nodesAndEdges = (0 until 10).map(x => (x, 1))
    val g = new ArrayBufferGraph[Int, Int](bidirectional = false).fillFastUnsafe(GraphGenUtils.cycleGraph(nodesAndEdges))
    for(
      i <- 0 until 10;
      j <- 1 to 9){
      val f = i
      val t = (i + j) % 10
      val path = g.shortestPath(f, t)
      assert(path.nonEmpty)
      assert(path.get.length() == j)
      assert(path.get.nodes == (i to (i + j)).map(_ % 10))
    }
  }

  test("find path on weighted graph"){
    val g = new ArrayBufferGraph[Int, Int](bidirectional = false)
    g.addNode(0)
    g.addNode(1)
    g.addNode(2)
    g.addNode(3)
    g.addNode(4)
    g.addNode(5)
    //0 3 6 (5)
    g.addEge(0, 1, 2)
    g.addEge(1, 2, 4)
    g.addEge(0, 3, 6)
    g.addEge(0, 3, 4)
    g.addEge(0, 4, 1)
    g.addEge(3, 5, 1)
    g.addEge(0, 5, 20)
    g.addEge(4, 0, 1)
    assert(g.shortestPath(0, 5, pathCost = x => x).get.nodes == Seq(0, 3, 5))
    assert(g.shortestPath(0, 5, pathCost = x => x).get.length(x => x) == 5)
  }




}
