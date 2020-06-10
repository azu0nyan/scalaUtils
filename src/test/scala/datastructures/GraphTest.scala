package datastructures

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._

import utils.datastructures.IntV2
import utils.datastructures.graph.{ArrayBufferGraph, GraphGenUtils}
class GraphTest extends AnyFunSuite {



  test("Array Buffer Graph traversal"){
    val nodes = (0 until 10) toSeq
    val g:ArrayBufferGraph[Int, Unit] = new ArrayBufferGraph[Int,Unit]().fillFastUnsafe(GraphGenUtils.fullGraph(nodes))
    for(i<- nodes){
      val bft = g.bft(i).toSet
      assert(bft == nodes.toSet) withClue s"bft failed traversal from $i traversed ${bft} should travese ${nodes.toSet}"
      val dft = g.dft(i).toSet
      assert(dft == nodes.toSet) withClue s"dft failed traversal from $i traversed ${dft} should travese ${nodes.toSet}"
    }
  }

  test("Graph find path"){
    val nodes = (0 until 10) toSeq
    val g:ArrayBufferGraph[Int, Unit] = new ArrayBufferGraph[Int,Unit]().fillFastUnsafe(GraphGenUtils.fullGraph(nodes))
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
    val g:ArrayBufferGraph[Int, Unit] = new ArrayBufferGraph[Int,Unit]().fillFastUnsafe(GraphGenUtils.fullGraph(nodes))
    println(GraphGenUtils.fullGraph(nodes))
    assert(g.shortestPath(0, 2).nonEmpty)
  }


}
