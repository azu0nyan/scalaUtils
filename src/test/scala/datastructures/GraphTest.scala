package datastructures

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._

import utils.datastructures.IntV2
import utils.datastructures.graph.{ArrayBufferGraph, GraphGenUtils}
class GraphTest extends AnyFunSuite {



  test("Array Buffer Graph traversal"){
    val nodes = (0 until 10) toSeq
    val g:ArrayBufferGraph[Int, Unit] = new ArrayBufferGraph[Int,Unit](GraphGenUtils.fullGraph(nodes))
    for(i<- nodes){
      val bft = g.bft(i).toSet
      assert(bft == nodes.toSet) withClue s"bft failed traversal from $i travesed ${bft} should travese ${nodes.toSet}"
      val dft = g.dft(i).toSet
      assert(bft == nodes.toSet) withClue s"dft failed traversal from $i travesed ${bft} should travese ${nodes.toSet}"
    }
  }

}
