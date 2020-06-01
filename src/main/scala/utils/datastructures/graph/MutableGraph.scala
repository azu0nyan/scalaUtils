package utils.datastructures.graph

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.collection.mutable.ArrayBuffer

object MutableGraph {

  /** NodeData/EdgeData used to refer to node for ex. removal, a swell as storing data */
  class MutableGraph[NodeData, EdgeData]() {
    //Node and Edge

    type E = Int

    var nodeId = new AtomicLong()
    var edgeId = new AtomicLong()



//    case class GraphSnapshot(nodes:IndexedSeq[Node] = IndexedSeq()){
//
//    }
//
//    var snapshot:GraphSnapshot = GraphSnapshot()
//
    }



}
