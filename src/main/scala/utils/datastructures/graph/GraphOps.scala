package utils.datastructures.graph

import utils.datastructures.containers.DisjointSet
import utils.datastructures.graph.Graph.Graph
import utils.math.Scalar

object GraphOps {
  def minimumSpanningTree[ND, ED](g: Graph[ND, ED], length: ED => Scalar): Graph[ND, ED] = {
    val res = new ArrayBufferGraph[ND, ED](false)
    val set = new DisjointSet[ND]
    for (v <- g.nodes) {
      set.makeSet(v)
      res.addNode(v)
    }

    for ((f, w, t) <- g.edgesAndNeighbours.toSeq.sortBy(e => length(e._2))) {
      val n1 = set.findSet(f)
      val n3 = set.findSet(t)
      if (n1 != n3) {
        res.addEdge(f, t, w)
        set.union(n1, n3)
      }
    }

    res
  }
}
