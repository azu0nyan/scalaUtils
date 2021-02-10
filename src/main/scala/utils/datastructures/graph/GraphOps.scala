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

  def reachableFrom[ND, ED](g: Graph[ND, ED], vertex: ND): Seq[ND] = {
    g.traversal(vertex).toSeq
  }

  def strongConnectivityComponentsOnDirectedGraph[ND, ED](g: Graph[ND, ED]): Seq[Seq[ND]] = {
    var unvisited:Set[ND] = g.nodes.toSet
    var res: Seq[Seq[ND]] = Seq()
    while (unvisited.nonEmpty) {
      val comp = g.traversal(unvisited.head).toSeq
      res = comp +: res
      unvisited = unvisited &~ comp.toSet
    }
    res
  }

  def fromAdjacencyMatrix[ND, ED](nodes:IndexedSeq[ND], edges:IndexedSeq[IndexedSeq[Option[ED]]]):Graph[ND, ED] = {
    val g = new ArrayBufferGraph[ND, ED](false)
    nodes.foreach(n => g.addNode(n))

    for(i<- edges.indices; j <- edges(i).indices; e <- edges(i)(j)){
      g.addEdge(nodes(i), nodes(j), e)
    }

    g
  }

}
