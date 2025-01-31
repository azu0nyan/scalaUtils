package utils.datastructures.graph

import utils.datastructures.CircullarOps.*
import utils.datastructures.graph.Graph.*

object GraphGenUtils {
  def fullGraph[ND](nodes: Seq[ND], loops: Boolean = false): AdjacencyList[ND, Unit] =
    nodes.map(n =>
      (n, //node
        (if (loops) nodes else nodes.filterNot(_ == n))
          .map(to => ((), to)) //edge
      )
    )

  def cycleGraph[ND, ED](nodes: Seq[(ND, ED)]): AdjacencyList[ND, ED] =
    for (i <- nodes.indices) yield (nodes(i)._1, Seq((nodes(i)._2, nodes(circullarIndex(i + 1, nodes.size))._1)))

}
