package utils.datastructures.graph

import utils.datastructures.graph.Graph._

object GraphGenUtils {
  def fullGraph[ND](nodes: Seq[ND], loops: Boolean = false): AdjacencyList[ND, Unit] =
    nodes.map(n =>
      (n, //node
        (if (loops) nodes else nodes.filterNot(_ == n))
          .map(to => ((), to)) //edge
      )
    )

}
