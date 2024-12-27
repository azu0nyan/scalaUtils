package utils.math.planar.algo.straightSkeleton

import java.util.{List, Set}



import java.util


object EdgeCreator {
  class DefaultEdgeCreator extends EdgeCreator {
    override def getEdges(old: Edge, startH: Corner, endH: Corner) = {
      val out = new ArrayList[_]
      startH.nextL = old
      endH.prevL = old
      //            e.angle = Math.PI / 4;
      //            e.machine = old.machine; //?
      out.add(startH)
      out.add(endH)
      out
    }
    override def getFeaturesFor(edgeH: Edge) = new HashSet[_]
  }
}
trait EdgeCreator {
  /**
   * Returns a list of edges as replacement for the old edge. The old edge
   * extruded to the current height had start startH and end endH.
   *
   * The edges are expected to have valid start and end corners and Machines,
   * all nextC and prevC. All other pointers are derrived.
   *
   * the first corner in the ordered chain must be startH, the last endH
   */
  def getEdges(old: Edge, startH: Corner, endH: Corner): util.List[Corner]
  def getFeaturesFor(edgeH: Edge): util.Set[Tag]
}



