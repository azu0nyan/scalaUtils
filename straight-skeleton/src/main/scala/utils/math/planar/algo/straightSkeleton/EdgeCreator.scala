package utils.math.planar.algo.straightSkeleton

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object EdgeCreator {
  class DefaultEdgeCreator extends EdgeCreator {
    override def getEdges(old: Edge, startH: Corner, endH: Corner) = {
      val out = new ArrayBuffer[Corner]
      startH.nextL = old
      endH.prevL = old
      //            e.angle = Math.PI / 4;
      //            e.machine = old.machine; //?
      out += startH
      out += endH
      out
    }
    override def getFeaturesFor(edgeH: Edge) = new mutable.HashSet[Tag]
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
  def getEdges(old: Edge, startH: Corner, endH: Corner): mutable.Seq[Corner]
  def getFeaturesFor(edgeH: Edge): mutable.Set[Tag]
}



