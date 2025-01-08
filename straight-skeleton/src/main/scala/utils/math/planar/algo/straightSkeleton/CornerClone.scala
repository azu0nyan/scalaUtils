package utils.math.planar.algo.straightSkeleton

import utils.datastructures.containers.map.impl.MutableBiMap
import utils.math.planar.algo.straightSkeleton.helpers.{Cache, Loop, LoopL, SetCorrespondence}

/**
 * Clones a new set of corners, edges.
 *
 * Edges machines are not cloned
 *
 * @param input the input to be cloned. Result dumped into variables.
 */
class CornerClone(input: LoopL[Corner]){
  var output = new LoopL[Corner]
  var nOSegments = new SetCorrespondence[Corner, Corner]
  var nOCorner = new MutableBiMap[Corner, Corner]
  
  val cornerCache = new Cache[Corner, Corner]() {
    override def create(i: Corner) = new Corner(i.asV3)
  }

  val edgeCache = new Cache[Edge, Edge]() {
    override def create(i: Edge) = {
      val edge = new Edge(cornerCache.get(i.start), cornerCache.get(i.end))
      edge.setAngle(i.getAngle)
      edge.machine = i.machine // nextL is null when we have a non root global

      //              edge.profileFeatures = new LinkedHashSet<Feature>(current.nextL.profileFeatures);
      //              edgeMap.put( edge, current.nextL );

      for (c <- i.currentCorners) {
        edge.currentCorners.add(cornerCache.get(c))
      }
      edge
    }
  }



  for (inputLoop <- input.iterator) {
    val loop = new Loop[Corner]
    output.add(loop)

    for (current <- inputLoop) {
      val s = cornerCache.get(current)
      val e = cornerCache.get(current.nextC)
      // one edge may have two segments, but the topology will not change between old and new,
      // so we may store the leading corner to match segments
      nOSegments.put(s, current)
      nOCorner.put(s, current)
      val edge = edgeCache.get(current.nextL)
      loop.append(s)
      s.nextC = e
      e.prevC = s
      s.nextL = edge
      e.prevL = edge
    }
  }
  
  def addSegment = true
  def addCorner = true
}

