package utils.math.planar.algo.straightSkeleton


import java.util
import java.util.Collections
import java.util.Comparator
import utils.math.space.V3
import utils.math.space.V3
import org.twak.utils.Pair
import org.twak.utils.Triple
import org.twak.utils.collections.ConsecutivePairs
import org.twak.utils.collections.ConsecutiveTriples
import org.twak.utils.geom.Ray3d
import org.twak.utils.geom.LinearForm3D


/**
 * A bunch of faces that collide at one point
 *
 * @author twak
 */
object CoSitedCollision {
  def buildChain2(start: Corner, input: Set[Corner]) = {
    val chain = new util.ArrayList[_]
    // check backwards
    var a = start
    while (input.contains(a)) {
      chain.add(0, a)
      input.remove(a)
      a = a.prevC
    }
    // check forwards
    a = start.nextC
    while (input.contains(a)) {
      chain.add(a)
      input.remove(a)
      a = a.nextC
    }
    new Chain(chain)
  } // start.nextL  start.prevL

  /**
   * Defines order by the angle the first corner in a chain makes with the second
   * against a fixed axis at a specified height.
   */
   val Y_UP = new Vector3d(0, 1, 0)
}
class CoSitedCollision(var loc: V3, ec: EdgeCollision, private var parent: HeightCollision) {
  add(ec)
  var edges = new util.LinkedHashSet[_]
  var debugHoriz = false
  var chains = new util.ArrayList[_]
  def add(ec: EdgeCollision): Unit = {
    edges.add(ec)
  }
  /**
   * New routine
   *
   * @return true if valid chains found at this site
   */
  def findChains(skel: Skeleton): Boolean = {
    chains = new util.ArrayList[_]
    // remove duplicate edges
    val allEdges = new util.LinkedHashSet[_]

    for (ec <- edges) {
      allEdges.add(ec.a)
      allEdges.add(ec.b)
      allEdges.add(ec.c)
    }
    val eit = allEdges.iterator
    while (eit.hasNext) if (!skel.liveEdges.contains(eit.next)) eit.remove()
    if (allEdges.size < 3) return false
    val edgeStarts = new util.LinkedHashSet[Corner]

    for (e <- allEdges) {

      for (c <- e.currentCorners) {
        if (c.nextL eq e) if (!skel.preserveParallel || EdgeCollision.bisectorsBound(c, loc, skel)) edgeStarts.add(c)
      }
    }
    while (!edgeStarts.isEmpty) {
      val start = edgeStarts.iterator.next
      val chain = CoSitedCollision.buildChain2(start, edgeStarts)
      if (chain != null) chains.add(chain)
    }
    edgeStarts.clear()

    for (c <- chains) {
      if (c.chain.size > 1) edgeStarts.addAll(c.chain)
    }
    val chit = chains.iterator
    while (chit.hasNext) {
      val chain = chit.next
      if (chain.chain.size == 1) {
        // first corner of edge is not necessarily the corner of the edge segment bounding the collision
        val s = chain.chain.get(0)
        val found = EdgeCollision.findCorner(s.nextL, loc, skel)
        //                if (found != null && !edgeStarts.contains( found ))
        // fixme: because we (strangely) add all the chain starts above, we can just check if it's unchanged...
        if ((found eq s) && !edgeStarts.contains(found)) {
        }
        else chit.remove()
      }
    }
    // while still no-horizontals in chains (there may be when dealing with multiple
    // sites at one height), process chains to a counter clockwise order
    if (chains.size > 1) Collections.sort(chains, new CoSitedCollision#ChainComparator(loc.z)) // size == 1 may have parallels in it (run away!)

    true
  }
  /**
   * If another collision has been evaluated at teh same height, this method
   * checks for any changes in the Corners involved in a skeleton. This is a problem
   * when several collisions at the same height occur against one smash edge.
   *
   * If the chain has length > 1, then the lost corner can be recorvered using
   * the following corner (the corners central to the collision, every one after
   * the first will all remain valid at one height).
   *
   * If the chain has length 1, we're in for a bit of a trek.
   *
   * We can skip the finding edges part if the current height only has one
   * collision?
   *
   * @param skel
   */
  def validateChains(skel: Skeleton): Unit = {
    // in case an edge has been removed by a previous event at this height

    for (c <- chains) {
      if (c.loop) {
        continue //todo: continue is not supported
        // nothing to do here
      }
      if (c.chain.size > 1) // previous
      {
        c.chain.remove(0)
        c.chain.add(0, c.chain.get(0).prevC)
      }
      else {
        /**
         * This covers the "corner situation" where a concave angle creates creates
         * another loop with it's point closer to the target, than the original loop.
         *
         * It may well break down with lots of adjacent sides.
         */
        val s = c.chain.get(0)
        val e = s.nextL
        val projectionLine = new Ray3d(loc, e.direction)
        val ceiling = new LinearForm3D(0, 0, 1, -loc.z)
        // project start onto line of collisions above smash edge
        try {
          var start: V3 = null
          if (e.uphill == s.prevL.uphill) start = ceiling.collide(e.start, e.uphill)
          else start = e.linearForm.collide(s.prevL.linearForm, ceiling)
          // line defined using collision point, so we're finding the line before 0
          val targetParam = 0
          // we should only end with start if it hasn't been elevated yet
          var bestPrev = s
          // ignore points before start (but allow the first point to override start!)
          var bestParam = projectionLine.projectParam(start) - 0.001
          // parameterize each corner in e's currentCorners by the line

          for (r <- e.currentCorners) {
            if (r.nextL eq e) {
              // parameterize
              val rOnHigh = if (Math.abs(r.z - loc.z) < 0.001) r
              else ceiling.collide(r.prevL.linearForm, r.nextL.linearForm)
              val param = projectionLine.projectParam(rOnHigh)
              // if this was the previous (todo: does this want a tolerance on < targetParam? why not?)
              if (param > bestParam && param <= targetParam) {
                bestPrev = r
                bestParam = param
              }
            }
          }
          c.chain.remove(0)
          c.chain.add(0, bestPrev)
          // might have formed a loop
          c.loop = c.chain.get(c.chain.size - 1).nextC eq c.chain.get(0)
        } catch {
          case t: Throwable =>
            t.printStackTrace()
            //                    System.err.println( "didn't like colliding " + e + "and " + s.prevL );
            continue //todo: continue is not supported

        }
      } // smash edge, search for correct corner (edges not in liveEdges removed next)

    }
    val edgeToCorner = new util.LinkedHashMap[Edge, Corner]

    for (cc <- chains) {

      for (c <- cc.chain) {
        edgeToCorner.put(c.nextL, c)
      }
    }
    // Find valid triples ~ now topology is as it will be before evaluation, we
    // can check that the input edge triplets still have two consecutive edges.
    val validEdges = new util.LinkedHashSet[Edge]

    for (ec <- edges) {
      // todo: adjacent pairs may not be parallel!
      if (hasAdjacent(edgeToCorner.get(ec.a), edgeToCorner.get(ec.b), edgeToCorner.get(ec.c))) if (skel.liveEdges.contains(ec.a) && skel.liveEdges.contains(ec.b) && skel.liveEdges.contains(ec.c)) {
        validEdges.add(ec.a)
        validEdges.add(ec.b)
        validEdges.add(ec.c)
      }
    }
    val chainOrder = new util.ArrayList[Chain](chains)
    // remove parts of chains that aren't a valid triple.

    for (cc <- chainOrder) {
      // remove and split
      chains.addAll(chains.indexOf(cc), cc.removeCornersWithoutEdges(validEdges))
    }
    // kill 0-length chains
    val ccit = chains.iterator
    while (ccit.hasNext) if (ccit.next.chain.size == 0) ccit.remove()
  }
  private def hasAdjacent(a: Corner, b: Corner, c: Corner): Boolean = {
    if (a == null || b == null || c == null) return false
    if ((a.nextC eq b) || (a.nextC eq c)) return true // todo: speedup by puting consec in a,b always?

    if ((b.nextC eq c) || (b.nextC eq a)) return true
    if ((c.nextC eq a) || (c.nextC eq b)) return true
    false
  }
  def processChains(skel: Skeleton): Boolean = {
    if (moreOneSmashEdge) return false // no test example case showing this is required?

    val allCorners = new util.LinkedHashSet[_]

    for (cc <- chains) {
      allCorners.addAll(cc.chain) //cc.chain.get(0).nextL.currentCorners

    }
    // after all the checks, if there are less than three faces involved, it's not a collision any more
    if (allCorners.size < 3) return false
    skel.debugCollisionOrder.add(this)
    val cit = chains.iterator
    while (cit.hasNext) {
      val chain = cit.next // chain.chain.get(2).nextL


      for (p <- new ConsecutivePairs[Corner](chain.chain, chain.loop)) {
        //                System.out.println( "proc consec " + p.first() + " and " + p.second() );
        EdgeCollision.processConsecutive(loc, p.first, p.second, skel)
      }
      // remove the middle faces in the loop from the list of live corners, liveEdges if
      // there are no more live corners, and the liveCorners list
      if (chain.chain.size >= 3) {
        val tit = new ConsecutiveTriples[Corner](chain.chain, chain.loop)
        while (tit.hasNext) {
          val middle = tit.next.second.nextL
          // face no longer referenced, remove from list of live edges
          if (middle.currentCorners.isEmpty) skel.liveEdges.remove(middle)
        }
      }
      if (chain.loop) cit.remove()
    }
    // was entirely closed loops
    if (chains.isEmpty) return true
    // connect end of previous chain, to start of next
    // in case we are colliding against a smash (no-corner/split event)-edge, we cache the next-corner before
    // any alterations
    val aNext = new util.LinkedHashMap[_, _]

    for (chain <- chains) {
      val c = chain.chain.get(chain.chain.size - 1)
      aNext.put(c, c.nextC)
    }
    // process intra-chain collisions (non-consecutive edges)

    for (adjacentChains <- new ConsecutivePairs[Chain](chains, true)) {
      val first = adjacentChains.first.chain
      val a = first.get(first.size - 1)
      val b = adjacentChains.second.chain.get(0)
      EdgeCollision.processJump(loc, a, aNext.get(a), b, skel, parent)
    }
    true
  }
  /**
   * Is this actually needed in any of the examples?
   */
  private def moreOneSmashEdge: Boolean = {
    // if two chains have length one, this is not a valid collision point
    var oneCount = 0

    for (ch <- chains) {
      if (ch.chain.size == 1) oneCount += 1
    }
    if (oneCount > 1) return false
    oneCount > 1
  }
  class ChainComparator( var height: Double) //        LinearForm3D ceiling;
    extends Comparator[Chain] {
    //            this.ceiling = new LinearForm3D( 0, 0, 1, -height );
    override def compare(o1: Chain, o2: Chain) = {
      val c1 = o1.chain.get(0)
      val c2 = o2.chain.get(0)
      // except for the first and and last point
      // chain's non-start/end points are always at the position of the collision - so to
      // find the angle of the first edge at the specified height, we project the edge before start
      // coordinate the desired height and take the angle relative to the collision
      // !could speed up with a chain-class that caches this info!
      //            try
      //            {
      val p1 = Edge.collide(c1, height) //ceiling.collide( c1.prevL.linearForm, c1.nextL.linearForm );

      val p2 = Edge.collide(c2, height) //ceiling.collide( c2.prevL.linearForm, c2.nextL.linearForm );

      p1.sub(loc)
      p2.sub(loc)
      // start/end line is (+-)Pi
      Double.compare(Math.atan2(p1.y, p1.x), Math.atan2(p2.y, p2.x))
      //            }
      //            catch (RuntimeException e)
      //            {
      //                // we can probably fix these up (by assuming that they're horizontal?)
      //                // todo: can we prove they are safe to ignore? eg: no parallel edges inbound, none outbound etc..
      //                 System.err.println( "didn't like colliding 1" + c1.prevL + " and " + c1.nextL );
      //                 System.err.println( "                      2" + c2.prevL + " and " + c2.nextL );
      //                 return 0;
      //            }
    }
  }
  def getHeight = loc.z
  override def toString = {
    val sb = new StringBuilder("{")

    for (e <- edges) {
      sb.append(e + ",")
    }
    sb.append("}")
    sb.toString
  }
}

