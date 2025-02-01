package utils.math.planar.algo.straightSkeleton


import utils.math.Scalar
import utils.math.planar.algo.straightSkeleton.implhelpers.ConsecutivePairs
import utils.math.planar.algo.straightSkeleton.math.{LinearForm3D, Ray3d}
import utils.math.space.V3

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * A bunch of faces that collide at one point
 *
 * @author twak
 */

class CoSitedCollision(val loc: V3, ec: EdgeCollision, private var parent: HeightCollision) {
  var edges = new mutable.LinkedHashSet[EdgeCollision]
  var debugHoriz = false
  var chains = new ArrayBuffer[Chain]()

  add(ec)

  def add(ec: EdgeCollision): Unit = {
    edges.add(ec)
  }

  /**
   * New routine
   *
   * @return true if valid chains found at this site
   */
  def findChains(skel: Skeleton): Boolean = {
    chains = new ArrayBuffer[Chain]()
    if(loc == V3(450.0, -50.0, 50.00000000000001)) {
//      println()
    }
    // remove duplicate edges
    val allEdges = new mutable.LinkedHashSet[Edge]

    for (ec <- edges) {
      allEdges.add(ec.a)
      allEdges.add(ec.b)
      allEdges.add(ec.c)
    }

    allEdges.filterInPlace(skel.liveEdges.contains)

    if (allEdges.size < 3)
      false
    else {
      val edgeStarts = new mutable.LinkedHashSet[Corner]

      for (e <- allEdges) {
        for (c <- e.currentCorners) {
          if (c.nextL eq e)
            if (!skel.preserveParallel || EdgeCollision.bisectorsBound(c, loc, skel))
              edgeStarts.add(c)
        }
      }
      while (edgeStarts.nonEmpty) {
        val start = edgeStarts.iterator.next
        //!!!!drains from edgeStarts
        val chain = CoSitedCollision.buildChain2(start, edgeStarts)
        chains += chain
      }
      edgeStarts.clear()

      for (c <- chains) {
        if (c.chain.size > 1) edgeStarts.addAll(c.chain)
      }

      chains.filterInPlace { chain =>
        if (chain.chain.size == 1) {
          // first corner of edge is not necessarily the corner of the edge segment bounding the collision
          val s = chain.chain.head
          val found = EdgeCollision.findCorner(s.nextL, loc, skel)
          //                if (found != null && !edgeStarts.contains( found ))
          // fixme: because we (strangely) add all the chain starts above, we can just check if it's unchanged...
          if ((found eq s) && !edgeStarts.contains(found)) {
            //                    chain.chain.clear();
            //                    chain.chain.add( found );
            true
          } else false
        } else true
      }
      // while still no-horizontals in chains (there may be when dealing with multiple
      // sites at one height), process chains to a counter clockwise order
      if (chains.size > 1)
        // size == 1 may have parallels in it (run away!)
        chains.sortInPlace()(chainOrdering(loc.z))
      true
    }
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
        // continue
        // nothing to do here
      } else {
        if (c.chain.size > 1) // previous
        {
          c.chain.remove(0)
          c.chain.insert(0, c.chain.head.prevC)
        }
        else {
          /**
           * This covers the "corner situation" where a concave angle creates creates
           * another loop with it's point closer to the target, than the original loop.
           *
           * It may well break down with lots of adjacent sides.
           */
          val s = c.chain.head
          val e = s.nextL
          val projectionLine = new Ray3d(loc, e.direction)
          val ceiling = new LinearForm3D(0, 0, 1, -loc.z)
          // project start onto line of collisions above smash edge
          try {
            val start =
              if (e.uphill == s.prevL.uphill) ceiling.collide(e.start.asV3, e.uphill)
              else e.linearForm.collide(s.prevL.linearForm, ceiling)

            start match
              case Some(start) => // line defined using collision point, so we're finding the line before 0
                val targetParam = 0
                // we should only end with start if it hasn't been elevated yet
                var bestPrev = s
                // ignore points before start (but allow the first point to override start!)
                var bestParam = projectionLine.projectParam(start) - 0.001
                // parameterize each corner in e's currentCorners by the line

                for (r <- e.currentCorners) {
                  if (r.nextL eq e) {
                    // parameterize
                    val rOnHigh = if (Math.abs(r.z - loc.z) < 0.001) Some(r.asV3)
                    else ceiling.collide(r.prevL.linearForm, r.nextL.linearForm)
                    val param = projectionLine.projectParam(rOnHigh.get) // todo refactor out logic on throwables
                    // if this was the previous (todo: does this want a tolerance on < targetParam? why not?)
                    if (param > bestParam && param <= targetParam) {
                      bestPrev = r
                      bestParam = param
                    }
                  }
                }
                c.chain.remove(0)
                c.chain.insert(0, bestPrev)
                // might have formed a loop
                c.loop = c.chain.last.nextC eq c.chain.head
              case None =>

          } catch {
            case t: Throwable =>
              t.printStackTrace()
            //                    System.err.println( "didn't like colliding " + e + "and " + s.prevL );
          }
        } // smash edge, search for correct corner (edges not in liveEdges removed next)
      }
    }
    val edgeToCorner = new mutable.LinkedHashMap[Edge, Corner]

    for (cc <- chains) {

      for (c <- cc.chain) {
        edgeToCorner.put(c.nextL, c)
      }
    }
    // Find valid triples ~ now topology is as it will be before evaluation, we
    // can check that the input edge triplets still have two consecutive edges.
    val validEdges = new mutable.LinkedHashSet[Edge]

//    println("Edges" + edges)
    for (ec <- edges) {
      // todo: adjacent pairs may not be parallel!
      if (hasAdjacent(edgeToCorner.get(ec.a), edgeToCorner.get(ec.b), edgeToCorner.get(ec.c)))
        if (skel.liveEdges.contains(ec.a) && skel.liveEdges.contains(ec.b) && skel.liveEdges.contains(ec.c)) {
          validEdges.add(ec.a)
          validEdges.add(ec.b)
          validEdges.add(ec.c)
        }
    }
    val chainOrder = mutable.Buffer[Chain](chains.toSeq *)
    // remove parts of chains that aren't a valid triple.

    for (cc <- chainOrder) {
      // remove and split
      chains.insertAll(chains.indexOf(cc), cc.removeCornersWithoutEdges(validEdges).iterator)
    }
    // kill 0-length chains
    chains.filterInPlace(_.chain.nonEmpty)
  }

  private def hasAdjacent(a: Option[Corner], b: Option[Corner], c: Option[Corner]): Boolean =
    if (a.isEmpty || b.isEmpty || c.isEmpty) false
    else if ((a.get.nextC eq b.get) || (a.get.nextC eq c.get)) true // todo: speedup by puting consec in a,b always?
    else if ((b.get.nextC eq c.get) || (b.get.nextC eq a.get)) true
    else if ((c.get.nextC eq a.get) || (c.get.nextC eq b.get)) true
    else false


  def processChains(skel: Skeleton): Boolean = {
    if (moreOneSmashEdge)
      false // no test example case showing this is required?
    else {
      val allCorners = new mutable.LinkedHashSet[Corner]

      for (cc <- chains) {
        allCorners.addAll(cc.chain) //cc.chain.get(0).nextL.currentCorners

      }
      // after all the checks, if there are less than three faces involved, it's not a collision any more
      if (allCorners.size < 3) {
        false
      }
      else {
        skel.debugCollisionOrder += this


        chains.foreach(processChain(_, skel))

        chains = chains.filterNot(_.loop)

        // was entirely closed loops
        if (chains.isEmpty) true
        else {
          // connect end of previous chain, to start of next
          // in case we are colliding against a smash (no-corner/split event)-edge, we cache the next-corner before
          // any alterations
          val aNext = new mutable.LinkedHashMap[Corner, Corner]

          for (chain <- chains) {
            val c = chain.chain.last
            aNext.put(c, c.nextC)
          }
          // process intra-chain collisions (non-consecutive edges)

          for (adjacentChains <- new ConsecutivePairs[Chain](chains, true)) {
            val first = adjacentChains._1.chain
            val a = first.last
            val b = adjacentChains._2.chain.head
            EdgeCollision.processJump(loc, a, aNext(a), b, skel, parent)
          }
          true
        }
      }
    }
  }

  def processChain(chain: Chain, skel: Skeleton) = {
    for (p <- new ConsecutivePairs[Corner](chain.chain, chain.loop)) {
      //                System.out.println( "proc consec " + p.first() + " and " + p.second() );
      EdgeCollision.processConsecutive(loc, p._1, p._2, skel)
    }
    // remove the middle faces in the loop from the list of live corners, liveEdges if
    // there are no more live corners, and the liveCorners list
    if (chain.chain.size >= 3) {
      val tit = new ConsecutiveTriples[Corner](chain.chain, chain.loop)
      while (tit.hasNext) {
        val middle = tit.next._2.nextL
        // face no longer referenced, remove from list of live edges
        if (middle.currentCorners.isEmpty)
          skel.liveEdges.remove(middle)
      }
    }
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


  def chainOrdering(height: Scalar) =
    new Ordering[Chain] {
      override def compare(o1: Chain, o2: Chain) = {
        val c1 = o1.chain.head
        val c2 = o2.chain.head
        // except for the first and and last point
        // chain's non-start/end points are always at the position of the collision - so to
        // find the angle of the first edge at the specified height, we project the edge before start
        // coordinate the desired height and take the angle relative to the collision
        // !could speed up with a chain-class that caches this info!
        //            try
        //            {
        val p1 = Edge.collide(c1, height) - loc //ceiling.collide( c1.prevL.linearForm, c1.nextL.linearForm );

        val p2 = Edge.collide(c2, height) - loc //ceiling.collide( c2.prevL.linearForm, c2.nextL.linearForm );

        // start/end line is (+-)Pi
        Math.atan2(p1.y, p1.x).compare(Math.atan2(p2.y, p2.x))
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
  override def toString = edges.mkString("{", ",", "}")
}


object CoSitedCollision {
  /** removes elements from Set */
  def buildChain2(start: Corner, input: mutable.Set[Corner]) = {
    val chain = mutable.Buffer[Corner]()
    // check backwards
    var a = start
    while (input.contains(a)) {
      chain.insert(0, a)
      input.remove(a)
      a = a.prevC
    }
    // check forwards
    a = start.nextC
    while (input.contains(a)) {
      chain += a
      input.remove(a)
      a = a.nextC
    }
    new Chain(chain)
  } // start.nextL  start.prevL

  /**
   * Defines order by the angle the first corner in a chain makes with the second
   * against a fixed axis at a specified height.
   */
  val Y_UP = new V3(0, 1, 0)
}