package utils.math.planar.algo.straightSkeleton


import utils.math.space.V3
import org.twak.camp.debug.DebugDevice
import org.twak.utils.Pair
import org.twak.utils.collections.CloneConfirmIterator
import org.twak.utils.collections.ConsecutivePairs
import org.twak.utils.geom.LinearForm3D

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}


/**
 * A bunch of faces that collide at the same height
 *
 * * @param coHeighted .size() > 1
 */
class HeightCollision(
                       val coHeighted: mutable.Buffer[EdgeCollision]
                     ) extends HeightEvent {
  var height = coHeighted(0).getHeight
  val newHoriz = new mutable.LinkedHashSet[Corner]

  override def getHeight = height
  /**
   * This is a bit of quest!
   *
   * Assumption is that there are no parallel edges creating horizontal bisectors
   * in the current loops. We create some here, then process them all, again removing
   * all horizontal bisectors from the current loops.
   *
   * @return true if topology has changed, false (we ignored all events)
   */
  override def process(skel: Skeleton) = {
    var changed = false
    var coSited = mutable.Buffer[CoSitedCollision]()

    // I love the smell of O(n^2) in the morning
    for (ec <- coHeighted) {

      var shouldAdd = false

      breakable {
        for (csc <- coSited) {
          if (ec.loc.distance(csc.loc) < 0.01) {
            csc.add(ec)
            shouldAdd = true
            break
          }
        }
      }
      if (shouldAdd) coSited += new CoSitedCollision(ec.loc, ec, this)
    }

    /**
     * todo: This is a two-step process, for (I suspect) historical
     * reasons. It should be possible to find the chains as we
     * go using line-projection.
     */
    //    var cit = coSited.iterator
    //    while (cit.hasNext) {
    //      val css = cit.next
    //      if (!css.findChains(skel)) cit.remove()
    //    }

    coSited = coSited.filter(_.findChains(skel)) //TODO do it in place

    /**
     * We don't remove any points as it merges faces. All the
     * information (chains etc..) contains references to the
     * faces that we don't want destroyed as the faces merge.
     */
    skel.qu.holdRemoves()
    val cit = coSited.iterator
    while (cit.hasNext) {
      val css = cit.next
      css.validateChains(skel)
      changed |= css.processChains(skel)
    }
    skel.qu.resumeRemoves()
    processHoriz(skel)
    changed
  }

  /**
   * Process horizontals.
   *
   * assumes that all corners with horizontal bisectors are at the same height(?), and
   * that all faces that need to be merged, have been.
   *
   * @param skel
   */
  def processHoriz(skel: Skeleton): Unit = {
    val chains = new mutable.LinkedHashSet[Chain]
    while (newHoriz.nonEmpty)
      chains.add(CoSitedCollision.buildChain2(newHoriz.iterator.next, newHoriz))
    if (chains.nonEmpty) {
      // if there are two lines of events at the same hight (but different lines), we need them to share their end points.
      val intraface = new mutable.LinkedHashSet[Chain]

      for (chain <- chains) {
        //            if (chain.chain.isEmpty())
        //                continue;
        val priority = mutable.Buffer[Edge]

        for (c <- chain.chain) {
          // both edges are parallel - these are the only corners added to newHoriz...
          priority.add(c.nextL)
          priority.add(c.prevL)
        }
        // find a set of coplanar edges that survive this transition in winners (equal highest priority)
        val hComp = skel.getHorizontalComparator
        Collections.sort(priority, hComp)
        val winners = new LinkedHashSet[_]
        val winner = priority.remove(0)
        winners.add(winner)
        while (!priority.isEmpty && hComp.compare(winner, priority.get(0)) == 0) winners.add(priority.remove(0))
        // if first edge needs an additional corner - "if we're adding a cap at the start" and "first isn't already an interface"
        var first = chain.chain.get(0)
        if (!winners.contains(first.prevL)) //skel.liveCorners.contains(first)
          if (!intraface.contains(first.prevC)) // hasn't already been raised up by a previous chain
          {
            //                    V3 res =//new LinearForm3D( 0, 0, 1, -first.z ).collide( first.prevL.linearForm, first.prevC.prevL.linearForm );
            val newFirst = new Corner(Edge.collide(first.prevC, first.z))
            skel.output.addOutputSideTo(first.prevC, newFirst, first.prevL, first.prevC.prevL)
            Corner.replace(first.prevC, newFirst, skel)
            chain.chain.add(0, newFirst)
            intraface.add(newFirst)
            first = newFirst
          }
          else chain.chain.add(0, first.prevC)
        else {
          // the edge before the first point is a winner, add it
          chain.chain.add(0, first = first.prevC)
        }
        var last = chain.chain.get(chain.chain.size - 1)
        // if last edge needs an additional corner
        if (!winners.contains(last.nextL)) if (!intraface.contains(last.nextC)) // hasn't already been raised up by a previous chain
        {
          //                    V3 res = new LinearForm3D( 0, 0, 1, -last.z ).collide( last.nextL.linearForm, last.nextC.nextL.linearForm );
          val newLast = new Corner(Edge.collide(last.nextC, last.z))
          skel.output.addOutputSideTo(last.nextC, newLast, last.nextL, last.nextC.nextL)
          Corner.replace(last.nextC, newLast, skel)
          chain.chain.add(newLast)
          intraface.add(newLast)
          last = newLast
        }
        else chain.chain.add(last.nextC)
        else {
          // the edge after the last point is a winner, add it
          chain.chain.add(last = last.nextC)
        }

        for (pair <- new ConsecutivePairs[Corner](chain.chain, false)) {
          val s = pair.first
          val e = pair.second
          assert(s.nextL eq e.prevL)
          // if this is the edge that spreads out over all others
          if (winners.contains(s.nextL)) {
            if (s.nextL ne winner) skel.output.merge(winner.start, s) // assumes start of edge forms part of it's output
            s.nextL.currentCorners.remove(e)
            s.nextL.currentCorners.remove(s)
          }
          else {
            // this (section of this ) edge ends at this height
            s.nextL.currentCorners.remove(s)
            s.nextL.currentCorners.remove(e)
            skel.output.addOutputSideTo(true, s, e, s.nextL, winner)
          }
          skel.liveCorners.remove(s) // add in first and last below

          skel.liveCorners.remove(e)
        }
        skel.liveCorners.add(first)
        skel.liveCorners.add(last)
        winner.currentCorners.add(first)
        winner.currentCorners.add(last)
        first.nextC = last
        last.prevC = first
        first.nextL = winner
        last.prevL = winner

        for (c <- chain.chain) {
          if (c.nextL.currentCorners.size == 0) skel.liveEdges.remove(c.nextL)
        }
      }
      // no need to recalculate events - no faces added. wrong!- any new connectivity needs to be flagged as loop-of-two etc...
      skel.qu.clearFaceEvents()

      for (lc <- new CloneConfirmIterator[Corner](skel.liveCorners)) {
        skel.qu.addCorner(lc, this)
      }
      // can't think of a case wher ethis could happen. could iterate in...
      assert(newHoriz.size == 0)
      skel.validate()
    }
  }
  def newHoriz(toAdd: Corner): Unit = {
    newHoriz.add(toAdd)
  }
  override def toString = "collisions at " + height
}

