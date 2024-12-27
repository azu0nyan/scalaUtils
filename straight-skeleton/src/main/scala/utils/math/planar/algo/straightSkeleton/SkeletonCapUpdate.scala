package utils.math.planar.algo.straightSkeleton

import java.util.{Iterator, List, Map, Set}

import java.util
import java.util.Collections
import java.util.Comparator
import utils.math.space.V3
import javax.vecmath.Vector3d
import org.twak.camp.debug.DebugDevice
import org.twak.utils.collections.DHash
import org.twak.utils.collections.LoopL
import org.twak.utils.collections.SetCorrespondence
import org.twak.utils.geom.Ray3d


/**
 * Arbitrary topology update of the corners/edges on the sweep plane
 *
 * @author twak
 */
object SkeletonCapUpdate {
  class EdgeInfo( var base: Edge) {
    // corners with their start on the old edge
    //        private Set<Corner>
    //                topSegs = new LinkedHashSet(),
    //                bottomSegs = new LinkedHashSet();
     val segs = new ArrayList[_]
    def addTopSeg(c: Corner): Unit = {
      segs.add(new SkeletonCapUpdate.Segment(c, true, true))
      segs.add(new SkeletonCapUpdate.Segment(c.nextC, true, false))
    }
    def addBottomSeg(c: Corner): Unit = {
      segs.add(new SkeletonCapUpdate.Segment(c, false, true))
      segs.add(new SkeletonCapUpdate.Segment(c.nextC, false, false))
    }
     def sort = {
      Collections.sort(segs, new SkeletonCapUpdate.LineProjectionComparator(base.start, base.end))
      segs
    }
     def addFrom(togo: SkeletonCapUpdate.EdgeInfo): Unit = {
      segs.addAll(togo.segs)
    }
  }
   class Segment ( var corner: Corner,  var top: Boolean, // !bottom
                                             var start: Boolean // !end
                                           ) {
  }
  /**
   * Clones a loop (corner and edges), and returns a map of new to old edges and corners.
   *
   * DEPRICATED: use CornerClone
   *
   */
  //    public static LoopL<Corner>cloneEdges (LoopL<Corner> in, final DHash<Edge, Edge> nOEdge, final DHash<Corner, Corner> nOCorner)
  //    {
  //        final Cache<Corner, Corner> cornerCache = new Cache<Corner, Corner>()
  //        {
  //            @Override
  //            public Corner create( Corner i )
  //            {
  //                Corner out = new Corner( i.x, i.y, i.z );
  //                nOCorner.put( out, i );
  //                return out;
  //            }
  //        };
  //
  //        Cache<Edge, Edge> edgeCache = new Cache<Edge, Edge>()
  //        {
  //            @Override
  //            public Edge create( Edge i )
  //            {
  //                Edge nE = new Edge( cornerCache.get( i.start), cornerCache.get(i.end) );
  //                nOEdge.put( nE, i );
  //
  //                nE.machine = i.machine;
  //                nE.profileFeatures = new HashSet(i.profileFeatures);
  //                nE.setAngle( i.getAngle() );
  //
  //                return nE;
  //            }
  //        };
  //
  //        LoopL<Corner> out = new LoopL();
  //
  //        for ( Loop<Corner> inLoop : in )
  //        {
  //            Loop<Corner> outLoop = new Loop<Corner>();
  //            out.add( outLoop );
  //            for (Corner c : inLoop)
  //            {
  //                Corner nC = cornerCache.get( c );
  //                outLoop.append( nC );
  //                nC.nextL = edgeCache.get( c.nextL );
  //                nC.prevL = edgeCache.get( c.prevL );
  //            }
  //        }
  //
  //        return out;
  //    }
   class LineProjectionComparator(start: V3, end: V3) extends Comparator[SkeletonCapUpdate.Segment] {
    val dir = new Vector3d(end)
    dir.sub(s)
    line = new Ray3d(s, dir)
     var line: Ray3d = null
    override def compare(o1: SkeletonCapUpdate.Segment, o2: SkeletonCapUpdate.Segment) = Double.compare(line.projectParam(o1.corner), line.projectParam(o2.corner))
  }
}
class SkeletonCapUpdate( var skel: Skeleton) {
  // height is our height now, finalHeight is the height of all new geometry (we might slop upwards a bit)
   var height = .0
   var finalHeight = .0
   var oBCorner = new DHash[_, _]
   var oldCorners: LoopL[Corner] = null
   val edgeInfo = new HashMap[_, _]
  // given in update
   var nOCorner: DHash[Corner, Corner] = null
   var nOSegments: SetCorrespondence[Corner, Corner] = null
  /**
   * Returns a copy of "old" loop. Users are expected to duplicate it, before returning it to
   * update, below, with the corresponding data about what came from where.
   *
   * > you must not modify it
   * > you must not change the .currentCorners
   *
   */
  def getCap(height: Double) = getCap(height, height)
  def getCap(height: Double, finalHeight: Double) = {
    this.height = height
    this.finalHeight = finalHeight
    oldCorners = skel.capCopy(height)
    oBCorner = skel.cornerMap

    for (c <- oldCorners.eIterator) {
      c.z = 0
      val baseC = skel.cornerMap.get(c)
      var ei = edgeInfo.get(baseC.nextL)
      if (ei == null) edgeInfo.put(baseC.nextL, ei = new SkeletonCapUpdate.EdgeInfo(baseC.nextL))
      ei.addBottomSeg(c)
    }

    for (c <- oldCorners.eIterator) {
      c.nextL.calculateLinearForm()
    }
    oldCorners
  }
  def getOldBaseLookup = oBCorner
  /**
   * The complicated bit:
   *
   * Given a new topology, and a map of what corresponds to the old bits, we ensure the output-faces of the skeleton
   * are continuous. A picture of the algorithm is in a SVG file. somewhere.
   *
   * When we update there are several possibilities:
   * (*) One edge may split to several parallel edges
   * (*) Several edges may combine to one (if parallel)
   * (*) Edges are created/destroyed
   *
   * @param newPlan    The new plan that we're updating to
   * @param nOSegments The edge-correspondence between the new plan and the old. These two segments (corner -> corner.nextC ) are aligned, and the edge remains the same.
   * @param nOCorner   The corner-correspondence between the new plan and the old. A corner has an entry here if it is in exzacadey the same place.
   *
   *                   NewPlan contains new edges, if there is a mapping for them in nOsegments, then the skeleton should only contain the corresponding edge from base.
   *
   */
  def update(newPlan: LoopL[Corner], nOSegments: SetCorrespondence[Corner, Corner], nOCorner: DHash[Corner, Corner]): Unit = {
    this.nOSegments = nOSegments
    this.nOCorner = nOCorner
    //        DebugWindow.showIfNotShown( newPlan );
    // The edge info sections were constructed on the base topology. Colinear edges may have been merged. Extract the info from nOSegments, and merge edgeInfos.

    for (nc <- newPlan.eIterator) {
      val eiToMerge = new LinkedHashSet[_]

      for (old <- nOSegments.getSetA(nc)) {
        eiToMerge.add(edgeInfo.get(oBCorner.get(old).nextL))
      }
      if (eiToMerge.size < 2) continue //todo: continue is not supported
      val eiit = eiToMerge.iterator
      val toKeep = eiit.next // edgeInfo.size()

      while (eiit.hasNext) {
        val togo = eiit.next
        toKeep.addFrom(togo)
        edgeInfo.remove(togo.base)
        skel.output.merge(toKeep.base.start, togo.base.start)
      }
    }

    for (old <- oBCorner.ab.keySet) {
      old.z = height
    }

    for (c <- newPlan.eIterator) {
      c.z = finalHeight
    } // del: c.nextC.z != 0


    for (neu <- newPlan.eIterator) {
      neu.nextL.calculateLinearForm()
      // before neu list is broken up (by collectCorners), store the segments in edgeInfo

      for (oldSegment <- nOSegments.getSetA(neu)) {
        //            assert ( oldEdge == null || oldSegment.nextL == oldEdge ); // if all coplanar edges are merged into one
        //
        //            oldEdge = oldSegment.nextL;
        val baseEdge = oBCorner.get(oldSegment).nextL
        var ei = edgeInfo.get(baseEdge)
        if (ei == null) edgeInfo.put(baseEdge, ei = new SkeletonCapUpdate.EdgeInfo(baseEdge))
        ei.addTopSeg(neu)
      }
    }
    val cornersToDelete = new HashSet[_](skel.liveCorners)

    for (c <- newPlan.eIterator) {
      collectCorners(c, cornersToDelete)
    }

    for (baseC <- cornersToDelete) {
      val old = oBCorner.teg(baseC)
      // add vertical edge
      skel.output.addOutputSideTo(old, baseC, baseC.prevL, baseC.nextL)
      baseC.prevL.currentCorners.remove(baseC)
      baseC.nextL.currentCorners.remove(baseC)
    }
    skel.liveCorners.removeAll(cornersToDelete)
    // edgeInfo contains all segments from old and new that pretend to come from the same old edge
    oldEdges //todo: labels are not supported

    for (baseE <- edgeInfo.keySet) {
      //            Edge baseE;
      //            old.end.z = old.start.z = height;
      //            old.calculateLinearForm();
      // edges with joins/departs have an edgeInfo entry
      // everything else is already dealt with. old is still in the new structure
      val ei = edgeInfo.get(baseE)
      if (ei == null) continue oldEdges //todo: continue is not supported
      val segs = ei.sort
      var trailing: SkeletonCapUpdate.Segment = null
      segs //todo: labels are not supported

      for (s <- segs) {
        // if it's a unchanged segment, do nothing,
        if (nOCorner.containsA(s.corner) || nOCorner.containsB(s.corner)) {
          // the corner isn't being replaced by this edge, nothing to do
          assert(trailing == null)
          // don't count towards towards alternating trailing
          continue segs //todo: continue is not supported

        }
        else if (!s.top) {
          // add vertical end for base-> old
          val baseC = oBCorner.get(s.corner)
          skel.output.addOutputSideTo(s.corner, baseC, baseC.prevL, baseC.nextL)
          baseE.currentCorners.remove(s.corner)
        }
        else {
          // just add the relevant entries in the corner maps
          skel.liveCorners.add(s.corner)
          baseE.currentCorners.add(s.corner)
          if (s.start) s.corner.nextL = baseE
          else s.corner.prevL = baseE
        }
        if (trailing != null) // polarity needs fixing up? first section sometimes capped, sometimes not.
        {
          // because the cap (base) is at height 0, we elevate it here.
          skel.output.addOutputSideTo(true, trailing.corner, s.corner, baseE)
          trailing = null // every other segment

        }
        else trailing = s // every other segment
      }
    }
    /**
     * Pointers may have been removed by previous/next edges, or some edges may have been removed entirely
     */
    // this is broken - where do we remove other corners that might reference htis edge?
    val eit = skel.liveEdges.iterator
    while (eit.hasNext) {
      val e = eit.next
      if (e.currentCorners.size == 0) {
        eit.remove()
        skel.liveCorners.remove(e.start)
        skel.liveCorners.remove(e.end)
      }
    }
    skel.refindAllFaceEventsLater()
    DebugDevice.dump("post cap update", skel)
    skel.validate()
  }
  /**
   * this accumulates the structure of EdgeInfos, detailing the points
   * to be inserted into each edge of the old structure.
   *
   * @param toDelete -- remove the corner from this list if it shouldn't be deleted
   */
  private def collectCorners(neu: Corner, toDelete: util.Set[Corner]): Unit = {
    val oldSegments = nOSegments.getSetA(neu)
    //        Edge oldEdge = null;
    val old = nOCorner.get(neu)
    val base = oBCorner.get(old)
    val neuEdge = neu.nextL
    if (oldSegments.isEmpty) {
      //            Corner previous = base != null ? base.nextL.start
      if (skel.liveEdges.add(neuEdge)) {
        skel.output.newEdge(neuEdge, null, neuEdge.profileFeatures)
        neuEdge.machine.addEdge(neuEdge, skel)
      }
      skel.output.newDefiningSegment(neu)
    } // tis an entirely new edge, add it in!

    if (old != null) {
      assert(base != null) // topology of old, base matches

      neu.prevC.nextC = base
      neu.nextC.prevC = base
      base.nextC = neu.nextC
      base.prevC = neu.prevC
      toDelete.remove(base) // we'll use that, thanks

    }
    else {
      skel.liveCorners.add(neu)
      neuEdge.currentCorners.add(neu)
      neu.prevL.currentCorners.add(neu)
    } // old == null (=> base == null)

  }
  //                if (!s.neu)
  //                {
  //                    if (s.start)
  //                        inOld = true;
  //                    else
  //                        inOld = false;
  //                }
  //                else
  //                {
  //                    if (s.start)
  //                        inNeu = true;
  //                    else
  //                        inOld = false;
  //                }
  //            }
  //            // if the old corner from the start of this line is longer in use
  //            if ( nOCorner.get( firstC ) == null )
  //            {
  //                Corner togo = cornerMap.get( old.start );
  //                assert ( togo.nextL == base );
  //                base.currentCorners.remove( togo );
  //                skel.output.addOutputSideTo( togo, old.start, togo.nextL, togo.prevL );
  //                // should also add output side to the previous horizontal underside!
  //                skel.output.addOutputSideTo( old.start, firstC, togo.nextL );
  //            }
  //            if ( firstC.nextC.prevC != firstC )
  //                // corner is a lower corner - was never raised up
  //                cornersFromStart.set( 0, firstC.nextC.prevC );
  //
  //            if ( nOCorner.get( lastC ) == null )
  //            {
  //                Corner togo = cornerMap.get( old.end );
  //                assert (togo.prevL == base);
  //                base.currentCorners.remove( togo );
  //                skel.output.addOutputSideTo( togo, old.end, togo.prevL, togo.nextL );
  //                skel.output.addOutputSideTo( old.end, lastC, togo.prevL ); // fixme: missing underside on nextL
  //                }
  //            if ( lastC.prevC.nextC != lastC )
  //                // corner is a lower corner - was never raised up
  //                cornersFromStart.set( cornersFromStart.size() - 1, lastC.prevC.nextC );
  //
  //            // if this is an old edge that we're keeping we go through the following, but shouldn't change anything!
  //
  //            // are we currently inside the new solid? -_-
  //            boolean onOld = true;
  //            for ( Pair<Corner, Corner> pair : new ConsecutiveItPairs<Corner>( cornersFromStart ) )
  //            {
  //                skel.liveCorners.add( pair.first() ); // adds twice for each interior point, no harm done
  //                skel.liveCorners.add( pair.second() );
  //
  //                if ( onOld )
  //                {
  //                    pair.first().nextL = base;
  //                    pair.second().prevL = base;
  //                    base.currentCorners.add( pair.first() );
  //                    base.currentCorners.add( pair.second() );
  //                }
  //                else
  //                    skel.output.addOutputSideTo( pair.first(), pair.second(), base );
  //
  //                onOld = !onOld;
  //            }
  //        }
  // remove any old edges from the current graph - old if
  //        liveEdges:
  //        while (eit.hasNext())
  //        {
  //            Edge base = eit.next();
  //
  //            Edge old = edgeMap.teg( base );
  //
  //            if (old == null) // new line!
  //                continue liveEdges;
  //
  //            old.end.z = old.start.z = height;
  //
  //            EdgeInfo ei = edgeInfo.get( old );
  //
  //            if (ei == null)
  //            {
  //                // this edge isn't referenced in the new edge loop. kill it.
  //                eit.remove();
  //
  //                // add output edges to cap the edge
  //                skel.output.addOutputSideTo( old.start, old.end, base );
  //
  //                base.currentCorners.clear();
  //            }
  //        }
}

