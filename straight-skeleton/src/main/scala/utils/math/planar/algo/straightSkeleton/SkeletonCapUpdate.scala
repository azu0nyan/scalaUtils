package utils.math.planar.algo.straightSkeleton

import utils.datastructures.containers.map.impl.MutableBiMap
import utils.math.planar.algo.straightSkeleton.SkeletonCapUpdate.EdgeInfo
import utils.math.planar.algo.straightSkeleton.implhelpers.{LoopL, SetCorrespondence}
import utils.math.planar.algo.straightSkeleton.math.Ray3d
import utils.math.space.V3

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
 * Arbitrary topology update of the corners/edges on the sweep plane
 *
 * @author twak
 */
object SkeletonCapUpdate {
  class EdgeInfo(var base: Edge) {
    // corners with their start on the old edge
    //        private Set<Corner>
    //                topSegs = new LinkedHashSet(),
    //                bottomSegs = new LinkedHashSet();
    val segs = ArrayBuffer[Segment]()

    def addTopSeg(c: Corner): Unit = {
      segs += new SkeletonCapUpdate.Segment(c, true, true)
      segs += new SkeletonCapUpdate.Segment(c.nextC, true, false)
    }
    def addBottomSeg(c: Corner): Unit = {
      segs += new SkeletonCapUpdate.Segment(c, false, true)
      segs += new SkeletonCapUpdate.Segment(c.nextC, false, false)
    }

    def sort: collection.Seq[Segment] = {
      segs.sortInPlace()(new SkeletonCapUpdate.LineProjectionComparator(base.start.asV3, base.end.asV3))
      segs
    }

    def addFrom(togo: SkeletonCapUpdate.EdgeInfo): Unit = {
      segs.addAll(togo.segs)
    }
  }
  class Segment(var corner: Corner, var top: Boolean, // !bottom
                var start: Boolean // !end
               ) {
  }

  class LineProjectionComparator(start: V3, end: V3) extends Ordering[SkeletonCapUpdate.Segment] {
    val dir = end - start
    val line = new Ray3d(start, dir)
    override def compare(o1: SkeletonCapUpdate.Segment, o2: SkeletonCapUpdate.Segment) =
      line.projectParam(o1.corner.asV3) compare line.projectParam(o2.corner.asV3)
  }
}

class SkeletonCapUpdate(var skel: Skeleton) {
  // height is our height now, finalHeight is the height of all new geometry (we might slop upwards a bit)
  var height = .0
  var finalHeight = .0
  var oBCorner = new MutableBiMap[Corner, Corner]
  var oldCorners: LoopL[Corner] = null
  val edgeInfo = new mutable.HashMap[Edge, EdgeInfo]
  // given in update
  var nOCorner: MutableBiMap[Corner, Corner] = null
  var nOSegments: SetCorrespondence[Corner, Corner] = null
  /**
   * Returns a copy of "old" loop. Users are expected to duplicate it, before returning it to
   * update, below, with the corresponding data about what came from where.
   *
   * > you must not modify it
   * > you must not change the .currentCorners
   *
   */
  def getCap(height: Double): LoopL[Corner] = getCap(height, height)
  def getCap(height: Double, finalHeight: Double): LoopL[Corner] = {
    this.height = height
    this.finalHeight = finalHeight
    oldCorners = skel.capCopy(height)
    oBCorner = skel.cornerMap

    for (c <- oldCorners.eIterator) {
      c.z = 0
      val baseC = skel.cornerMap.get(c).get //todo safe

      (edgeInfo.get(baseC.nextL) match
        case Some(ei) => ei
        case None =>
          val ei = new SkeletonCapUpdate.EdgeInfo(baseC.nextL)
          edgeInfo.put(baseC.nextL, ei)
          ei
        ).addBottomSeg(c)
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
  def update(newPlan: LoopL[Corner], nOSegments: SetCorrespondence[Corner, Corner], nOCorner: MutableBiMap[Corner, Corner]): Unit = {
    this.nOSegments = nOSegments
    this.nOCorner = nOCorner
    //        DebugWindow.showIfNotShown( newPlan );
    // The edge info sections were constructed on the base topology. Colinear edges may have been merged. Extract the info from nOSegments, and merge edgeInfos.

    for (nc <- newPlan.eIterator) {
      val eiToMerge = new mutable.LinkedHashSet[EdgeInfo]

      for (old <- nOSegments.getSetA(nc)) {
        eiToMerge.add(edgeInfo.get(oBCorner.get(old).get.nextL).get) //todo safe
      }
      if (eiToMerge.size < 2) {
        /*continue*/
      }
      else {
        val eiit = eiToMerge.iterator
        val toKeep = eiit.next // edgeInfo.size()

        while (eiit.hasNext) {
          val togo = eiit.next
          toKeep.addFrom(togo)
          edgeInfo.remove(togo.base)
          skel.output.merge(toKeep.base.start, togo.base.start)
        }
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
        val baseEdge = oBCorner.get(oldSegment).get.nextL //todo safe
        (edgeInfo.get(baseEdge) match {
          case Some(ei) => ei
          case None =>
            val ei = new SkeletonCapUpdate.EdgeInfo(baseEdge)
            edgeInfo.put(baseEdge, ei)
            ei

        }).addTopSeg(neu)
      }
    }
    val cornersToDelete = new mutable.HashSet[Corner]()
    cornersToDelete ++= skel.liveCorners

    for (c <- newPlan.eIterator) {
      collectCorners(c, cornersToDelete)
    }

    for (baseC <- cornersToDelete) {
      val old = oBCorner.teg(baseC)
      // add vertical edge
      skel.output.addOutputSideTo(old.get.asV3, baseC.asV3, baseC.prevL, baseC.nextL) //todo safe
      baseC.prevL.currentCorners.remove(baseC)
      baseC.nextL.currentCorners.remove(baseC)
    }
    skel.liveCorners --= (cornersToDelete)
    // edgeInfo contains all segments from old and new that pretend to come from the same old edge
    for (baseE <- edgeInfo.keySet) {
      //            Edge baseE;
      //            old.end.z = old.start.z = height;
      //            old.calculateLinearForm();
      // edges with joins/departs have an edgeInfo entry
      // everything else is already dealt with. old is still in the new structure
      edgeInfo.get(baseE) match
        case None => /*continue*/
        case Some(ei) => {
          val segs = ei.sort
          var trailing: Option[SkeletonCapUpdate.Segment] = None

          for (s <- segs) {
            // if it's a unchanged segment, do nothing,
            if (nOCorner.containsA(s.corner) || nOCorner.containsB(s.corner)) {
              // the corner isn't being replaced by this edge, nothing to do
              //              assert(trailing == null)
              // don't count towards towards alternating trailing
              //continue segs 
            } else {
              if (!s.top) {
                // add vertical end for base-> old
                val baseC = oBCorner.get(s.corner)
                skel.output.addOutputSideTo(s.corner.asV3, baseC.get.asV3, baseC.get.prevL, baseC.get.nextL) // todo safe
                baseE.currentCorners.remove(s.corner)
              }
              else {
                // just add the relevant entries in the corner maps
                skel.liveCorners.add(s.corner)
                baseE.currentCorners.add(s.corner)
                if (s.start) s.corner.nextL = baseE
                else s.corner.prevL = baseE
              }

              if (trailing.nonEmpty) // polarity needs fixing up? first section sometimes capped, sometimes not.
              {
                // because the cap (base) is at height 0, we elevate it here.
                skel.output.addOutputSideTo_(true, trailing.get.corner.asV3, s.corner.asV3, baseE)
                trailing = None // every other segment

              }
              else
                trailing = Some(s) // every other segment
            }
          }
        }


    }

    /**
     * Pointers may have been removed by previous/next edges, or some edges may have been removed entirely
     */
    // this is broken - where do we remove other corners that might reference htis edge?
    skel.liveEdges.filterInPlace(e =>
      if (e.currentCorners.nonEmpty) false
      else {
        skel.liveEdges -= e
        skel.liveCorners.remove(e.start)
        skel.liveCorners.remove(e.end)
      }
    )

    skel.refindAllFaceEventsLater()
    skel.validate()
  }

  /**
   * this accumulates the structure of EdgeInfos, detailing the points
   * to be inserted into each edge of the old structure.
   *
   * @param toDelete -- remove the corner from this list if it shouldn't be deleted
   */
  private def collectCorners(neu: Corner, toDelete: mutable.Set[Corner]): Unit = {
    val oldSegments = nOSegments.getSetA(neu)
    //        Edge oldEdge = null;
    val old = nOCorner.get(neu)
    val base = oBCorner.get(old.get) // todo safe
    val neuEdge = neu.nextL
    if (oldSegments.isEmpty) {
      //            Corner previous = base != null ? base.nextL.start
      if (skel.liveEdges.add(neuEdge)) {
        skel.output.newEdge(neuEdge, null, neuEdge.profileFeatures)
        neuEdge.machine.addEdge(neuEdge, skel)
      }
      skel.output.newDefiningSegment(neu)
    } // tis an entirely new edge, add it in!

    old match
      case Some(old) => {
        assert(base != null) // topology of old, base matches

        neu.prevC.nextC = base.get //todo safe
        neu.nextC.prevC = base.get //todo safe
        base.get.nextC = neu.nextC
        base.get.prevC = neu.prevC
        toDelete -= base.get // we'll use that, thanks //todo safe
      }

      case None => {
        skel.liveCorners.add(neu)
        neuEdge.currentCorners.add(neu)
        neu.prevL.currentCorners.add(neu)
      } // old == null (=> base == null)

  }
}

