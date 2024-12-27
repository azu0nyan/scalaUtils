package utils.math.planar.algo.straightSkeleton

import java.util.{List, Map, Set}



import java.util
import org.twak.camp.Output.Face
import org.twak.camp.ui.DirectionHeightEvent
import org.twak.utils.collections.DHash
import org.twak.utils.collections.Loop
import org.twak.utils.collections.LoopL
import org.twak.utils.collections.ManyManyMap
import org.twak.utils.collections.SetCorrespondence


/**
 * input - given in constructor
 * old - the copy we make and store in var corners
 * output - the copies we return in the Offset
 *
 * @author twak
 */
object OffsetSkeleton {
  class Offset(
                // the shape of the offset at the given height
                var shape: LoopL[Corner], // Contains a map between the input edges and the edges in shape, above.
                var nOSegments: ManyManyMap[Corner, Corner]) {
  }
  def shrink(in: LoopL[Edge], dist: Double) = {
    val cLoopL = Corner.cornerToEdgeLoopL(in)
    val os = new OffsetSkeleton[Machine](cLoopL, 100)
    val allMachines = new HashSet[_]

    for (e <- in.eIterator) {
      allMachines.add(e.machine)
    }

    for (m <- allMachines) {
      os.registerProfile(m, Math.atan(dist / 100), 0)
    }
    val res = os.getResults
    if (res.isEmpty) new LoopL[_]
    else res.get(0).shape
  }
  /**
   * The output of an offset surface only knows about corresponding edges, not corresponding corners. Given
   * that you know if an old corner will have moved (did you assign a weight to it's machine?) this class recovers
   * the map.
   *
   * Unmoved corners end up in nOCorner.
   * Equivilent edges end up in nOSegment (this may not be what you want)
   *
   */
  abstract class FindNOCorner(offset: OffsetSkeleton.Offset, cap: LoopL[Corner]) {
    nOSegmentsUpdate = offset.nOSegments.toSetCorrespondence

    /**
     * The maps that come out of offset relate the old edge positions to their
     * new positions, not the direct edge or corner correspondence that they do in
     * cap update => goal:
     *
     * if both adjacent edges of speed 0, entry in nOCorner.
     * for all input corners:
     * if equivilent edges exist in cap output, and had speed 0
     * add an entry between (find new corner between two edges) -> existing
     *
     * if segment's edge has speed 0, entry in nOSegment
     * */
    // add all valid corners to nOCorner



    for (oldC <- cap.eIterator) {
      if (didThisOldCornerRemainUnchanged(oldC)) {
        // two edges in the
        val second = nOSegmentsUpdate.getSetB(oldC)
        val first = nOSegmentsUpdate.getSetB(oldC.prevC)
        val neuC = findAdjacent(first, second)
        if (neuC != null) nOCorner.put(neuC, oldC) // corner hasn't been swallowed

      }
    } // only if both edges are 0-speed (don't use our machine)


    // results end up here
    var nOSegmentsUpdate: SetCorrespondence[Corner, Corner] = null
    var nOCorner = new DHash[Corner, Corner]
    /**
     * Given two sets of leading corners, find the corner (if any) that lies on
     * both edges
     *
     * @return
     */
    private def findAdjacent(first: util.Set[Corner], second: util.Set[Corner]): Corner = {

      for (c <- first) {
        if (second.contains(c.nextC)) return c.nextC
      }
      null
    }
    def didThisOldCornerRemainUnchanged(oldCorner: Corner): Boolean
  }
}
class OffsetSkeleton[E <: Machine](corners: LoopL[Corner], var interval: Double) { // clone the input, so we're non-destructive
  val cc = new CornerClone(corners)
  this.corners = cc.output
  oldInputSegments = cc.nOSegments
  private var output: util.List[OffsetSkeleton.Offset] = null
  var profileOffset = new LinkedHashMap[_, _]
  var offsetProfile = new LinkedHashMap[_, _]
   var corners: LoopL[Corner] = null
   var machinesCount = 0
   val machinesOutstanding = 0
   var lastStep = -1
   var oldInputSegments: SetCorrespondence[Corner, Corner] = null
  // the skeleton resulting from
  var outputSkeleton: Skeleton = null
  /**
   * We ignore the contents of the profile, and just use it as a marker to set the specified offset
   * at teh specified height.
   *
   * @param step - which multiple of interval are we at?
   */
  def registerProfile(profile: E, angle: Double, step: Int): Unit = {
    assert(profile != null)
    val height = step * interval
    var om = profileOffset.get(profile)
    if (om == null) {
      om = new OffsetSkeleton[E]#OffsetMachine
      profileOffset.put(profile, om)
      offsetProfile.put(om, profile)
      machinesCount += 1
    }
    om.addHeightEvent(new DirectionHeightEvent(om, height, angle))
    if (step > lastStep) lastStep = step
  }
  //    public List<Offset> getResults()
  //    {
  //        return getResults( false );
  //    }
  def getResults = {
    output = new ArrayList[_]
    val unspecifiedNewMachineOld = new HashMap[_, _]
    // assign the new machines we've created

    for (c <- corners.eIterator) {
      val e = c.nextL
      var m = profileOffset.get(e.machine)
      if (m == null) {
        // default angle is 0
        m = new Machine(0)
        unspecifiedNewMachineOld.put(m, e.machine)
        e.machine = m
      }
      else e.machine = m
    }
    outputSkeleton = new Skeleton(corners, (lastStep + 1) * interval, true)
    outputSkeleton.name = "offset"
    var last: OffsetSkeleton[E]#InstanceHeightEvent = null
    // add instancing events to capture skeleton cap at each height
    for (i <- 0 to lastStep) {
      outputSkeleton.qu.add(last = new OffsetSkeleton[E]#InstanceHeightEvent(i))
    }
    if (last != null) last.endHere = true
    //        DebugWindow.showIfNotShown( edges );
    outputSkeleton.skeleton()
    // restore original machines for calling routine

    for (offset <- output) {

      for (e <- Edge.uniqueEdges(offset.shape)) {
        assert(e.machine != null)
        val origMachine = offsetProfile.get(e.machine)
        if (origMachine != null) e.machine = origMachine
        else e.machine = unspecifiedNewMachineOld.get(e.machine)
        e.machine.addEdge(e, outputSkeleton)
      }
    }
    //        for ( Face f : outputShape.output.faces.values() )// new HashSet ( outputShape.output.faces.values().size()).size()
    //        {
    //            Edge e = f.edge;
    //
    //            System.out.println(">>> "+e);
    //
    //            Machine origMachine = offsetProfile.get( e.machine );
    //            if ( origMachine != null )
    //                e.machine = origMachine;
    //            else
    //                e.machine = unspecifiedNewMachineOld.get( e.machine );
    //            e.machine.addEdge( e, outputShape );
    //        }
    output
  } //boolean kludge)

  def getInputEdge(f: Output#Face) = {
    val first = outputSkeleton.output.getGreatestGrandParent(f).edge
    new ArrayList[_](oldInputSegments.getSetA(first.start))
  }
  class InstanceHeightEvent( var step: Int) extends HeightEvent {
    height = Math.nextAfter((step + 1) * interval, -1) // just before we change direction for the next set of edges

     var height = .0
     val endHere = false
    override def getHeight = height
    override def process(skel: Skeleton) = {
      val copy = skel.capCopy(height)
      // I would like to appologise to my future self for writing this method.
      // (skel -> cap) segment map to (skel-before-direction-events -> cap)
      val outputOldSegments = new ManyManyMap[Corner, Corner]#ConvertInputCollection[Corner](skel.getSegmentOriginator).get
      // to input -> cap
      var inputCapSegments = new ManyManyMap[Corner, Corner]#ConvertInputCollection[Corner](oldInputSegments.asCache).get
      inputCapSegments = inputCapSegments.getFlipShallow
      output.add(new OffsetSkeleton.Offset(copy, inputCapSegments)) // should be new->old segments

      if (endHere) {
        // cap everything below to tidy up.

        for (c <- skel.liveCorners) {
          val top = skel.cornerMap.teg(c)
          skel.output.addOutputSideTo(c, top, c.nextL, c.prevL)
          skel.output.addOutputSideTo(true, top, top.nextC, c.nextL)
        }
        // no more output events pls!
        //                skel.output.faces.clear();
        skel.liveEdges.clear()
        skel.liveCorners.clear()
        skel.qu.clearFaceEvents()
      } // fixme: part of a kludge-fix. seems to be responsible for final cap.

      false
    }
  }
  class OffsetMachine
  //        E profile;
    extends Machine {
    events.clear()
  }
}

