package utils.math.planar.algo.straightSkeleton

import utils.datastructures.containers.map.impl.MutableBiMap
import utils.math.planar.algo.straightSkeleton.OffsetSkeleton.Offset
import utils.math.planar.algo.straightSkeleton.helpers.{LoopL, ManyManyMap, SetCorrespondence}

import scala.collection.mutable

/**
 * input - given in constructor
 * old - the copy we make and store in var corners
 * output - the copies we return in the Offset
 *
 * @author twak
 */


class OffsetSkeleton(cornersIn: LoopL[Corner], var interval: Double) { // clone the input, so we're non-destructive
  val cc = new CornerClone(cornersIn)
  var corners: LoopL[Corner] = cc.output
  var oldInputSegments: SetCorrespondence[Corner, Corner] = cc.nOSegments
  private var output: mutable.Buffer[OffsetSkeleton.Offset] = null

  var profileOffset = new mutable.LinkedHashMap[Machine, Machine]
  var offsetProfile = new mutable.LinkedHashMap[Machine, Machine]

  var machinesCount = 0
  val machinesOutstanding = 0
  var lastStep = -1

  // the skeleton resulting from
  var outputSkeleton: Skeleton = null
  /**
   * We ignore the contents of the profile, and just use it as a marker to set the specified offset
   * at teh specified height.
   *
   * @param step - which multiple of interval are we at?
   */
  def registerProfile(profile: Machine, angle: Double, step: Int): Unit = {
    assert(profile != null)
    val height = step * interval
    val om = profileOffset.get(profile) match
      case Some(om) => om
      case None =>
        val om = new OffsetMachine
        profileOffset.put(profile, om)
        offsetProfile.put(om, profile)
        machinesCount += 1
        om

    om.addHeightEvent(new DirectionHeightEvent(om, height, angle))
    if (step > lastStep)
      lastStep = step
  }

  def getResults = {
    output = mutable.Buffer[Offset]()
    val unspecifiedNewMachineOld = new mutable.HashMap[Machine, Machine]
    // assign the new machines we've created

    for (c <- corners.eIterator) {
      val e = c.nextL
      e.machine = profileOffset.get(e.machine) match
        case Some(m) => m
        case None =>
          // default angle is 0
          val m = new Machine(0)
          unspecifiedNewMachineOld.put(m, e.machine)
          m
    }

    outputSkeleton = new Skeleton(corners, (lastStep + 1) * interval, true)
    outputSkeleton.name = "offset"
    var last: OffsetSkeleton#InstanceHeightEvent = null
    // add instancing events to capture skeleton cap at each height
    for (i <- 0 to lastStep) {
      last = new InstanceHeightEvent(i)
      outputSkeleton.qu.add(last)
    }
    if (last != null)
      last.endHere = true
    //        DebugWindow.showIfNotShown( edges );
    outputSkeleton.skeleton()
    // restore original machines for calling routine

    for (offset <- output) {

      for (e <- Edge.uniqueEdges(offset.shape)) {
        assert(e.machine != null)
        val origMachine = offsetProfile.get(e.machine).get //todo safe
        if (origMachine != null) e.machine = origMachine
        else e.machine = unspecifiedNewMachineOld.get(e.machine).get //todo safe
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
    val first = outputSkeleton.output.getGreatestGrandParent(Some(f)).get.edge //todo safe
    val r = mutable.Buffer[Corner]()
    r ++= oldInputSegments.getSetA(first.start)
    r
  }

  class InstanceHeightEvent(var step: Int) extends HeightEvent {
    height = Math.nextAfter((step + 1) * interval, -1) // just before we change direction for the next set of edges

    var height = .0
    var endHere = false
    override def getHeight = height
    override def process(skel: Skeleton) = {
      val copy = skel.capCopy(height)
      // I would like to appologise to my future self for writing this method.
      // (skel -> cap) segment map to (skel-before-direction-events -> cap)
//      skel.segmentMap.ConvertInputCollection[Corner, mutable.Buffer[Corner]](???)
      
      val outputOldSegments = skel.segmentMap.ConvertInputCollection[Corner, mutable.Buffer[Corner]](skel.getSegmentOriginator)
      // to input -> cap
      var inputCapSegments =  skel.segmentMap.ConvertInputCollection[Corner, mutable.Set[Corner]](oldInputSegments.asCache)
      inputCapSegments = inputCapSegments.getFlipShallow
      output += new OffsetSkeleton.Offset(copy, inputCapSegments) // should be new->old segments

      if (endHere) {
        // cap everything below to tidy up.

        for (c <- skel.liveCorners) {
          val top = skel.cornerMap.teg(c)
          skel.output.addOutputSideTo(c.asV3, top.get.asV3, c.nextL, c.prevL) //todo safe
          skel.output.addOutputSideTo_(true, top.get.asV3, top.get.nextC.asV3, c.nextL)
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

object OffsetSkeleton {
  class Offset(
                // the shape of the offset at the given height
                var shape: LoopL[Corner], // Contains a map between the input edges and the edges in shape, above.
                var nOSegments: ManyManyMap[Corner, Corner]) {
  }

  def shrink(in: LoopL[Edge], dist: Double) = {
    val cLoopL = Corner.cornerToEdgeLoopL(in)
    val os = new OffsetSkeleton(cLoopL, 100)
    val allMachines = new mutable.HashSet[Machine]()

    for (e <- in.eIterator) {
      allMachines.add(e.machine)
    }

    for (m <- allMachines) {
      os.registerProfile(m, Math.atan(dist / 100), 0)
    }
    val res = os.getResults
    if (res.isEmpty) new LoopL[Offset]
    else res(0).shape
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
        if (neuC.nonEmpty)
          nOCorner.put(neuC.get, oldC) // corner hasn't been swallowed

      }
    } // only if both edges are 0-speed (don't use our machine)


    // results end up here
    var nOSegmentsUpdate: SetCorrespondence[Corner, Corner] = null
    var nOCorner = new MutableBiMap[Corner, Corner]
    /**
     * Given two sets of leading corners, find the corner (if any) that lies on
     * both edges
     *
     * @return
     */
    private def findAdjacent(first: collection.Set[Corner], second: collection.Set[Corner]): Option[Corner] =
      first.find(c => second.contains(c.nextC)).map(_.nextC)

    def didThisOldCornerRemainUnchanged(oldCorner: Corner): Boolean
  }
}