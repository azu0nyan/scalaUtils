package utils.math.planar.algo.straightSkeleton

import scala.collection.mutable

class DirectionHeightEvent(
                            var machine: Machine,
                            protected var height: Double, // newAngle is the angle that this edge will turn towards at the above height
                           // length is the distance until the next event (only used for horizontal directions)
                           var newAngle: Double) extends HeightEvent {
  var profileFeatures = new mutable.LinkedHashSet[Tag]
  def this(machine: Machine, angle: Double)  = {
    this(machine, 0, angle)
  }
  def getAngle = newAngle
  override def getHeight = height
  override def process(skel: Skeleton) = {
    //        System.out.println("machine "+machine.toString()+" at "+height+" setting angle "+newAngle );
    // set the new angle
    machine.currentAngle = newAngle
    val update = new SkeletonCapUpdate(skel)
    
    // add in the output edges for the outgoing face:
    val cap = update.getCap(height)
    val cc = new CornerClone(cap)
    
    // preserve corner information for assigning parents, later
    val nOCorner = cc.nOCorner.shallowDupe()
    
    for (c <- cc.output.eIterator) {
      // corners are untouched if neither attached edge has this machine
      if ((c.nextL.machine eq machine) || (c.prevL.machine eq machine)) nOCorner.removeA(c)
      // segments are untouched if they don't contain this machine
      if (c.nextL.machine eq machine) {
        // copy over profile features
        // yes, it looks like we add features to an edge we later disable, but it is still referenced in the entire loop and gets used for edge properties.
        c.nextL.profileFeatures = profileFeatures
        cc.nOSegments.removeA(c)
      }
    }
    update.update(cc.output, cc.nOSegments, nOCorner)

    /**
     * Must now update the parent field in output for the new edges, so that
     * we know where a face came from
     */
    for (c <- skel.liveCorners) {
      if (c.nextL.machine eq machine) {
        val old = update.getOldBaseLookup.get(cc.nOCorner.get(c).get)//todo safe
        skel.output.setParent(c.nextL.start, old.get.nextL.start) //todo safe
      }
    }
    machine.findNextHeight(skel)
    true
  }
}


