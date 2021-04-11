package utils.math.planar

import utils.math._

object SegmentPlanarOps {
  def haveCommonSegmentAt(fSeg:SegmentPlanar, tSeg:SegmentPlanar, length:Scalar, atFirstFraction:Scalar = 0.5 ):Boolean = {
    fSeg.intersection(tSeg) match {
      case Some(SegmentIntersection(intersection)) =>
        val fractionOffset = length / fSeg.length * .5
        val atFrom = fSeg.sampleAt(atFirstFraction - fractionOffset)
        val atTo = fSeg.sampleAt(atFirstFraction + fractionOffset)
        intersection.contains(atFrom) && intersection.contains(atTo)
      case _ => false
    }
  }
}
