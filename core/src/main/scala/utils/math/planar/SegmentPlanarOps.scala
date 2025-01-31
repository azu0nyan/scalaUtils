package utils.math.planar

import utils.math.*

import scala.collection.mutable

object SegmentPlanarOps {
  def haveCommonSegmentAt(fSeg: SegmentPlanar, tSeg: SegmentPlanar, length: Scalar, atFirstFraction: Scalar = 0.5): Boolean = {
    fSeg.intersection(tSeg) match {
      case Some(SegmentIntersection(intersection)) =>
        val fractionOffset = length / fSeg.length * .5
        val atFrom = fSeg.sampleAt(atFirstFraction - fractionOffset)
        val atTo = fSeg.sampleAt(atFirstFraction + fractionOffset)
        intersection.contains(atFrom) && intersection.contains(atTo)
      case _ => false
    }
  }

  def groupByCollinearity(segs: Seq[SegmentPlanar]):Map[Scalar, Seq[SegmentPlanar]] = {
    val res = mutable.Map[Scalar, mutable.Buffer[SegmentPlanar]]()
    for (s <- segs) {
      val a = {
        val a = s.body.angleToOX
        if (a >= 0) a else a + PI
      }
      res.keySet.find(_ ~= a) match {
        case Some(angle) => res(angle) += s
        case None => res += a -> mutable.Buffer(s)
      }
    }
    res.view.mapValues(_.toSeq).toMap
  }
}
