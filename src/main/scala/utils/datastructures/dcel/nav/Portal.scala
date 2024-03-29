package utils.datastructures.dcel.nav

import utils.math.Scalar
import utils.math.planar.{SegmentPlanar, V2}
object Portal {





    /** Connects two half edges */
    sealed trait Portal {

      def from: NavigableHalfEdge
      def to: NavigableHalfEdge
      def fromFraction: Scalar
      def toFraction: Scalar
      def width: Scalar

      def fraction(b: NavigableHalfEdge): Scalar = if (b == from) fromFraction else toFraction

      def fromPoint: V2 = from.hierarchicalEdge.asSegment.sampleAt(fromFraction)

      def toPoint: V2 = to.hierarchicalEdge.asSegment.sampleAt(fromFraction)


      def middleFromSegment: SegmentPlanar =
        SegmentPlanar(
          fromPoint - from.hierarchicalEdge.asSegment.body.normalize * width * .5f,
          fromPoint + from.hierarchicalEdge.asSegment.body.normalize * width * .5f,
        )
      def middleToSegment: SegmentPlanar =
        SegmentPlanar(
          toPoint - to.hierarchicalEdge.asSegment.body.normalize * width * .5f,
          toPoint + to.hierarchicalEdge.asSegment.body.normalize * width * .5f,
        )

      def isValid: Boolean =
        fromFraction >= 0 &&
          fromFraction <= 1 && {
          val len = from.hierarchicalEdge.asSegment.length
          val side1 = len * fromFraction
          val side2 = len * (1 - fromFraction)
          width / 2f <= side1 && width / 2f <= side2
        } && {
          val len = to.hierarchicalEdge.asSegment.length
          val side1 = len * toFraction
          val side2 = len * (1 - toFraction)
          width / 2f <= side1 && width / 2f <= side2
        }
    }


}
