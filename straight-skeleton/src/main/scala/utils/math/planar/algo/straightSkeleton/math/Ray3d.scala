package utils.math.planar.algo.straightSkeleton.math

import utils.math.space.V3

object Ray3d {
  def fromStartEnd(start: V3, end: V3) = {
    new Ray3d(start, end - start)
  }
}

class Ray3d(val origin: V3, val direction: V3) {
  def projectLine(ept: V3) = this.project(ept, false)
  def projectSegment(ept: V3) = this.project(ept, true)

  private def project(ept: V3, segment: Boolean): Option[V3] =
    val factor = this.projectParam(ept)
    Option.when(!segment || !(factor < 0.0F.toDouble) && !(factor > 1.0F.toDouble)) {
      direction * factor + origin
    }


  def projectParam(ept: V3) =
    val b = ept - origin
    val factor = (direction ** b) / (direction ** this.direction)
    factor


  override def toString = "[" + this.origin + "." + this.direction + "]"

}

