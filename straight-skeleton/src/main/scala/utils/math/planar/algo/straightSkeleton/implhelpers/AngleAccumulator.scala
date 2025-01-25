package utils.math.planar.algo.straightSkeleton.implhelpers

import utils.math.planar.V2
import utils.math.space.V3


/**
 * Keeps track of the angles of a polygon to decide on clockwise or counterclockwise determination.
 * Should assume anti-clockwise traversal
 *
 * @author twak
 */
object AngleAccumulator {

  def sign(loop: Loop[V2]): Boolean = {
    val aa = new AngleAccumulator(true, V3(0, 0, 1))
    for (p <- loop.iterator) {
      aa.add(V3(p.x, p.y, 0))
    }
    aa.correctAngle()
  }
}

class AngleAccumulator(targetPositive: Boolean, normal: V3) {
  var angle: Double = 0
  var firstPoint: Option[V3] = None
  var lastPoint: Option[V3] = None
  var firstVector: Option[V3] = None
  var lastVector: Option[V3] = None

  def add(pt: V3): Unit = {
    if (lastPoint.isEmpty) {
      firstPoint = Some(pt)
      lastPoint = Some(pt)
    } else {
      val v = pt - lastPoint.get
      addInner(v)
      lastPoint = Some(pt)
    }
  }

  private def addInner(v: V3): Unit = {
    if (lastVector.nonEmpty) {
      //            System.err.println ("adding angle between "+lastVector +" and "+v);
      var dA = v.angle(lastVector.get)
      val cross = lastVector.get ^ v
      // parallel
      if (Math.abs(cross.length) < 0.01) {}
      else if (cross.angle(normal) > Math.PI / 2) {
        dA = -dA
      }
      angle += dA
    }
    else firstVector = Some(v)
    // rotate angle around origin so normal is up
    lastVector = Some(v)
  }

  def correctAngle(): Boolean = {
    if (firstVector.nonEmpty) {
      add(firstPoint.get)
      add(firstVector.get)
      firstVector = None // allow method to be called 1+ time (can't add mroe points tho ;))
    }

    if (targetPositive)
      Math.abs(angle - Math.PI * 2) < 0.1
    else
      Math.abs(angle + Math.PI * 2) < 0.1
  }
}
