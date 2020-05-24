package utils.math.planar.patch

import utils.math._
import utils.math.planar.{TransformablePlanar, V2}
import utils.math.planar.patch.Path.PathWithTangent
import utils.math.space.Transform
import utils.math.space.Transform.Transformable

object BezierCurve {
  case class BezierCurveWrongNumberOfPoints() extends Exception
  def apply(w: Seq[V2]): BezierCurve = w.length match {
    case 0 | 1 => throw BezierCurveWrongNumberOfPoints()
    case 3 => BezierCurve2(w(0), w(1), w(2))
    case 4 => BezierCurve3(w(0), w(1), w(2), w(3))
    case 2 | _ => BezierCurveNPoints(w.toIndexedSeq)
  }

  def bezier(t: Scalar, w: IndexedSeq[V2]): V2 = {
    var sum: V2 = V2.ZERO
    val n = w.size - 1
    for (k <- w.indices) {
      sum += w(k) * binomial(n, k).toDouble * ((1 - t) ^^ (n - k)) * (t ^^ k)
    }
    return sum
  }

  def bezier2(w0: V2, w1: V2, w2: V2)(t: Scalar): V2 = {
    val t2 = t * t
    val mt = 1 - t
    val mt2 = mt * mt
    return w0 * mt2 + w1 * 2 * mt * t + w2 * t2
  }

  def bezier3(w0: V2, w1: V2, w2: V2, w3: V2)(t: Scalar): V2 = {
    val t2 = t * t
    val t3 = t2 * t
    val mt = 1 - t
    val mt2 = mt * mt
    val mt3 = mt2 * mt
    return w0 * mt3 + w1 * (mt2 * t * 3) + w2 * (mt * t2 * 3) + w3 * t3
  }

  trait BezierCurve extends PathWithTangent {
    override def argStart: Scalar = 0

    override def argEnd: Scalar = 1

    def controlPoints: Seq[V2]
  }

  case class BezierCurveNPoints(w: IndexedSeq[V2]) extends BezierCurve {
    override def posFromT(t: Scalar): V2 = bezier(t, w)
    override def map(f: V2 => V2): BezierCurveNPoints = BezierCurveNPoints(w.map(f))
    def controlPoints: Seq[V2] = w
  }

  case class BezierCurve2(start: V2, c1: V2, end: V2) extends BezierCurve {
    override def posFromT(t: Scalar): V2 = bezier2(start, c1, end)(t)
    def controlPoints: Seq[V2] = Seq(start, c1, end)
  }

  case class BezierCurve3(start: V2, c1: V2, c2: V2, end: V2) extends BezierCurve {
    override def posFromT(t: Scalar): V2 = bezier3(start, c1, c2, end)(t)
    def controlPoints: Seq[V2] = Seq(start, c1, c2, end)
  }

}




