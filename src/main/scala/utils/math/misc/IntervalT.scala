package utils.math.misc

import utils.math.Scalar

object IntervalT {

}

trait IntervalTrait[T] {
  def start: T

  def end: T

  def containsStart: Boolean

  def containsEnd: Boolean = true

  def open: Boolean = !containsStart && !containsEnd

  def closed: Boolean = containsStart && containsEnd

  def inverted: IntervalT[T] = IntervalT(end, start, containsEnd, containsStart)

  def lengthSigned(implicit c: Numeric[T]): T = implicitly[Numeric[T]].minus(end, start)

  def notContains(v: T)(implicit ev$1: T => Ordered[T]): Boolean = !contains(v)

  //def length: Float = math.abs(lengthSigned)

  def contains(point: T)(implicit ev$1: T => Ordered[T]): Boolean = (if (containsStart) {
    point >= start
  } else {
    point > start
  }) && (if (containsEnd) {
    point <= end
  } else {
    point < end
  }
    )
}

case class Interval(override val start: Scalar,
                    override val end: Scalar,
                    override val containsStart: Boolean = true,
                    override val containsEnd: Boolean = true) extends IntervalTrait[Scalar]

case class IntervalT[T](
                         override val start: T,
                         override val end: T,
                         override val containsStart: Boolean = true,
                         override val containsEnd: Boolean = true)  extends IntervalTrait[T]
