package utils.math.misc

import utils.math.Scalar

object Maps extends MapsTrait

trait MapsTrait {

//  def mapIntervalToIntervalUnsafe(from:IntervalT[Scalar], to:IntervalT[Scalar]):Scalar => Scalar = x => lerp(from, to, x)

  def withDefaultValue[A, B](default: B, f: A => Option[B]): A => B = f.apply(_).getOrElse(default)

  def clampToInterval[A](interval: IntervalT[A])(implicit ev$1: A => Ordered[A]): A => Option[A] = value => Option.when(interval.contains(value))(value)

  def mapInterval[A, B](interval: IntervalT[A], f: A => B)(implicit ev$1: A => Ordered[A]): A => Option[B] = value => clampToInterval(interval)(ev$1)(value).map(f)

  def combineMaps[A, B](maps: Seq[A => Option[B]], combine: (B, B) => B): A => Option[B] = value => maps.flatMap(f => f(value)).reduceOption(combine)

  def deadZoneInterval[A](zone: IntervalT[A])(implicit ev$1: A => Ordered[A]): A => Option[A] = v => Option.when(zone.notContains(v))(v)

  def deadZone(deadZone: Scalar): Scalar => Option[Scalar] = deadZoneInterval(IntervalT[Scalar](-deadZone, deadZone))

  def deadZoneWithDefault(default: Scalar, deadZone: Scalar): Scalar => Scalar = withDefaultValue(default, deadZoneInterval(IntervalT[Scalar](-deadZone, deadZone)))

}
