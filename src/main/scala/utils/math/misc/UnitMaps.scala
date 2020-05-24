package utils.math.misc

import utils.math.{Scalar, _}

object UnitMaps  {
  //type UnitScalar = Scalar
  /**
    * Mappings from [0,1] to [0,1]
    */
  type UnitIntervalMap = Scalar => Scalar

  /**
    * fast start slow end
    */
  def sinUnitCurve(t:Scalar):Scalar  = sin(t * HALF_PI)

  /**
    * slow start fast end
    */
  def cosUnitCurve(t:Scalar):Scalar = 1f - cos(t * HALF_PI)

  def quadricUnitCurve(t:Scalar):Scalar= t * t

  /**
    * 3-th grade polynom curve
    */
  def smoothStepCubic(t:Scalar):Scalar= t * t * (3f - 2f * t)

  /**
    * 5-th grade polynom curve
    */
  def smoothStepQuintic(t:Scalar):Scalar= t * t * t * (t * (6f * t - 15f) + 10f)

  /**
    * 7-th grade polynom curve
    */
  def smoothStepSeptic(t:Scalar):Scalar=t * t * t * t * ( 35f + t * (-84f + t * (70 - 20 * t)))
}

