package utils

import utils.math.misc.MapsTrait
import utils.math.space.intersections.{Raycast, RaycastTrait, SegmentCast, SegmentCastTrait}
import utils.math.space.{TransformTrait, UnitV3, V3, V3Implicits}

package object math
  extends MathUtilsTrait
    with V3Implicits {

  type Normal = V3
  type Scalar = Double
  type Angle = Scalar

  type Translation = V3
  val idTranslation: Translation = V3.ZERO
  type Position = V3
  type Direction = V3
  type Scale = V3
  val idScale: Scale = V3.one
  type Axes = (V3, V3, V3)

  val percision: Scalar = 0.00000001d


  val ZERO: Scalar = 0f
  val ONE: Scalar = 1f
  val TWO: Scalar = 2f
  val HALF: Scalar = 0.5f
  val QUARTER: Scalar = 0.25f
  val EIGHTH: Scalar = 0.125f
  val THIRD: Scalar = 1f / 3f

  val SMALL_NUMBER: Scalar = 0.00001d
  val KINDA_BIG_NUMBER: Scalar = Integer.MAX_VALUE.toDouble
  val BIG_NUMBER: Scalar = Float.MaxValue
  val SMALL_V3: V3 = V3(SMALL_NUMBER, SMALL_NUMBER, SMALL_NUMBER)

  val E: Scalar = Math.E
  val PI: Scalar = Math.PI
  val TWO_PI: Scalar = 2.0f * PI
  val ONE_AND_HALF_PI: Scalar = 1.5f * PI
  val HALF_PI: Scalar = 0.5f * PI
  val QUARTER_PI: Scalar = 0.25f * PI
  val EIGHT_PI: Scalar = 0.125f * PI
  val THREE_QUARTER_PI: Scalar = PI * 0.75
  val INV_PI: Scalar = 1.0f / PI
  val INW_TWO_PI: Scalar = 2.0f * TWO_PI
  /** A value to multiply a degree value by, to convert it to radians. */
  val DEG_TO_RAD: Scalar = PI / 180.0f
  /** A value to multiply a radian value by, to convert it to degrees. */
  val RAD_TO_DEG: Scalar = 180.0f / PI

  /*experimental naming*/
  val `PI/2` = PI / 2
  val `3PI/2` = 3 * PI / 2
  val `2PI/3` = 2 * PI / 3
  val `PI/3` = PI / 3
  val `3PI/4` = 3 * PI / 4
  val `PI/4` = PI / 4
  val `PI/6` = PI / 6
  val `2PI/6` = 2 * PI / 6
  val `5PI/6` = 5 * PI / 6
  val `2PI` = 2 * PI
  val `3PI` = 3 * PI
  
  val `sqrt(2)` = math.sqrt(2)

  def toShortStr(s: Scalar): String = {
    val res = f"$s%.3f"
    val dropped = res.reverse.dropWhile(_ == '0').reverse
    if (dropped.last == '.') dropped.dropRight(1)
    else dropped
  }

  implicit class Power(val s: Scalar) extends AnyVal {
    def ^^(p: Scalar): Scalar = pow(s, p)

    def squared: Scalar = s * s

    def cubed: Scalar = s * s * s

    def tesseracted: Scalar = s * s * s * s

    def sqrt: Scalar = math.sqrt(s)

    def cbrt: Scalar = math.pow(s, 1.0 / 3.0)
  }


  implicit class LongPower(val s: Long) extends AnyVal {
    def fastPow(n: Long, p: Long): Long =
      if (p == 0) 1
      else if (p == 1) n
      else if ((p & 1) == 0) {
        val res = fastPow(n, p >> 1)
        res * res
      } else n * fastPow(n, p - 1)
    def ^^(p: Long): Long = fastPow(s, p)

    def squared: Long = s * s

    def cubed: Long = s * s * s

    def tesseracted: Long = s * s * s * s

  }


  implicit class WithAlmostEquals(val d: Scalar) extends AnyVal {
    def ~>=(d2: Scalar): Boolean = (d > d2) || (this ~= d2)

    def ~>(d2: Scalar): Boolean = (d > d2) && (this !~= d2)

    def ~<(d2: Scalar): Boolean = (d < d2) && (this !~= d2)

    def ~<=(d2: Scalar): Boolean = (d < d2) || (this ~= d2)

    def ~=(d2: Scalar): Boolean = (d - d2).abs <= percision

    def !~=(d2: Scalar): Boolean = !(this ~= d2)

    def inc: Scalar = d + 1
  }


  /**
   * safe scalar from [0,1]
   */
  case class UnitScalar private[math](value: Scalar) extends AnyVal {
    /** 1 - value */
    def inverse: Scalar = 1 - value
  }

  implicit class ToUnitScalar(val value: Scalar) extends AnyVal {
    def toUnitScalar: UnitScalar = UnitScalar(clamp(value, 0d, 1d))
  }

  implicit def unitScalarToScalar(uc: UnitScalar): Scalar = uc.value

  case class NonNegativeScalar private[math](value: Scalar) extends AnyVal

  implicit class ToPositiveScalar(val value: Scalar) extends AnyVal {
    def toNonNegative: NonNegativeScalar = NonNegativeScalar(abs(value))
  }

  implicit def nonNegativeScalarToScalar(uc: NonNegativeScalar): Scalar = uc.value


  implicit def toScalar[T](a: T)(implicit n: Numeric[T]): Scalar = n.toDouble(a)

  implicit def toScalar(n: Int): Scalar = n.toDouble

  implicit def toScalar(n: Float): Scalar = n.toDouble

  implicit def toScalar(n: Double): Scalar = n.toDouble

  implicit def toScalar(n: Long): Scalar = n.toDouble

  def compare(a: Scalar, b: Scalar): Int = if (a < b) -1 else if (a > b) 1 else 0


  //casts
  val Raycast: RaycastTrait = utils.math.space.intersections.Raycast

  val SegmentCast: SegmentCastTrait = utils.math.space.intersections.SegmentCast

  //maps

  val Maps: MapsTrait = utils.math.Maps

  //  val UnitMaps: UnitMaps = utils.math.UnitMaps
  //@inline implicit def doubleToScalar(d:Double):Scalar = d.toFloat
  //implicit def add_~=(d: Float): WithAlmostEquals = new WithAlmostEquals(d)
}


