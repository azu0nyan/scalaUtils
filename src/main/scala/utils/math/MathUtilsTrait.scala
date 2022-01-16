package utils.math

import utils.datastructures.IntV2
import utils.math.misc.{Interval, IntervalT}
import utils.math.planar.V2
import utils.math.space.Rotations.Rotation
import utils.math.space.{Quat, V3}

import scala.util.Random

trait MathUtilsTrait {

  @inline def atan2(y: Scalar, x: Scalar): Scalar = math.atan2(y, x)

  @inline def sqrt(lengthSquared: Scalar): Scalar = math.sqrt(lengthSquared)

  @inline def exp(v: Scalar): Scalar = math.exp(v)

  /** natural logarithm  */
  @inline def log_e(v: Scalar): Scalar = math.log(v)

  @inline def log2(v: Scalar): Scalar = logab(v, 2)

  /** a-value b-base, changing base to e => log_b(a) = log_e(a)/log_e(b) */
  @inline def logab(value: Scalar, base: Scalar): Scalar = log_e(value) / log_e(base)

  @inline def asin(v: Scalar): Scalar = math.asin(v)

  @inline def acos(v: Scalar): Scalar = math.acos(v)

  @inline def sin(v: Scalar): Scalar = math.sin(v) //FastMath.sin(v)

  @inline def cos(v: Scalar): Scalar = math.cos(v) //FastMath.cos(v)

  @inline def min(a: Scalar, b: Scalar): Scalar = math.min(a, b)

  @inline def max(a: Scalar, b: Scalar): Scalar = math.max(a, b)

  @inline def floor(s: Scalar): Int = math.floor(s).toInt

  @inline def round(s: Scalar): Int = math.round(s).toInt

  @inline def ceil(s: Scalar): Int = math.ceil(s).toInt

  @inline def abs(s: Scalar): Scalar = math.abs(s)

  @inline def pow(a: Scalar, b: Scalar): Scalar = math.pow(a, b)

  def eqWithDelta(a:Scalar, b:Scalar, delta:Scalar):Boolean = abs(a - b) <= delta

  def min(vals: Scalar*): Scalar = vals.min

  def max(vals: Scalar*): Scalar = vals.max



  /** creates vector where each component is minimum of corresponding components from v1 and v2 */
  def minCords(v1: V3, v2: V3): V3 = V3.combine(v1, v2, min)

  /** creates vector where each component is maximum of corresponding components from v1 and v2 */
  def maxCords(v1: V3, v2: V3): V3 = V3.combine(v1, v2, max)

  /** return number from [0, length) */
  def circullarIndex(element: Int, length: Int): Int = {
    if (length == 0) return 0
    if (element >= 0) {
      element % length
    } else {
      (element % length) + length
    }
  }

  //Rands

  def randInRange(a: Scalar, b: Scalar, r: Random = new Random()): Scalar = {
    val min = scala.math.min(a, b)
    val max = scala.math.max(a, b)
    val range = max - min
    r.nextDouble() * range + min
  }

  /**
   * @param a from inclusive
   * @param b to inclusive
   */
  def randIntInRangeInclusive(a: Int, b: Int, r: Random = new Random()): Int = {
    val min: Int = scala.math.min(a, b)
    val max: Int = scala.math.max(a, b)
    //    val range = max - min
    r.nextInt(max - min + 1) + min
  }

  def randV3InRange(min: V3, max: V3, r: Random = new Random()): V3 = {
    V3(randInRange(min.x, max.x, r), randInRange(min.y, max.y, r), randInRange(min.z, max.z, r))
  }

  def randPlanarDirection(minAngle: Scalar = 0, maxAngle: Scalar = TWO_PI, r: Random = new Random()): V2 = V2.ox.rotate(randInRange(minAngle, maxAngle))

  def randV2InRange(min: V2, max: V2, r: Random = new Random()): V2 = {
    V2(randInRange(min.x, max.x, r), randInRange(min.y, max.y, r))
  }

  def randRotation(r: Random = new Random()): Rotation = {
    Quat(r.nextFloat(), r.nextFloat(), r.nextFloat(), r.nextFloat()).normalized
  }

  //mix
  @inline def mix(x:Scalar, y:Scalar, a:Scalar):Scalar = x *(1 - a) + y * a

  @inline def mixV2(x:V2, y:V2, a:Scalar):V2 = V2(
    mix(x.x, y.x, a),
    mix(x.y, y.y, a)
  )

  @inline def mixV3(x:V3, y:V3, a:Scalar):V3 = V3(
    mix(x.x, y.x, a),
    mix(x.y, y.y, a),
    mix(x.z, y.z, a),
  )


  //Clamps
  
  @inline def snapToClosest(x: Scalar, snapping: Scalar):Scalar = {
    floor((x + snapping / 2) / snapping) * snapping    
  }
  
  @inline def snapToClosest(x: V2, snapping: Scalar):V2 = V2(snapToClosest(x.x, snapping), snapToClosest(x.y, snapping))

  def clamp(x: V3, min: V3, max: V3): V3 = V3(clamp(x.x, min.x, max.x), clamp(x.y, min.y, max.y), clamp(x.z, min.y, max.z))

  def clamp(x: V2, min: V2, max: V2): V2 = V2(clamp(x.x, min.x, max.x), clamp(x.y, min.y, max.y))

  @inline def clamp(x: Scalar, min: Scalar, max: Scalar): Scalar = if(x < min) min else if(x > max) max else x

  @inline def clamp(x: Int, min: Int, max: Int): Int = if(x < min) min else if(x > max) max else x

  def clampSnap(x: V2, min: V2, max: V2): V2 = V2(clampSnap(x.x, min.x, max.x), clampSnap(x.y, min.y, max.y))

  /** clamps x between min and max, and snaps it to min or max if x close to them */
  def clampSnap(x: Scalar, min: Scalar, max: Scalar): Scalar =
    if (clamp(x, min, max) ~= min) min
    else if (clamp(x, min, max) ~= max) max
    else clamp(x, min, max)

  def clampRepeat(x: Scalar, min: Scalar, max: Scalar): Scalar = {
    if (x < min) {
      max - ((min - x) % (max - min))
    } else if (x > max) {
      min + ((x - max) % (max - min))
    } else x
  }

  def clampRepeat(x:V2, min:V2, max:V2):V2 = V2(clampRepeat(x.x, min.x, max.x), clampRepeat(x.y, min.y, max.y))

  def clampRepeat(x:V3, min:V3, max:V3):V3 = V3(clampRepeat(x.x, min.x, max.x), clampRepeat(x.y, min.y, max.y), clampRepeat(x.z, min.z, max.z))

  val unitInterval: IntervalT[Scalar] = IntervalT[Scalar](ZERO, ONE)

  /////////////////////////////Interpolations//////////////////////////
  trait Lerp[T] {
    def lerp: (T, T, Scalar) => T = apply

    def apply(t1: T, t2: T, alpha: Scalar): T

    def bilerp(t00: T, t10: T, t01: T, t11: T, xAlpha: Scalar, yAlpha: Scalar): T =
      lerp(
        lerp(t00, t10, xAlpha),
        lerp(t01, t11, xAlpha),
        yAlpha
      )
  }

  implicit val scalarLerp: Lerp[Scalar] = ScalarLerp

  object ScalarLerp extends Lerp[Scalar] {
    override def apply(t1: Scalar, t2: Scalar, alpha: Scalar): Scalar = utils.math.lerpUnit(t1, t2, alpha)
  }

  implicit val v3Lerp: Lerp[V3] = V3Lerp

  object V3Lerp extends Lerp[V3] {
    override def apply(t1: V3, t2: V3, alpha: Scalar): V3 = V3(
      scalarLerp(t1.x, t2.x, alpha),
      scalarLerp(t1.y, t2.y, alpha),
      scalarLerp(t1.z, t2.z, alpha)
    )
  }

  implicit val v2Lerp: Lerp[V2] = V2Lerp

  object V2Lerp extends Lerp[V2] {
    override def apply(t1: V2, t2: V2, alpha: Scalar): V2 = V2(
      scalarLerp(t1.x, t2.x, alpha),
      scalarLerp(t1.y, t2.y, alpha)
    )
  }

  val quatFastLerp: Lerp[Quat] = Quat.fastLerpNotNormalized

  val quatSLerpNormalized: Lerp[Quat] = Quat.slerpNomalized


  /** maps interval to interval */
  def lerp(from: Interval, to: Interval): Scalar => Scalar = lerp(from.start, to.start, from.end, to.end, _)

  def lerp(x1: Scalar, x1Value: Scalar, x2: Scalar, x2Value: Scalar, point: Scalar): Scalar = {
    val diff = x2 - x1
    if (diff == 0f) x1Value
    else x1Value * (x2 - point) / diff + x2Value * (point - x1) / diff
  }

  def lerpUnit(f0: Scalar, f1: Scalar, x: Scalar): Scalar = f0 * (1 - x) + f1 *  x

  def bilerpUnit(x1: Scalar, y1: Scalar,
                 x2: Scalar, y2: Scalar,
                 q11: Scalar, q12: Scalar,
                 q21: Scalar, q22: Scalar,
                 x: Scalar, y: Scalar): Scalar = {
    val fxy1 = lerp(x1, q11, x2, q21, x)
    val fxy2 = lerp(x1, q12, x2, q22, x)
    lerp(y1, fxy1, y2, fxy2, y)
  }

  def bilerpUnit(f00: Scalar, f01: Scalar,
                 f10: Scalar, f11: Scalar,
                 x: Scalar, y: Scalar
                ): Scalar = f00 * (1 - x) * (1 - y) + f01 * (1 - x) * y + f10 * x * (1 - y) + f11 * x * y


  def fifthFade(t: Scalar): Scalar = (6 * t * t * t * t * t) - (15 * t * t * t * t) + (10 * t * t * t)

  def fastfloor(x: Double): Int = if (x > 0) x.toInt
  else x.toInt - 1

  /**
   * conversion from spherical coordinates to world coordinates
   *
   * @param radius      0 to inf
   * @param inclination 0 to PI
   * @param azimuth     0 to 2PI
   */
  @inline def fromSphericalCoordinates(radius: Scalar, inclination: Angle, azimuth: Angle): V3 =
    V3(
      radius * sin(inclination) * cos(azimuth),
      radius * sin(inclination) * sin(azimuth),
      radius * cos(inclination)
    )

  //combinatorics
  /** up to 100 */
  val maxPascalTriangleSize = 100;
  val pascalTriangle: Array[Array[Long]] = {
    val res: Array[Array[Long]] = new Array[Array[Long]](maxPascalTriangleSize)
    for (i <- 0 until maxPascalTriangleSize) {
      res(i) = new Array[Long](i + 1)
      res(i)(0) = 1
      res(i)(i) = 1
    }
    for (i <- 2 until maxPascalTriangleSize;
         j <- 1 until i) {
      res(i)(j) = res(i - 1)(j - 1) + res(i - 1)(j)
    }
    res
  }

  def binomial(n: Int, k: Int): Long = pascalTriangle(n)(k)

  /** Ближайшая степень двойки меньше равная аргументу */
  def greatestLEQ2Power(s: NonNegativeScalar): Long =
    if (s.value == 0) return 0
    else if (s.value > (1L << 62)) return 1L << 62
    else {
      var pow2: Long = 1
      while (pow2 * 2 <= s) pow2 *= 2
      return pow2
    }

  /** Ближайшая степень двойки больше равная аргументу */
  def lowestGEQ2Power(s: NonNegativeScalar): Long =
    if (s.value == 0) return 0
    else if (s.value > (1L << 62)) return 1L << 62
    else {
      var pow2: Long = 1
      while (pow2 < s) pow2 *= 2
      return pow2
    }

}
