package utils.math.space

import utils.math.{space, _}
import utils.math.planar.V2

object V3 extends V3Constants {

  def apply(s:Scalar): V3 = from(s)

  def apply[T](s:T)(implicit f:T => Scalar): V3 = from(s)

  def from(s:Scalar):V3 = V3.fromScalars(s,s,s)

  def fromScalars(x: Scalar, y: Scalar, z: Scalar): V3 = V3(x, y, z)

  @inline def combine(v1: V3, v2: V3, f: (Scalar, Scalar) => Scalar): V3 = V3(
    f(v1.x, v2.x),
    f(v1.y, v2.y),
    f(v1.z, v2.z)
  )

  def fromArray(a: Array[Scalar]): V3 = V3(a(0), a(1), a(2))

  def fromArray[T](a: Array[T])(implicit f:T => Scalar): V3 = V3(a(0), a(1), a(2))

  def toNormal(v: V3): Normal = if (v.lengthSquared ~= 1f) v else v.normalize
}

trait V3Constants {

  val x: V3 = V3(1f, 0f, 0f)
  val y: V3 = V3(0f, 1f, 0f)
  val z: V3 = V3(0f, 0f, 1f)
  val ZERO: V3 = V3(0f, 0f, 0f)
  val one: V3 = V3(1, 1, 1)

  /*
             /\ +Z  Forward
             |
             |
  +X Left   |
  <------------------->-X Right
            |
            |
           \/ - Z Backward

           Y+ Up  Y- Down
   */
  val forwardAxis: V3 = z
  val backwardAxis: V3 = -z
  val leftAxis: V3 = x
  val rightAxis: V3 = -x
  val upAxis: V3 = y
  val downAxis: V3 = -y

  val axises: Seq[V3] = Seq(x, y, z)

  val negativeAxises: Seq[V3] = Seq(-x, -y, -z)

  val positiveNegativeAxises: Seq[V3] = axises ++ negativeAxises

  val diagonals: Seq[V3] = for (
    i <- Seq(-1, 1);
    j <- Seq(-1, 1);
    k <- Seq(-1, 1)
  ) yield V3(i, j, k).normalize

  val axisesAndDiagonals: Seq[V3] = axises ++ diagonals
}

case class V3(x: Scalar, y: Scalar, z: Scalar) {

  @inline def toQuatWithFreeCoeff(freeCoeff: Scalar): Quat = space.Quat(freeCoeff, x, y, z)

  @inline def reciporalSafe: V3 = V3(
    1 / (if (x == 0) SMALL_NUMBER else x),
    1 / (if (y == 0) SMALL_NUMBER else y),
    1 / (if (z == 0) SMALL_NUMBER else z))

  @inline def map(f: Scalar => Scalar): V3 = V3(f(x), f(y), f(z))

  @inline def isZero: Boolean = this == V3.ZERO

  /** for indexing in cycles */
  @inline def apply(i: Int): Scalar = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"$i out of bounds of vector")
  }

  @inline def unary_- : V3 = opposite

  @inline def opposite: V3 = V3(-x, -y, -z)

  @inline def +(v: V3) = V3(x + v.x, y + v.y, z + v.z)

  @inline def -(v: V3) = V3(x - v.x, y - v.y, z - v.z)

  @inline def *(v: V3) = V3(x * v.x, y * v.y, z * v.z)

  @inline def *(s: Scalar) = V3(x * s, y * s, z * s)

  @inline def /(v: V3) = V3(x / v.x, y / v.y, z / v.z)

  @inline def -(): V3 = V3(-x, -y, -z)

  /** vector product */
  @inline def ^(v: V3) = V3(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)

  /** scalar product */
  @inline def **(v: V3): Scalar = x * v.x + y * v.y + z * v.z

  /** almost equals*/
  @inline def ~=(v: V3): Boolean = (x ~= v.x) && (y ~= v.y) && (z ~= v.z)

  /** fast safe normalization, doing stuff only if needed */
  @inline def normalize: V3 = {
    val l = lengthSquared
    if (l ~= 1f) return this
    if (l == 0) {
      return V3(0f, 0f, 0f)
    }
    return this / length
  }

  /**non oriented angle*/
  @inline def angle(v: V3): Scalar = acos(this ** v / (this.length * v.length))

  @inline def distance(v: V3): Scalar = (this - v).length

  @inline def distanceSquared(v: V3): Scalar = (this - v).lengthSquared

  //todo
  //def rotate(a: Scalar) = V3(x * math.cos(a).toScalar - y * math.sin(a).toScalar, x * math.sin(a).toScalar + y * math.cos(a).toScalar)

  @inline def lengthSquared: Scalar = this ** this

  @inline def length: Scalar = sqrt(lengthSquared)

  @inline def longerThan(ot: V3): Boolean = lengthSquared > ot.lengthSquared

  @inline def shorterThan(ot: V3): Boolean = lengthSquared < ot.lengthSquared

  override def toString: String = s"""V3($x, $y, $z)"""

  @inline def heightCord: Scalar = y //todo heightComponent

  /** creating 2d vector dropping x */
  @inline def dropX = V2(y, z)

  /** creating 2d vector dropping y */
  @inline def dropY = V2(x, z)

  /** creating 2d vector dropping z */
  @inline def dropZ = V2(x, y)

  @inline def replaceX(nx: Scalar) = V3(nx, y, z)

  @inline def replaceY(ny: Scalar) = V3(x, ny, z)

  @inline def replaceZ(nz: Scalar) = V3(x, y, nz)

  @inline def toSeq: Seq[Scalar] = Seq(x, y, z)

  @inline def toArray: Array[Scalar] = Array(x, y, z)

  /** project vector on other vector v ** ||ot||*/
  @inline def projectionOn(ot: V3): Scalar = this ** V3.toNormal(ot.normalize)

  @inline def toBasis(basis: (V3, V3, V3)): V3 = toBasis(basis._1, basis._2, basis._3)

  /** projecting vector onto given basis */
  @inline def toBasis(i: V3, j: V3, k: V3): V3 = V3(projectionOn(i), projectionOn(j), projectionOn(k))

  @inline def fromBasis(basis: (V3, V3, V3)): V3 = fromBasis(basis._1, basis._2, basis._3)

  /** (i * x) + (j * y) + (k * z)  */
  @inline def fromBasis(i: V3, j: V3, k: V3): V3 = (i * x) + (j * y) + (k * z)

  //@ENGINE CONVERSION todo
  @inline def toPlanar: V2 = V2(x, -z)

  @inline def collinear(other: V3): Boolean = (normalize ~= other.normalize) || (normalize ~= other.normalize.opposite)

  /** zeroing other axises except x */
  @inline def onlyX: V3 = V3(x, 0, 0)

  /** zeroing x axis */
  @inline def woX: V3 = V3(0, y, z)

  /** zeroing other axises except y */
  @inline def onlyY: V3 = V3(0, y, 0)

  /** zeroing y axis */
  @inline def woY: V3 = V3(x, 0, z)

  /** zeroing other axises except z */
  @inline def onlyZ: V3 = V3(0, 0, z)

  /** zeroing z axis */
  @inline def woZ: V3 = V3(x, y, 0)

  /** return this or -this based on scalar product */
  @inline def matchDirection(toMatch: V3): V3 = if (toMatch ** this > 0) this else -this

  @inline def forwardComponent: Scalar = projectionOn(V3.forwardAxis)

  @inline def backwardComponent: Scalar = projectionOn(V3.backwardAxis)

  @inline def leftComponent: Scalar = projectionOn(V3.leftAxis)

  @inline def rightComponent: Scalar = projectionOn(V3.rightAxis)

  @inline def upComponent: Scalar = projectionOn(V3.upAxis)

  @inline def downComponent: Scalar = projectionOn(V3.downAxis)

}

class UnitV3(v: V3) extends V3(v.normalize.x, v.normalize.y, v.normalize.z) {


}