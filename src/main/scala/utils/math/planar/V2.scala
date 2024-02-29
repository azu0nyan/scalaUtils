package utils.math.planar

import utils.abstractions.Monoid
import utils.datastructures.IntV2
import utils.math.space.V3
import utils.math._
import utils.system.WithMap

object V2 {
  def apply(s:Scalar): V2 = from(s, s)

  def from(x:Scalar, y:Scalar) :V2 = V2(x, y)

  implicit def pairToV2(p: (Scalar, Scalar)): V2 = V2(p._1, p._2)

  implicit def fToV2(f: Scalar): V2 = V2(f, f)


  implicit def toUnit(v: V2): UnitV2 = new UnitV2(v)

  val ox: V2 = V2(1, 0)

  val oy: V2 = V2(0, 1)

  val ZERO : V2= V2(0, 0)

  implicit  val V2additionMonoid: Monoid[V2] = new Monoid[V2] {
    override def empty: V2 = ZERO

    override def combine(x: V2, y: V2): V2 = x + y
  }



}

case class V2(x: Scalar, y: Scalar) extends WithMap{
  
  
  

  def toProduct:(Scalar, Scalar) = (x, y)

  @inline def reciporalSafe: V2 = V2(
    1 / (if (x == 0) SMALL_NUMBER else x),
    1 / (if (y == 0) SMALL_NUMBER else y),
 )


  @inline def apply(i: Int): Scalar = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"$i out of bounds of vector")
  }

  @inline def xInt:Int = x.toInt

  @inline def yInt:Int = y.toInt

  @inline def unary_- : V2 = opposite

  @inline def opposite: V2 = V2(-x, -y)

  @inline def +(v: V2): V2 = V2(x + v.x, y + v.y)

  @inline def -(v: V2): V2 = V2(x - v.x, y - v.y)

  @inline def *(v: V2): V2 = V2(x * v.x, y * v.y)

  @inline def *(s:Scalar): V2 = V2(x * s, y * s)

  @inline def /(v: V2): V2 = V2(x / v.x, y / v.y)

  @inline def -(): V2 = V2(-x, -y)

  @inline def **(v: V2): Scalar = x * v.x + y * v.y

  @inline def det(v: V2): Scalar = x * v.y - y * v.x

  @inline def ~=(v: V2): Boolean = (x ~= v.x) && (y ~= v.y)

  @inline def normalize: V2 = if (length == 0) {
    V2(0, 0)
  } else {
    /(length)
  }

  @inline def angleToOX: Scalar =  math.atan2(y, x)

  @inline def angle(v: V2): Scalar = (math.atan2(v.y, v.x) - math.atan2(y, x))

  @inline def angleClampedToPi(v: V2): Scalar = {
    var a = angle(v)
    a =  a % TWO_PI
    a = (a + TWO_PI) % TWO_PI;
    if (a > PI) a - TWO_PI
    else a
  }

  /**Y - up, returns from - PI to PI*/
  def angleCCW(ot: V2): Scalar = atan2(det(ot), this ** ot)

  def angleCCW0to2PI(ot: V2): Scalar = {
    val angle = angleCCW(ot)
    if(angle >= 0) angle
    else angle + TWO_PI
  }


  @inline def distance(v: V2): Scalar = (this - v).length

  @inline def rotate90CCW:V2 = V2(-y, x)

  @inline def rotate90CW:V2 = V2(y, -x)

  @inline def rotate(a: Scalar):V2 = V2(x * cos(a) - y * sin(a), x * sin(a) + y * cos(a))

  @inline   def rotateAroundPoint(rotation: Scalar, point: V2): V2 = (this - point).rotate(rotation) + point

  @inline def scaleAroundPoint(scale: Scalar, point: V2): V2 = (this - point) * scale + point

  @inline def lengthSquared:Scalar = this ** this

  @inline def length: Scalar = math.hypot(x, y)

  override def toString: String = s"""V2($x, $y)"""

  def toShortString: String = f"${toShortStr(x)}, ${toShortStr(y)}"

  @inline def addZ(z: Scalar):V3 = V3(x, y, z)

  @inline def addX(X: Scalar):V3 = V3(X, x, y)

  @inline def addY(Y: Scalar):V3 = V3(x, Y, y)

  @inline def toSeq: Seq[Scalar] = Seq(x, y)
  
  @inline def negateX: V2 = V2(-x, y)
  
  @inline def negateY: V2 = V2(x, -y)

  @inline def planarToV3(upCord: Scalar): V3 = V3(x, upCord, -y)

  @inline def clamp: (V2, V2) => V2 = utils.math.clamp(this, _, _)

  @inline def clampSnap: (V2, V2) => V2 = utils.math.clampSnap(this, _, _)

  @inline def toIntV2: IntV2 = IntV2(x.toInt, y.toInt)

  @inline def collinear(other: V2): Boolean = (normalize ~= other.normalize) || (normalize ~= other.normalize.opposite)

  //todo optimize and check
//  @inline def sameDirection(other: V2): Boolean = y == 0 && other.y == 0 || (x / y ~= other.x / other.y)
  @inline def sameDirection(other: V2): Boolean = normalize ~= other.normalize


}

class UnitV2(v: V2) extends V2(v.normalize.x, v.normalize.y)