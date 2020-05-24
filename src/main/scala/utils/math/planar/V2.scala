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



  //@ENGINE CONVERSION


  //implicit def fromVector2D(v: Vector2D): V2 = V2(v.getX.toFloat, v.getY.toFloat)

  //  implicit def toVector2d(v: V2): Vector2D = new Vector2D(v.x, v.y)

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


  def apply(i: Int): Scalar = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"$i out of bounds of vector")
  }

  def xInt:Int = x.toInt

  def yInt:Int = y.toInt

  def unary_- : V2 = opposite

  def opposite: V2 = V2(-x, -y)

  def +(v: V2): V2 = V2(x + v.x, y + v.y)

  def -(v: V2): V2 = V2(x - v.x, y - v.y)

  def *(v: V2): V2 = V2(x * v.x, y * v.y)

  def *(s:Scalar): V2 = V2(x * s, y * s)

  def /(v: V2): V2 = V2(x / v.x, y / v.y)

  def -(): V2 = V2(-x, -y)

  def **(v: V2): Scalar = x * v.x + y * v.y

  def det(v: V2): Scalar = x * v.y - y * v.x

  def ~=(v: V2): Boolean = (x ~= v.x) && (y ~= v.y)

  def normalize: V2 = if (length == 0) {
    V2(0, 0)
  } else {
    /(length)
  }

  def angle(v: V2): Scalar = (math.atan2(v.y, v.x) - math.atan2(y, x))

  def distance(v: V2): Scalar = (this - v).length

  def rotate90CW:V2 = V2(-y, x)

  def rotate90CCW:V2 = V2(y, -x)

  def rotate(a: Scalar):V2 = V2(x * cos(a) - y * sin(a), x * sin(a) + y * cos(a))

  def rotateAroundPoint(rotation: Scalar, point: V2): V2 = (this - point).rotate(rotation) + point

  def scaleAroundPoint(scale: Scalar, point: V2): V2 = (this - point) * scale + point

  def lengthSquared:Scalar = this ** this

  def length: Scalar = math.hypot(x, y)

  override def toString: String = s"""V2($x, $y)"""

  def addZ(z: Scalar):V3 = V3(x, y, z)

  def addX(X: Scalar):V3 = V3(X, x, y)

  def addY(Y: Scalar):V3 = V3(x, Y, y)

  def toSeq: Seq[Scalar] = Seq(x, y)

  //@ENGINE CONVERSION  todo
  def planarToV3(upCord: Scalar): V3 = V3(x, upCord, -y)

  def clamp: (V2, V2) => V2 = utils.math.clamp(this, _, _)

  def clampSnap: (V2, V2) => V2 = utils.math.clampSnap(this, _, _)

  def toIntV2: IntV2 = IntV2(x.toInt, y.toInt)

  def collinear(other: V2): Boolean = (normalize ~= other.normalize) || (normalize ~= other.normalize.opposite)
}

class UnitV2(v: V2) extends V2(v.normalize.x, v.normalize.y)