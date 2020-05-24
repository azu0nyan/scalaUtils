package utils.datastructures

import utils.math.planar.V2

object IntV2 {
//  implicit def toV2(v:IntV2):V2 = v.toV2

  implicit def toIntV2(x: Int): IntV2 = IntV2(x, x)

  implicit def toIntV2(x: (Int, Int)): IntV2 = IntV2(x._1, x._2)

  implicit def fromIntV2(x: IntV2): (Int,Int) = (x.i, x.j)
}

case class IntV2(i: Int, j: Int) {
  def x:Int  = i

  def y:Int  = j

  def area:Int = i * j

  def opposite: IntV2 = IntV2(-i, -j)

  def unary_- : IntV2 = opposite

  def +(v: IntV2):IntV2 = IntV2(i + v.i, j + v.j)

  def -(v: IntV2): IntV2 = IntV2(i - v.i, j - v.j)

  def *(v: IntV2): IntV2 = IntV2(i * v.i, j * v.j)

  def /(v: IntV2): IntV2 = IntV2(i / v.i, j / v.j)

  def clampCircullar(resolution: IntV2): IntV2 = IntV2(utils.math.circullarIndex(i, resolution.i), utils.math.circullarIndex(j, resolution.j))

  def toV2: V2 = V2(i, j)

  override def toString: String = s"[$i, $j]"
}
