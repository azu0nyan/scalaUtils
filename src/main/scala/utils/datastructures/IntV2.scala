package utils.datastructures

import utils.math.planar.V2

object IntV2 {

  def clamp(x: IntV2, min: IntV2, max: IntV2): IntV2 = IntV2(utils.math.clamp(x.x, min.x, max.x), utils.math.clamp(x.y, min.y, max.y))
  //  implicit def toV2(v:IntV2):V2 = v.toV2

  implicit def toIntV2(x: Int): IntV2 = IntV2(x, x)

  implicit def toIntV2(x: (Int, Int)): IntV2 = IntV2(x._1, x._2)

  implicit def fromIntV2(x: IntV2): (Int, Int) = (x.i, x.j)

  /** for storing grid in 1D array */
  def toFlatIndex(index: IntV2, dims: IntV2): Int = (index.i * dims.j) + index.j

  def fromFlatIndex(flatIndex: Int, dims: IntV2): IntV2 = IntV2(flatIndex / dims.j, flatIndex % dims.j)

  def apply(z:Int):IntV2 = IntV2(z, z)
}

case class IntV2(i: Int, j: Int) {

  def x: Int = i

  def y: Int = j

  def area: Int = i * j

  def opposite: IntV2 = IntV2(-i, -j)

  def unary_- : IntV2 = opposite

  def +(v: IntV2): IntV2 = IntV2(i + v.i, j + v.j)

  def -(v: IntV2): IntV2 = IntV2(i - v.i, j - v.j)

  def *(v: IntV2): IntV2 = IntV2(i * v.i, j * v.j)

  def /(v: IntV2): IntV2 = IntV2(i / v.i, j / v.j)

  def clampCircullar(resolution: IntV2): IntV2 = IntV2(utils.math.circullarIndex(i, resolution.i), utils.math.circullarIndex(j, resolution.j))

  def toV2: V2 = V2(i, j)

  override def toString: String = s"[$i, $j]"
}
