package utils.datastructures

import utils.math.space.V3

object IntV3 {
  //  implicit def toV3(v:IntV3):V3 = v.toV3

  implicit def toIntV3(x: Int): IntV3 = IntV3(x, x, x)

  implicit def toIntV3(x: (Int, Int, Int)): IntV3 = IntV3(x._1, x._2, x._3)

  implicit def fromIntV3(x: IntV3): (Int, Int, Int) = (x.i, x.j, x.k)
}

case class IntV3(i: Int, j: Int, k: Int) {

  @inline def volume: Int = (i + 1) * (j + 1) * (k + 1)

  @inline def x: Int = i

  @inline def y: Int = j

  @inline def z: Int = k

  @inline def opposite: IntV3 = IntV3(-i, -j, -k)

  @inline def +(v: IntV3) = IntV3(i + v.i, j + v.j, k + v.k)

  @inline def -(v: IntV3) = IntV3(i - v.i, j - v.j, k - v.k)

  @inline def *(v: IntV3) = IntV3(i * v.i, j * v.j, k * v.k)

  @inline def /(v: IntV3) = IntV3(i / v.i, j / v.j, k / v.k)

  @inline def clampCircullar(resolution: IntV3): IntV3 = IntV3(utils.math.circullarIndex(i, resolution.i), utils.math.circullarIndex(j, resolution.j),utils.math.circullarIndex(k, resolution.k))

  @inline def toV3: V3 = V3(i, j, k)

  override def toString: String = s"[$i, $j, $k]"
}
