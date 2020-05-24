package utils.math.misc

import utils.math.{Scalar, misc}

/**
  * https://www.paulinternet.nl/?page=bicubic
  */
object CubicInterpolation {

  /** x from [0, 1] interpolates between [p1, p2]  */
  @inline def apply(p0: Scalar, p1: Scalar, p2: Scalar, p3: Scalar, x: Scalar): Scalar =
    p1 + 0.5 * x * (p2 - p0 + x * (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3 + x * (3.0 * (p1 - p2) + p3 - p0)));

  /** x from [0, 1] interpolates between [p1, p2]  */
  @inline def apply(p: Array[Scalar], x: Scalar): Scalar = apply(p(0), p(1), p(2), p(3), x)

  @inline def apply(p: (Scalar, Scalar, Scalar, Scalar), x: Scalar): Scalar = apply(p._1, p._2, p._3, p._4, x)
}

object BicubicInterpolation {
  @inline def apply(p: Array[Array[Scalar]], x: Scalar, y: Scalar): Scalar = {
    val a0 = CubicInterpolation(p(0), y)
    val a1 = CubicInterpolation(p(1), y)
    val a2 = CubicInterpolation(p(2), y)
    val a3 = CubicInterpolation(p(3), y)
    misc.CubicInterpolation(a0, a1, a2, a3, x)
  }

  /** interpolates (x, y) <- [0,1]x[0,1]  values from [p11,p21]x[p21, p22]    */
  @inline def apply(
                     p00: Scalar, p10: Scalar, p20: Scalar, p30: Scalar,
                     p01: Scalar, p11: Scalar, p21: Scalar, p31: Scalar,
                     p02: Scalar, p12: Scalar, p22: Scalar, p32: Scalar,
                     p03: Scalar, p13: Scalar, p23: Scalar, p33: Scalar,
                     x: Scalar, y: Scalar
                   ): Scalar = {
    val a0 = CubicInterpolation(p00, p01, p02, p03, y)
    val a1 = CubicInterpolation(p10, p11, p12, p13, y)
    val a2 = CubicInterpolation(p20, p21, p22, p23, y)
    val a3 = CubicInterpolation(p30, p31, p32, p33, y)
    misc.CubicInterpolation(a0, a1, a2, a3, x)
  }
}

object TricubicInterpolation {
  def apply(p: Array[Array[Array[Scalar]]], x: Scalar, y: Scalar, z: Scalar): Scalar = {
    val a0 = BicubicInterpolation(p(0), y, z)
    val a1 = BicubicInterpolation(p(1), y, z)
    val a2 = BicubicInterpolation(p(2), y, z)
    val a3 = BicubicInterpolation(p(3), y, z)
    misc.CubicInterpolation(a0, a1, a2, a3, x)
  }

}

