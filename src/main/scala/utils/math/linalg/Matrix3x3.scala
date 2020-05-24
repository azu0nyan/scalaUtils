package utils.math.linalg

import utils.math.linalg.Matrix.SquareMatrix
import utils.math.space.V3
import utils.math.{Scalar, linalg}
import utils.math._

object Matrix3x3 {
  val id: Matrix3x3 = linalg.Matrix3x3(
    1d, 0d,0d,
    0d, 1d,0d,
    0d, 0d,1d
  )
}



case class Matrix3x3(
                      m00: Scalar, m01: Scalar, m02: Scalar,
                      m10: Scalar, m11: Scalar, m12: Scalar,
                      m20: Scalar, m21: Scalar, m22: Scalar,
                    ) extends SquareMatrix {
  override val size: Int = 3

  override def valueAt(row: Int, column: Int): Scalar = (row, column) match {
    case (0, 0) => m00
    case (0, 1) => m01
    case (0, 2) => m02
    case (1, 0) => m10
    case (1, 1) => m11
    case (1, 2) => m12
    case (2, 0) => m20
    case (2, 1) => m21
    case (2, 2) => m22
    case _ => throw new IndexOutOfBoundsException(s"wrong matrix index ${(row, column)} should be between (0, 0) and (2, 2)")
  }

  @inline def *(ot: Matrix3x3): Matrix3x3 = Matrix3x3(
    m00 * ot.m00 + m01 * ot.m10 + m02 * ot.m20, m00 * ot.m01 + m01 * ot.m11 + m02 * ot.m21, m00 * ot.m02 + m01 * ot.m12 + m02 * ot.m22,
    m10 * ot.m00 + m11 * ot.m10 + m12 * ot.m20, m10 * ot.m01 + m11 * ot.m11 + m12 * ot.m21, m10 * ot.m02 + m11 * ot.m12 + m12 * ot.m22,
    m20 * ot.m00 + m21 * ot.m10 + m22 * ot.m20, m20 * ot.m01 + m21 * ot.m11 + m22 * ot.m21, m20 * ot.m02 + m21 * ot.m12 + m22 * ot.m22
  )

  @inline def *(v: V3): V3 = V3(
    v.x * m00 + v.y * m01 + v.z * m02,
    v.x * m10 + v.y * m11 + v.z * m12,
    v.x * m20 + v.y * m21 + v.z * m22
  )

  @inline def *(f: Scalar): Matrix3x3 = Matrix3x3(
    m00 * f, m01 * f, m02 * f,
    m10 * f, m11 * f, m12 * f,
    m20 * f, m21 * f, m22 * f
  )

  @inline def ===(ot: Matrix3x3): Boolean =
    m00 == ot.m00 && m01 == ot.m01 && m02 == ot.m02 &&
      m10 == ot.m10 && m11 == ot.m11 && m12 == ot.m12 &&
      m20 == ot.m20 && m21 == ot.m21 && m22 == ot.m22

  @inline def ~=(ot: Matrix3x3): Boolean =
    (m00 ~= ot.m00) && (m01 ~= ot.m01) && (m02 ~= ot.m02) &&
      (m10 ~= ot.m10) && (m11 ~= ot.m11) && (m12 ~= ot.m12) &&
      (m20 ~= ot.m20) && (m21 ~= ot.m21) && (m22 ~= ot.m22)

  @inline def hasInverse: Boolean = det != 0

  @inline override def inverse: Option[Matrix3x3] = Option.when(hasInverse)(inverseUnsafe)

  @inline override def  inverseUnsafe: Matrix3x3 = {
    val invdet = 1d / det
    Matrix3x3(
      (m11 * m22 - m12 * m21) * invdet,
      (m02 * m21 - m01 * m22) * invdet,
      (m01 * m12 - m02 * m11) * invdet,
      (m12 * m20 - m10 * m22) * invdet,
      (m00 * m22 - m02 * m20) * invdet,
      (m02 * m10 - m00 * m12) * invdet,
      (m10 * m21 - m11 * m20) * invdet,
      (m01 * m20 - m00 * m21) * invdet,
      (m00 * m11 - m01 * m10) * invdet)
  }

  @inline override def transposed: Matrix3x3 = Matrix3x3(
    m00, m10, m20,
    m01, m11, m21,
    m02, m12, m22
  )

  @inline override lazy val  det: Scalar =
    m00*m11*m22 + m01*m12*m20 + m02*m10*m21 - m00*m12*m21 - m01*m10*m22 - m02*m11*m20

}

