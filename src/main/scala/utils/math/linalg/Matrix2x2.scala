package utils.math.linalg

import utils.math.linalg.Matrix.{Matrix, SquareMatrix}
import utils.math.planar.V2
import utils.math.{Scalar, linalg}
import utils.math._
object Matrix2x2 {
  val id: Matrix2x2 = linalg.Matrix2x2(
    1f, 0f,
    0f, 1f
  )
}

case class Matrix2x2(m00: Scalar, m01: Scalar,
                     m10: Scalar, m11: Scalar) extends SquareMatrix {

  @inline override val size: Int = 2

  @inline override def valueAt(row: Int, column: Int): Scalar = (row, column) match {
    case (0, 0) => m00
    case (0, 1) => m01
    case (1, 0) => m10
    case (1, 1) => m11
    case _ => throw new IndexOutOfBoundsException(s"wrong matrix index ${(row, column)} should be between (0, 0) and (1, 1)")
  }


  @inline def *(ot: Matrix2x2): Matrix2x2 = Matrix2x2(
    m00 * ot.m00 + m01 * ot.m10, m00 * ot.m01 + m01 * ot.m11,
    m10 * ot.m00 + m11 * ot.m10, m10 * ot.m01 + m11 * ot.m11
  )

  @inline def *(v: V2): V2 =
    V2(
      v.x * m00 + v.y * m01,
      v.x * m10 + v.y * m11)

  @inline def *(f: Scalar): Matrix2x2 = Matrix2x2(
    m00 * f, m01 * f, m10 * f, m11 * f
  )

  @inline def ===(ot: Matrix2x2): Boolean = m00 == ot.m00 && m01 == ot.m01 && m10 == ot.m10 && m11 == ot.m11

  @inline def ~=(ot: Matrix2x2): Boolean = (m00 ~= ot.m00) && (m01 ~= ot.m01) && (m10 ~= ot.m10) && (m11 ~= ot.m11)

  @inline def hasInverse: Boolean = det != 0

  @inline def inverseSame: Option[Matrix2x2] = Option.when(hasInverse)(inverseUnsafe)

  @inline override def inverseUnsafe: Matrix2x2 =
    Matrix2x2(m11 / det, -m01 / det, -m10 / det, m00 / det)

  @inline override def inverse: Option[Matrix] = Option.when(det != 0)(inverseUnsafe)

  @inline override def transposed: Matrix2x2 = Matrix2x2(m00, m10, m01, m11)

  @inline override val det: Scalar = m00 * m11 - m10 * m11

  @inline def adj: Matrix2x2 = Matrix2x2(m11, -m01, -m10, m00)

}


