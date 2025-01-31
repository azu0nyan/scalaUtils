package utils.math.linalg

import utils.math.linalg.Matrix.SquareMatrix
import utils.math.space.Rotations.Rotation
import utils.math.space.{Rotations, V3, V4}
import utils.math.*

object Matrix4x4 {

  val id: Matrix4x4 = linalg.Matrix4x4(
    1f, 0f, 0f, 0f,
    0f, 1f, 0f, 0f,
    0f, 0f, 1f, 0f,
    0f, 0f, 0f, 1f
  )

  def genByRule(rule: (Int, Int) => Scalar): Matrix4x4 = linalg.Matrix4x4(
    rule(0, 0), rule(0, 1), rule(0, 2), rule(0, 3),
    rule(1, 0), rule(1, 1), rule(1, 2), rule(1, 3),
    rule(2, 0), rule(2, 1), rule(2, 2), rule(2, 3),
    rule(3, 0), rule(3, 1), rule(3, 2), rule(3, 3)
  )

  def translation(translation: V3): Matrix4x4 = linalg.Matrix4x4(
    1f, 0f, 0f, translation.x,
    0f, 1f, 0f, translation.y,
    0f, 0f, 1f, translation.z,
    0f, 0f, 0f, 1f
  )

  def scale(scale: V3): Matrix4x4 = linalg.Matrix4x4(
    scale.x, 0f, 0f, 0f,
    0f, scale.y, 0f, 0f,
    0f, 0f, scale.z, 0f,
    0f, 0f, 0f, 1f
  )

  def xRoll(xRoll: Scalar): Matrix4x4 = {
    val c = cos(xRoll)
    val s = sin(xRoll)
    linalg.Matrix4x4(
      1f, 0f, 0f, 0f,
      0f, c, -s, 0f,
      0f, s, c, 0f,
      0f, 0f, 0f, 1f
    )
  }

  def yRoll(xRoll: Scalar): Matrix4x4 = {
    val c = cos(xRoll)
    val s = sin(xRoll)
    linalg.Matrix4x4(
      c, 0f, s, 0f,
      0f, 1f, 0f, 0f,
      -s, 0f, c, 0f,
      0f, 0f, 0f, 1f
    )
  }

  def zRoll(xRoll: Scalar): Matrix4x4 = {
    val c = cos(xRoll)
    val s = sin(xRoll)
    linalg.Matrix4x4(
      c, -s, 0f, 0f,
      s, c, 0f, 0f,
      0f, 0f, 1f, 0f,
      0f, 0f, 0f, 1f
    )


  }

  def rotationMatrix(r: Rotation): Matrix4x4 = {
    val euler = Rotations.toEulerAngles(r)
    rotationMatrix(euler.x, euler.y, euler.z)
  }

  /** this conversion uses NASA standard aeroplane conventions as described on page:
    * https://www.euclideanspace.com/maths/geometry/rotations/euler/index.htm
    * Coordinate System: right hand
    * Positive angle: right hand
    * Order of euler angles: heading first, then attitude, then bank
    * matrix row column ordering:
    * [m00 m01 m02]
    * [m10 m11 m12]
    * [m20 m21 m22] */
  def rotationMatrix(xRoll: Scalar, yRoll: Scalar, zRoll: Scalar): Matrix4x4 = {
    val attitude = xRoll
    val heading = yRoll
    val bank = zRoll

    // Assuming the angles are in radians.
    val ch = cos(heading)
    val sh = sin(heading)
    val ca = cos(attitude)
    val sa = sin(attitude)
    val cb = cos(bank)
    val sb = sin(bank)

    linalg.Matrix4x4(
      ch * ca, sh * sb - ch * sa * cb, ch * sa * sb + sh * cb, 0f,
      sa, ca * cb, -ca * sb, 0f,
      -sh * ca, sh * sa * cb + ch * sb, -sh * sa * sb + ch * cb, 0f,
      0f, 0f, 0f, 1f
    )
  }

}

case class Matrix4x4(m00: Scalar, m01: Scalar, m02: Scalar, m03: Scalar,
                     m10: Scalar, m11: Scalar, m12: Scalar, m13: Scalar,
                     m20: Scalar, m21: Scalar, m22: Scalar, m23: Scalar,
                     m30: Scalar, m31: Scalar, m32: Scalar, m33: Scalar,
                    ) extends SquareMatrix{
  override val size : Int = 4

  override def valueAt(row: Int, column: Int): Scalar = (row, column) match {
    case (0, 0) => m00
    case (0, 1) => m01
    case (0, 2) => m02
    case (0, 3) => m03
    case (1, 0) => m10
    case (1, 1) => m11
    case (1, 2) => m12
    case (1, 3) => m13
    case (2, 0) => m20
    case (2, 1) => m21
    case (2, 2) => m22
    case (2, 3) => m23
    case (3, 0) => m30
    case (3, 1) => m31
    case (3, 2) => m32
    case (3, 3) => m33
    case _ => throw new IndexOutOfBoundsException(s"wrong matrix index ${(row, column)} should be between (0, 0) and (3, 3)")
  }

  @inline def *(ot: Matrix4x4): Matrix4x4 = Matrix4x4(
    m00 * ot.m00 + m01 * ot.m10 + m02 * ot.m20 + m03 * ot.m30, m00 * ot.m01 + m01 * ot.m11 + m02 * ot.m21 + m03 * ot.m31, m00 * ot.m02 + m01 * ot.m12 + m02 * ot.m22 + m03 * ot.m32, m00 * ot.m03 + m01 * ot.m13 + m02 * ot.m23 + m03 * ot.m33,
    m10 * ot.m00 + m11 * ot.m10 + m12 * ot.m20 + m13 * ot.m30, m10 * ot.m01 + m11 * ot.m11 + m12 * ot.m21 + m13 * ot.m31, m10 * ot.m02 + m11 * ot.m12 + m12 * ot.m22 + m13 * ot.m32, m10 * ot.m03 + m11 * ot.m13 + m12 * ot.m23 + m13 * ot.m33,
    m20 * ot.m00 + m21 * ot.m10 + m22 * ot.m20 + m23 * ot.m30, m20 * ot.m01 + m21 * ot.m11 + m22 * ot.m21 + m23 * ot.m31, m20 * ot.m02 + m21 * ot.m12 + m22 * ot.m22 + m23 * ot.m32, m20 * ot.m03 + m21 * ot.m13 + m22 * ot.m23 + m23 * ot.m33,
    m30 * ot.m00 + m31 * ot.m10 + m32 * ot.m20 + m33 * ot.m30, m30 * ot.m01 + m31 * ot.m11 + m32 * ot.m21 + m33 * ot.m31, m30 * ot.m02 + m31 * ot.m12 + m32 * ot.m22 + m33 * ot.m32, m30 * ot.m03 + m31 * ot.m13 + m32 * ot.m23 + m33 * ot.m33
  )

  @inline def *(f: Scalar): Matrix4x4 = Matrix4x4(
    m00 * f, m01 * f, m02 * f, m03 * f,
    m10 * f, m11 * f, m12 * f, m13 * f,
    m20 * f, m21 * f, m22 * f, m23 * f,
    m30 * f, m31 * f, m32 * f, m33 * f
  )

  @inline def homoTransformPoint(v: V3): V3 = {
    val nx = v.x * m00 + v.y * m01 + v.z * m02 + 1 * m03
    val ny = v.x * m10 + v.y * m11 + v.z * m12 + 1 * m13
    val nz = v.x * m20 + v.y * m21 + v.z * m22 + 1 * m23
    val nw = v.x * m30 + v.y * m31 + v.z * m32 + 1 * m33
    if(nw == 0)return V3.ZERO
    return V3(nx / nw, ny / nw, nz / nw)
  }

  @inline def homoTransformVector(v: V3): V3 = {
    val nx = v.x * m00 + v.y * m01 + v.z * m02 + 1 * m03
    val ny = v.x * m10 + v.y * m11 + v.z * m12 + 1 * m13
    val nz = v.x * m20 + v.y * m21 + v.z * m22 + 1 * m23
    //val nw = v.x * c30 + v.y * c31 + v.z * c32 + 1 * c33 //should be zero
    //if(nw == 0)return V3.zero
    return V3(nx, ny, nz)
  }


  @inline def *(v: V4): V4 = V4(
    v.x * m00 + v.y * m01 + v.z * m02 + v.w * m03,
    v.x * m10 + v.y * m11 + v.z * m12 + v.w * m13,
    v.x * m20 + v.y * m21 + v.z * m22 + v.w * m23,
    v.x * m30 + v.y * m31 + v.z * m32 + v.w * m33
  )

  @inline override lazy val det: Scalar =
    m03 * m12 * m21 * m30 - m02 * m13 * m21 * m30 -
      m03 * m11 * m22 * m30 + m01 * m13 * m22 * m30 +
      m02 * m11 * m23 * m30 - m01 * m12 * m23 * m30 -
      m03 * m12 * m20 * m31 + m02 * m13 * m20 * m31 +
      m03 * m10 * m22 * m31 - m00 * m13 * m22 * m31 -
      m02 * m10 * m23 * m31 + m00 * m12 * m23 * m31 +
      m03 * m11 * m20 * m32 - m01 * m13 * m20 * m32 -
      m03 * m10 * m21 * m32 + m00 * m13 * m21 * m32 +
      m01 * m10 * m23 * m32 - m00 * m11 * m23 * m32 -
      m02 * m11 * m20 * m33 + m01 * m12 * m20 * m33 +
      m02 * m10 * m21 * m33 - m00 * m12 * m21 * m33 -
      m01 * m10 * m22 * m33 + m00 * m11 * m22 * m33

  @inline def hasInverse: Boolean = det != 0

  @inline override def inverse: Option[Matrix4x4] = Option.when(hasInverse)(inverseUnsafe)


  @inline override def inverseUnsafe: Matrix4x4 = Matrix4x4(
    m12 * m23 * m31 - m13 * m22 * m31 + m13 * m21 * m32 - m11 * m23 * m32 - m12 * m21 * m33 + m11 * m22 * m33,
    m03 * m22 * m31 - m02 * m23 * m31 - m03 * m21 * m32 + m01 * m23 * m32 + m02 * m21 * m33 - m01 * m22 * m33,
    m02 * m13 * m31 - m03 * m12 * m31 + m03 * m11 * m32 - m01 * m13 * m32 - m02 * m11 * m33 + m01 * m12 * m33,
    m03 * m12 * m21 - m02 * m13 * m21 - m03 * m11 * m22 + m01 * m13 * m22 + m02 * m11 * m23 - m01 * m12 * m23,

    m13 * m22 * m30 - m12 * m23 * m30 - m13 * m20 * m32 + m10 * m23 * m32 + m12 * m20 * m33 - m10 * m22 * m33,
    m02 * m23 * m30 - m03 * m22 * m30 + m03 * m20 * m32 - m00 * m23 * m32 - m02 * m20 * m33 + m00 * m22 * m33,
    m03 * m12 * m30 - m02 * m13 * m30 - m03 * m10 * m32 + m00 * m13 * m32 + m02 * m10 * m33 - m00 * m12 * m33,
    m02 * m13 * m20 - m03 * m12 * m20 + m03 * m10 * m22 - m00 * m13 * m22 - m02 * m10 * m23 + m00 * m12 * m23,

    m11 * m23 * m30 - m13 * m21 * m30 + m13 * m20 * m31 - m10 * m23 * m31 - m11 * m20 * m33 + m10 * m21 * m33,
    m03 * m21 * m30 - m01 * m23 * m30 - m03 * m20 * m31 + m00 * m23 * m31 + m01 * m20 * m33 - m00 * m21 * m33,
    m01 * m13 * m30 - m03 * m11 * m30 + m03 * m10 * m31 - m00 * m13 * m31 - m01 * m10 * m33 + m00 * m11 * m33,
    m03 * m11 * m20 - m01 * m13 * m20 - m03 * m10 * m21 + m00 * m13 * m21 + m01 * m10 * m23 - m00 * m11 * m23,

    m12 * m21 * m30 - m11 * m22 * m30 - m12 * m20 * m31 + m10 * m22 * m31 + m11 * m20 * m32 - m10 * m21 * m32,
    m01 * m22 * m30 - m02 * m21 * m30 + m02 * m20 * m31 - m00 * m22 * m31 - m01 * m20 * m32 + m00 * m21 * m32,
    m02 * m11 * m30 - m01 * m12 * m30 - m02 * m10 * m31 + m00 * m12 * m31 + m01 * m10 * m32 - m00 * m11 * m32,
    m01 * m12 * m20 - m02 * m11 * m20 + m02 * m10 * m21 - m00 * m12 * m21 - m01 * m10 * m22 + m00 * m11 * m22
  )

}
