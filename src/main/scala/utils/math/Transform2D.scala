package utils.math

import utils.math.planar.V2


case class Transform2D(
                      translation: V2 = V2(0, 0),
                      rotation: Scalar = 0d,
                      scale: V2 = V2(1d)
                    ) {


  def inverse: Transform2D = {
    val invScalar = -rotation
    val invV2 = scale.reciporalSafe
    val invTranslation = (invV2 * -translation).rotate(invScalar)
    return Transform2D(invTranslation, invScalar, invV2)
  }

  def setTranslation(tr: V2): Transform2D = Transform2D(tr, rotation, scale)

  def setScale(sc: V2): Transform2D = Transform2D(translation, rotation, sc)

  def setRotation(rot: Scalar): Transform2D = Transform2D(translation, rot, scale)

  def move(dPos: V2): Transform2D = Transform2D(translation + dPos, rotation, scale)

  def rotate(dRotate: Scalar): Transform2D = Transform2D(translation, rotation * dRotate, scale)

  def scaleBy(s: V2): Transform2D = Transform2D(translation, rotation, scale * s)

  def transformPoint(p: V2): V2 = (scale * p).rotate(rotation) + translation

  def transformVector(v: V2): V2 =(scale * v).rotate(rotation)

  def apply(v:V2):V2 = transformPoint(v)

  def transformScalar(r: Scalar): Scalar = rotation * r


  def inverseTransformV2(p: V2): V2 = (p - translation).rotate(-rotation) * scale.reciporalSafe


  def inverseTransformVector(v: V2): V2 = (v).rotate(-rotation) * scale.reciporalSafe


  /*  def transformPoint(v:V2): V2 = matrix.homoTransformPoint(v)

    def transformVector(v:V2): V2 = matrix.homoTransformVector(v)

    lazy val matrix:Matrix4x4 = Matrix4x4.translation(translation) * Matrix4x4.rotationMatrix(rotation)
  */
  override def toString: String = {
    val idT = V2.ZERO
    val idR = 0d
    val idS = V2(1d, 1d)
    s"Transform2D(${
      (translation, rotation, scale) match {
        case (`idT`, `idR`, `idS`) => ""
        case (`idT`, `idR`,  s) => s"scale = $s"
        case (`idT`, r, `idS`) => s"rotation = $r"
        case (t, `idR`, `idS`) => s"translation = $t"
        case (t, r, `idS`) => s"$t, $r"
        case (t, r, s) => s" $t, $r, $s"
      }
    })"
  }
}

