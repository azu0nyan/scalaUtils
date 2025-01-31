package utils.math.space

import utils.math.*
import utils.math.space.Rotations.*

object Transform extends TransformTrait{
  trait Transformable[TRANSFORMED]{
    def transform(t:Transform):TRANSFORMED
  }
}

trait TransformTrait {
  val idTransform: Transform = Transform(idTranslation, Rotations.idRotation, idScale)

  val lerpTransform: Lerp[Transform] = (t1: Transform, t2: Transform, alpha: Scalar) => Transform(
    v3Lerp(t1.translation, t2.translation, alpha),
    quatFastLerp(t1.rotation, t2.rotation, alpha),
    v3Lerp(t1.scale, t2.scale, alpha)
  )
}

/**
  * Transform in following order Scale -> Rotation -> Translation
  */
case class Transform(
                      translation: Translation = idTranslation,
                      rotation: Rotation = Rotations.idRotation,
                      scale: Scale = idScale
                    ) {
  def isIdTranslation: Boolean = translation == idTranslation

  def isIdRotation: Boolean = rotation == Rotations.idRotation

  def isIdScale: Boolean = scale == idScale

  def andThen(ot: Transform): Transform = {
    /** A then B
      * OutTransform->Rotation = B->Rotation*A->Rotation;
      * OutTransform->Scale3D = A->Scale3D*B->Scale3D;
      * OutTransform->Translation = B->Rotation*(B->Scale3D*A->Translation) + B->Translation;
      */
    Transform(ot.rotation.rotateVector(ot.scale * translation) + ot.translation,
      ot.rotation * rotation, //reverse order
      scale * ot.scale)
  }

  def *(ot:Transform):Transform = andThen(ot)

  def inverse: Transform = {
    val invRotation = rotation.inverse
    val invScale = scale.reciporalSafe
    val invTranslation = invRotation.rotateVector(invScale * -translation)
    return Transform(invTranslation, invRotation, invScale)
  }

  def setTranslation(tr: Translation): Transform = Transform(tr, rotation, scale)

  def setScale(sc: Scale): Transform = Transform(translation, rotation, sc)

  def setRotation(rot: Rotation): Transform = Transform(translation, rot, scale)

  def moveAndRotate(dPos:V3, dRotation:Rotation):Transform = Transform(translation + dPos,  rotation before dRotation, scale)

  def move(dPos: V3): Transform = Transform(translation + dPos, rotation, scale)

  def rotate(dRotate: Rotation): Transform = Transform(translation, rotation * dRotate, scale)

  def scaleBy(s: Scale): Transform = Transform(translation, rotation, scale * s)

  def transformPosition(p: Position): Position = rotation.rotateVector(scale * p) + translation

  def transformPositionNoScale(p: Position): Position = rotation.rotateVector(p) + translation

  def transformVector(v: V3): V3 = rotation.rotateVector(scale * v)

  def transformVectorNoScale(v: V3): V3 = rotation.rotateVector(v)

  def transformRotation(r: Rotation): Rotation = rotation * r

  def inverseTransformRotation(r: Rotation): Rotation = rotation.inverse * r

  def inverseTransformPosition(p: Position): Position = rotation.unrotateVector(p - translation) * scale.reciporalSafe

  def inverseTransformPositionNOScale(p: Position): Position = rotation.unrotateVector(p - translation)

  def inverseTransformVector(v: V3): V3 = rotation.unrotateVector(v) * scale.reciporalSafe

  def inverseTransformVectorNoScale(v: V3): V3 = rotation.unrotateVector(v)

  def scaledAxes: Axes = (
    transformVector(V3.x),
    transformVector(V3.y),
    transformVector(V3.z)
  )

  def unitAxes: Axes = (
    transformVectorNoScale(V3.x),
    transformVectorNoScale(V3.y),
    transformVectorNoScale(V3.z)
  )


  /*  def transformPoint(v:V3): V3 = matrix.homoTransformPoint(v)

    def transformVector(v:V3): V3 = matrix.homoTransformVector(v)

    lazy val matrix:Matrix4x4 = Matrix4x4.translation(translation) * Matrix4x4.rotationMatrix(rotation)
  */
  override def toString: String = {
    val idT = idTranslation
    val idR = idRotation
    val idS = idScale
    s"Transform(${
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
