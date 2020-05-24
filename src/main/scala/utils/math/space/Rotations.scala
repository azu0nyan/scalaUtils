package utils.math.space

import utils.math.{space, _}

object Rotations {
 // type Rotation = Quat

  val idRotation: Rotation = new Rotation(space.Quat(1, 0, 0, 0))

  val aroundXPI:Rotation = space.Quat(0, 1, 0, 0)

  val aroundYPI = space.Quat(0, 0, 1, 0)

  val aroundZPI = space.Quat(0, 0, 0, 1)

  /** Rotation around left axis like looking up or down
    */
  def pitch(a:Angle):Rotation = fromEulerAngles(V3.leftAxis * a)

  /** Rotation around forward axis like rolling left or right
    */
  def roll(a:Angle):Rotation = fromEulerAngles(V3.forwardAxis * a)

  /**Rotation around up axis like turning left or right
    */
  def yaw(a:Angle):Rotation = fromEulerAngles(V3.upAxis * a)

  def toEulerAngles(r:Rotation):V3 = r.q.toEulerAngles

  def fromEulerAngles(xRoll:Scalar, yRoll:Scalar, zRoll:Scalar):Rotation = Quat.fromAngles(xRoll, yRoll, zRoll)

 // def fromQuat(q:Quat):Rotation = q

 /* implicit class ToRotation(q:Quat){
    def toRot:Rotation = new Rotation(q)
  }*/
  implicit def toQuat(r:Rotation):Quat = r.q

  def fromEulerAngles(angles:V3):Rotation = Quat.fromAngles(angles)

  implicit class Rotation(val q:Quat) extends AnyVal {
    def equalRotation(ot:Quat):Boolean = q == ot || q == -ot

    def before(ot:Quat):Quat = ot * q

    def after(ot:Quat):Quat = q * ot

    def isIdRotation:Boolean = q == idRotation.q
  }


  //def toMatrix(r:Rotation):Matrix4x4 = (r.x, r.y, r.z)


}
