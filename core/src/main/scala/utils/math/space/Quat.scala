package utils.math.space


import utils.math.*
import utils.math.space.Rotations.Rotation

object Quat extends QuatTrait

trait QuatTrait {
  val idQuat = space.Quat(1, 0, 0, 0)

  val zeroQuat = space.Quat(0, 0, 0, 0)

  def fromAngles(angles: V3): Quat = fromAngles(angles.x, angles.y, angles.z)

  /**
    * <code>fromAngles</code> builds a Quaternion from the Euler rotation
    * angles (x,y,z) aka (pitch, yaw, roll)). Note that we are applying in order: (y, z, x) aka (yaw, roll, pitch) but
    * we've ordered them in x, y, and z for convenience.
    *
    * @see <a href="http://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToQuaternion/index.htm">http://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToQuaternion/index.htm</a>
    * @param xAngle
    * the Euler pitch of rotation (in radians). (aka Attitude, often rot
    * around x)
    * @param yAngle
    * the Euler yaw of rotation (in radians). (aka Heading, often
    * rot around y)
    * @param zAngle
    * the Euler roll of rotation (in radians). (aka Bank, often
    * rot around z)
    */
  def fromAngles(xAngle: Scalar, yAngle: Scalar, zAngle: Scalar): Rotation = {
    var angle: Scalar = 0
    angle = zAngle * 0.5f
    val sinZ = sin(angle)
    val cosZ = cos(angle)
    angle = yAngle * 0.5f
    val sinY = sin(angle)
    val cosY = cos(angle)
    angle = xAngle * 0.5f
    val sinX = sin(angle)
    val cosX = cos(angle)

    // variables used to reduce multiplication calls.
    val cosYXcosZ: Scalar = cosY * cosZ
    val sinYXsinZ: Scalar = sinY * sinZ
    val cosYXsinZ: Scalar = cosY * sinZ
    val sinYXcosZ: Scalar = sinY * cosZ

    Quat(
      cosYXcosZ * cosX - sinYXsinZ * sinX,
      cosYXcosZ * sinX + sinYXsinZ * cosX,
      sinYXcosZ * cosX + cosYXsinZ * sinX,
      cosYXsinZ * cosX - sinYXcosZ * sinX).normalized //Test if normalization needed
  }


  def fromAngleAxis(angle: Scalar, axisIn: V3): Rotation = {
    val axis = axisIn.normalize
    if (axis.x == 0 && axis.y == 0 && axis.z == 0) {
      return idQuat
    } else {
      val halfAngle = 0.5f * angle
      val sinval = sin(halfAngle)
      Quat(cos(halfAngle), sinval * axis.x, sinval * axis.y, sinval * axis.z).normalized
    }
  }

  def fastLerpNormalized(a: Quat, b: Quat, alpha: Scalar): Quat = fastLerpNotNormalized(a, b, alpha).normalized

  def fastLerpNotNormalized(a: Quat, b: Quat, alpha: Scalar): Quat = {
    // To ensure the 'shortest route', we make sure the dot product between the both rotations is positive.
    val dotResult: Scalar = a ** b
    val bias: Scalar = if (dotResult >= 0) 1 else -1
    return (b * alpha) + (a * (bias * (1 - alpha)))
  }

  /**
    * Result is NOT normalized.
    */
  def fastBilerpNotNormalized(P00: Quat, P10: Quat, P01: Quat, P11: Quat, FracX: Scalar, FracY: Scalar): Quat =
    fastLerpNotNormalized(
      fastLerpNotNormalized(P00, P10, FracX),
      fastLerpNotNormalized(P01, P11, FracX),
      FracY
    )

  def slerpNomalized(q1: Quat, q2: Quat, slerp: Scalar): Quat = slerpNotNormalized(q1, q2, slerp).normalized

  def slerpNotNormalized(q1: Quat, q2: Quat, slerp: Scalar): Quat = {
    // Get cosine of angle between quats.
    val RawCosom: Scalar = q1 ** q2

    // Unaligned quats - compensate, results in taking shorter route.
    val Cosom = if (RawCosom > 0) RawCosom else -RawCosom

    var Scale0: Scalar = 0
    var Scale1: Scalar = 0

    if (Cosom < 0.9999f) {
      val Omega = acos(Cosom);
      val InvSin = 1 / sin(Omega);
      Scale0 = sin((1 - slerp) * Omega) * InvSin;
      Scale1 = sin(slerp * Omega) * InvSin;
    } else {
      // Use linear interpolation.
      Scale0 = 1.0f - slerp;
      Scale1 = slerp;
    }

    // In keeping with our flipped Cosom:
    Scale1 = if (RawCosom > 0) Scale1 else -Scale1

    return Quat(
      Scale0 * q1.w + Scale1 * q2.w,
      Scale0 * q1.x + Scale1 * q2.x,
      Scale0 * q1.y + Scale1 * q2.y,
      Scale0 * q1.z + Scale1 * q2.z
    )
  }

  def biSlerp(P00: Quat, P10: Quat, P01: Quat, P11: Quat, FracX: Scalar, FracY: Scalar): Quat = slerpNomalized(
    slerpNotNormalized(P00, P10, FracX),
    slerpNotNormalized(P10, P11, FracX),
    FracY
  )

}

case class Quat(w: Scalar, x: Scalar, y: Scalar, z: Scalar) {

  @inline def squared: Quat = this * this

  @inline def vectorPart: V3 = V3(x, y, z)

  @inline def scalarPart: Scalar = w

  @inline def inverse: Quat = conjugation

  @inline def conjugation: Quat = Quat(w, -x, -y, -z)

  @inline def lengthSquared: Scalar = w * w + x * x + y * y + z * z

  @inline def length: Scalar = sqrt(lengthSquared)

  @inline def unary_- : Quat = Quat(-w, -x, -y, -z)

  @inline def +(ot: Quat): Quat = Quat(w + ot.w, x + ot.x, y + ot.y, z + ot.z)

  @inline def -(ot: Quat): Quat = Quat(w - ot.w, x - ot.x, y - ot.y, z - ot.z)

  @inline def *(ot: Quat): Quat = Quat(
    -x * ot.x - y * ot.y - z * ot.z + w * ot.w,
    x * ot.w + y * ot.z - z * ot.y + w * ot.x,
    -x * ot.z + y * ot.w + z * ot.x + w * ot.y,
    x * ot.y - y * ot.x + z * ot.w + w * ot.z
  )

  @inline def *(s: Scalar): Quat = Quat(w * s, x * s, y * s, z * s)

  @inline def /(s: Scalar): Quat = Quat(w / s, x / s, y / s, z / s)

  /** scalar product */
  @inline def **(ot: Quat): Scalar = x * ot.x + y * ot.y + z * ot.z + w * ot.w

  @inline def reciprocal: Quat = conjugation / lengthSquared

  @inline def normalized: Quat = if (lengthSquared != 0f) this / length else this

  @inline def ~=(ot: Quat): Boolean = (x ~= ot.x) && (y ~= ot.y) && (z ~= ot.z) && (w ~= ot.w)


  def exponential: Quat = {
    val r: Scalar = sqrt(x * x + y * y + z * z);
    val et: Scalar = utils.math.exp(w)
    val s: Scalar = if (r >= SMALL_NUMBER) et * sin(r) / r else 0d
    Quat(et * cos(r), x * s, y * s, z * s) /*can be used to create exp internal*/
  }

  def logarithm: Quat = {
    val r: Scalar = sqrt(x * x + y * y + z * z);
    val t: Scalar = if (r > SMALL_NUMBER) atan2(r, w) / r else 0f
    space.Quat(0.5f * log_e(w * w + x * x + y * y + z * z), x * t, y * t, z * t) /*can be used to create log internal*/
  }

  /** quaternion power, like rotate n times
    * !!!!!!!!!"This should only work for unit quaternions" */
  def powUnit(n: Scalar): Quat = (logarithm * n).exponential

  /** value of rotation representing by this quat */
  @inline def angle: Scalar = 2f * acos(w)

  @inline def angularDistance(ot: Quat): Scalar = {
    val innerProd = x * ot.x + y * ot.y + z * ot.z + w * ot.w;
    return acos((2 * innerProd * innerProd) - 1);
  }

  @inline def rotateVector(v: V3): V3 = {
    // http://people.csail.mit.edu/bkph/articles/Quaternions.pdf
    // V' = V + 2w(Q x V) + (2Q x (Q x V))
    // refactor:
    // V' = V + w(2(Q x V)) + (Q x (2(Q x V)))
    // T = 2(Q x V);
    // V' = V + w*(T) + (Q x T)

    val q = vectorPart
    val t = (q ^ v) * 2
    return v + (t * w) + (q ^ t)
  }

  @inline def unrotateVector(v: V3): V3 = {
    val q = V3(-x, -y, -z); // Inverse
    val t = (q ^ v) * 2
    return v + (t * w) + (q ^ t)
  }

  def toEulerAngles: V3 = {
    val sqw = w * w
    val sqx = x * x
    val sqy = y * y
    val sqz = z * z
    val unit = sqx + sqy + sqz + sqw; // if normalized is one, otherwise
    // is correction factor
    val test = x * y + z * w
    if (test > 0.499 * unit) { // singularity at north pole
      V3(0, 2 * atan2(x, w), HALF_PI)

    } else if (test < -0.499 * unit) { // singularity at south pole
      V3(0, 2 - HALF_PI, -2 * atan2(x, w))
    } else {
      V3(
        atan2(2 * x * w - 2 * y * z, -sqx + sqy - sqz + sqw), // pitch or attitude
        asin(2 * test / unit), // roll or bank
        atan2(2 * y * w - 2 * x * z, sqx - sqy - sqz + sqw) // yaw or heading
      )
    }
  }

  def toAngleAxis: (Scalar, V3) = {
    val sqrLength = x * x + y * y + z * z
    if (sqrLength == 0.0f) {
      (0, V3(1, 0, 0))
    } else {
      val invLength = 1.0f / sqrt(sqrLength)
      (2.0f * acos(w), V3(x * invLength, y * invLength, z * invLength))
    }
  }

  override def toString: String = s"""Q($w | $x, $y, $z)"""
}
