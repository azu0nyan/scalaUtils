package utils.math.space

import utils.math._
import utils.math.space.Transform.Transformable
//directed segment
case class Segment(start: V3, end: V3) extends Transformable[Segment]{

  @inline def body:V3 = ab

  def toRay: Ray = Ray(start, ab)

  @inline def direction: V3 = ab.normalize

  @inline def flip: Segment = Segment(end, start)

  @inline def a:V3 = start

  @inline def v1:V3 = start

  @inline def b:V3 = end

  @inline def v2:V3 = end

  @inline def center:V3 = (start + end) * HALF

  @inline def ab: V3 = end - start

  def planeIntersection(p:Plane):Option[V3] = {
    // Compute the t value for the directed line ab intersecting the plane
    val divisor = p.normal **  ab

    if(divisor == 0f) {
      return None
    }
    val t = (p.dot - p.normal ** start) / divisor

    // If t in [0..1] compute and return intersection point
    if (t >= 0.0f && t <= 1.0f) {
      Some(start + t * ab)
    }
    // Else no intersection
    None
  }

  def clothestPoint(point: V3): V3 = {
    if (ab.length ~= 0f) {
      return start
    }
    //v1 - a, v2 - b, point - c
    // Project c onto ab, computing parameterized position d(t) = a + t*(b – a)
    var t = (point - start) ** ab / ab.lengthSquared
    // If outside segment, clamp t (and therefore d) to the closest endpoint
    if (t < 0.0f) t = 0.0f
    if (t > 1.0f) t = 1.0f
    // Compute projected position from the clamped t
    return start + t * ab
  }


  def distanceToSquared(c: V3): Scalar = {
    //Vector ab = b – a, ac = c – a, bc = c – b;
    val ac = c - start
    val bc = c - end
    val e = ac ** ab
    // Handle cases where c projects outside ab
    if (e <= 0.0f) return ac **  ac
    val  f = ab ** ab
    if (e >= f) return bc ** bc
    // Handle cases where c projects onto ab
    return ac ** ac - e * e / f
  }

  @inline def distanceTo(c:V3):Scalar = sqrt(distanceToSquared(c))

  override def transform(t: Transform):Segment = Segment(t.transformPosition(start), t.transformPosition(end))
}
