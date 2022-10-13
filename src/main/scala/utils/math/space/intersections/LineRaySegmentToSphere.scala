package utils.math.space.intersections

import utils.math.space.{Sphere, V3}

object LineRaySegmentToSphere {
  // Intersects ray r = p + td, |d| = 1, with sphere s and, if intersecting,
  // returns t value of intersection and intersection point q
  def raySphere(origin:V3, direction:V3, s:Sphere):Option[V3] = {
    val m:V3 = origin - s.center
    val b = m ** direction
    val c = (m ** m) - s.r * s.r
    // Exit if r’s origin outside s (c > 0) and r pointing away from s (b > 0)
    if (c > 0.0f && b > 0.0f) return None
    val discr = b*b - c
    // A negative discriminant corresponds to ray missing sphere
    if (discr < 0.0f) return None
    // Ray now found to intersect sphere, compute smallest t value of intersection
    var t = -b - math.sqrt(discr).toDouble
    // If t is negative, ray started inside sphere so clamp t to zero
    if (t < 0.0f) t = 0.0f
    return Some(origin + direction * t)
  }

  def segmentSphere(start:V3, end:V3, s:Sphere):Option[V3] = {
    val body = end - start
    val len  = body.length
    if(body.length == 0f ){
      return Option.when(s.contains(start))(start)
    }

    val direction = (end - start) / V3(len)
    val m:V3 = start - s.center
    val b = m ** direction
    val c = (m ** m) - s.r * s.r
    // Exit if r’s origin outside s (c > 0) and r pointing away from s (b > 0)
    if (c > 0.0f && b > 0.0f) return None
    val discr = b*b - c
    // A negative discriminant corresponds to ray missing sphere
    if (discr < 0.0f) return None
    // Ray now found to intersect sphere, compute smallest t value of intersection
    var t = -b - math.sqrt(discr).toFloat
    // If t is negative, ray started inside sphere so clamp t to zero
    if(t > len) return None //start is to far
    if (t < 0.0f) t = 0.0f
    return Some(start + direction * t)
  }

  def lineSphere(origin:V3, direction:V3, s:Sphere):Option[V3] = {
    val m:V3 = origin - s.center
    val b = m ** direction
    val c = (m ** m) - s.r * s.r
    if (c > 0.0f && b > 0.0f) return None
    val discr = b*b - c
    // A negative discriminant corresponds to ray missing sphere
    if (discr < 0.0f) return None
    var t = -b - math.sqrt(discr).toFloat
    return Some(origin + direction * t)
  }

  // Test if ray r = p + td intersects sphere s
  def testRaySphere(origin:V3, direction:V3, s:Sphere): Boolean =  {
    val m = origin - s.center
    val c = m ** m - s.r * s.r
    // If there is definitely at least one real root, there must be an intersection
    if (c <= 0.0f) return true
    val b = m ** direction
    // Early exit if ray origin outside sphere and ray pointing away from sphere
    if (b > 0.0f) return false
    val disc = b*b - c
    // A negative discriminant corresponds to ray missing sphere
    if (disc < 0.0f) return false
    // Now ray must hit sphere
    return true
  }


}
