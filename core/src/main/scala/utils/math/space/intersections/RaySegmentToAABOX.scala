package utils.math.space.intersections

import utils.math.*
import utils.math.space.{AABox, V3}


object RaySegmentToAABOX {
  // Intersect ray R(t) = p + t*d against AABB a. When intersecting,
  // return intersection distance tmin and point q of intersection

  @inline def test(p: V3, d: V3, a: AABox, tminn: Scalar, tmaxx: Scalar): Option[V3] = {
    var tmin:Scalar = tminn
    var tmax:Scalar = tmaxx
    // For all three slabs
    var i: Int = 0
    while (i < 3) {
      if (math.abs(d(i)) ~= 0f) {
        // Ray is parallel to slab. No hit if origin not within slab
        if (p(i) < a.min(i) || p(i) > a.max(i)) return None
      } else {
        // Compute intersection t value of ray with near and far plane of slab
        val ood = 1.0f / d(i)
        var t1 = (a.min(i) - p(i)) * ood
        var t2 = (a.max(i) - p(i)) * ood
        // Make t1 be intersection with near plane, t2 with far plane
        if (t1 > t2) {
          val tmp = t1
          t1 = t2
          t2 = tmp
        }
        // Compute the intersection of slab intersection intervals
        tmin = max(tmin, t1)
        tmax = min(tmax, t2)
        //if (t1 > tmin) tmin = t1
        //if (t2 > tmax) tmax = t2
        // Exit with no collision as soon as slab intersection becomes empty
        if (tmin > tmax) return None
      }

      i += 1
    }

    // Ray intersects all 3 slabs. Return point (q) and intersection t value (tmin)
    return Some(p + d * tmin)
  }

  def rayToAABB(p: V3, d: V3, a: AABox): Option[V3] = {
    var tmin = 0.0f; // set to -FLT_MAX to get first hit on line
    var tmax = Float.MaxValue; // set to max distance ray can travel (for segment)
    test(p, d, a, tmin, tmax)
  }

  def lineToAABB(p: V3, d: V3, a: AABox): Option[V3] = {
    var tmin = Float.MinValue; // set to -FLT_MAX to get first hit on line
    var tmax = Float.MaxValue; // set to max distance ray can travel (for segment)
    test(p, d, a, tmin, tmax)
  }

  def segmnetToAABB(start: V3, end: V3, a: AABox): Option[V3] = {
    var tmin = 0.0f; // set to -FLT_MAX to get first hit on line
    var tmax = (end - start).length // set to max distance ray can travel (for segment)
    test(start, (end - start).normalize, a, tmin, tmax)
  }

}
