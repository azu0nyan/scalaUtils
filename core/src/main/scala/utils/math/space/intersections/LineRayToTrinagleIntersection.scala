package utils.math.space.intersections

import utils.math.*
import utils.math.space.*


object LineRayToTrinagleIntersection {
  /**
    * Möller–Trumbore RAY to TRIANGLE intersection algorithm
    * https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm#Java_Implementation
    *
    * @param ray ray
    * @param tri triangle
    */

  def rayToTriangle(ray: Ray, tri: Triangle): Option[V3] = {
    //h.cross(rayVector, edge2);
    val h: V3 = ray.direction ^ tri.ac //bc
    //a = edge1.dot(h);
    val a: Scalar = tri.ab ** h
    if (a ~= 0) {
      return None // This ray is parallel to this triangle.
    }
    val f = 1.0f / a
    //s.sub(rayOrigin, vertex0);
    val s = ray.origin - tri.a
    //u = f * s.dot(h)
    val u = f * (s ** h)
    if (u < 0.0 || u > 1.0) {
      return None
    }
    //q.cross(s, edge1);
    val q = s ^ tri.ab
    val v = f * (ray.direction ** q)
    if (v < 0.0 || u + v > 1.0) {
      return None
    }
    // At this stage we can compute t to find out where the intersection point is on the line.
    val t = f * (tri.bc ** q)
    if (t ~> 0) { // ray intersection
      return Some(ray.origin + ray.direction * t)
    } else { // This means that there is a line intersection but not a ray intersection.
      return None // no point
    }
  }


  def lineToTriangle(line: Line, tri: Triangle): Option[V3] = {
    //h.cross(rayVector, edge2);
    val h: V3 = line.direction ^ tri.bc
    //a = edge1.dot(h);
    val a: Scalar = tri.ab ** h
    if (a ~= 0f) {
      None // This ray is parallel to this triangle.
    } else {
      val f = 1.0f / a
      //s.sub(rayOrigin, vertex0);
      val s = line.origin - tri.a
      //u = f * s.dot(h)
      val u = f * (s ** h)
      if (u < 0.0 || u > 1.0) {
        None
      } else {
        //q.cross(s, edge1);
        val q = s ^ tri.ab
        val v = f * (line.direction ** q)
        if (v < 0.0 || u + v > 1.0) {
          None
        } else {
          // At this stage we can compute t to find out where the intersection point is on the line.
          val t = f * (tri.bc ** q)
          Some(line.origin + line.direction * t)
        }
      }
    }
  }

  def segmentToTriangle(seg: Segment, tr: Triangle): Option[V3] =
    rayToTriangle(seg.toRay, tr).
      flatMap(point => Option.when(seg.body.lengthSquared >= (point - seg.start).lengthSquared)(point))

  /*def segmentToTriagnle(s: Segment, tri: Triangle): Option[V3] = {
    //segment P -> Q
    val qp = s.ab.opposite
    // Compute denominator d. If d <= 0, segment is parallel to or points
    // away from triangle, so exit early
    val d = qp **  tri.normal
    if (d <= 0.0f) return None
    // Compute intersection t value of pq with plane of triangle. A ray
    // intersects iff 0 <= t. Segment intersects iff 0 <= t <= 1. Delay
    // dividing by d until intersection has been found to pierce triangle

    val ap = s.start - tri.a
    var t = ap ** tri.normal //
    if (t < 0.0f) return None
    if (t > d) return None // For segment; exclude this code line for a ray test
    // Compute barycentric coordinate components and test if within bounds
    val e = qp ^ ap
    val v = tri.ac ** e
    if (v < 0.0f || v > d) return None
    val w = - (tri.ab ** e)
    if (w < 0.0f || v + w > d) return None
    // Segment/ray intersects triangle. Perform delayed division and
    // compute the last barycentric coordinate component
    val  ood:Float = 1.0f / d
    t *= ood
    //v *= ood;
    //w *= ood;
    //u = 1.0f - v - w;
    return Some(s.start + ood * s.ab)
  }*/

}
