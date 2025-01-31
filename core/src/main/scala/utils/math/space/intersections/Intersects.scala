package utils.math.space.intersections

import utils.datastructures.spatial.TriangleSoup
import utils.math.*
import utils.math.space.*

/**
  ***********|point |line  |ray   |segmen|sphere|aabb  |obb   |tri   |soup  |plane |
  * point    |  TP  |  TP  |  TP  |      |  TP  |  TP  |  TP  |  T?P |  T-  |  TP  |
  * line     |------|      |      |      |  TP  |  TP  |      |  TP  |      |  TP? |
  * ray      |------|------|      |      |  TP  |  TP  |      |  TP  |      |  TP? |
  * segment  |------|------|------|      |  TP  |  TP  |      |  TP  |      |  TP  |
  * sphere   |------|------|------|------|  T   |  T   |  T   |  T   |  T-  |  T   |
  * aabb     |------|------|------|------|------|  T   |  T   |  T   |  T-  |  T   |
  * obb      |------|------|------|------|------|------|  T   |  T?  |  T-  |  T   |
  * tri      |------|------|------|------|------|------|------|  T   |  T-  |      |
  * soup     |------|------|------|------|------|------|------|------|  T-  |      |
  * plane    |------|------|------|------|------|------|------|------|------|      |
  *
  **/
object Intersects {

 // def apply[A <: Triangle, B<: Triangle](a:A, b:B): Boolean = triangleToTriangle(a, b)

  //def apply[A<: AABox, B <: Triangle](a:A, b:B): Boolean = aaboxToTriange(a, b)


  def pointToPoint(a: V3, b: V3): Boolean = a ~= b

  def pointToLine(p: V3, l: Line): Boolean = l.projectPoint(p).distanceSquared(p) ~= 0f

  //TODO check
  def pointToRay(p: V3, r: Ray): Boolean = pointToLine(p, r.toLine) && ((p - r.origin) ** r.direction ~>= 0f)

  def pointToSphere(p: V3, s: Sphere): Boolean = s.distanceSquared(p) <= s.r

  def pointToAABox(p: V3, b: AABox): Boolean = b.contains(p)

  def pointToOBox(p: V3, b: OBox): Boolean = b.distanceSquared(p) ~= 0f

  def pointToTriangle(p: V3, t: Triangle): Boolean = t.closestPoint(p) ~= p

  def pointToPlane(p:V3, plane: Plane):Boolean = plane.contains(p)

  def lineToLine(l1: Line, l2: Line): Boolean = false

  def lineToRay(l: Line, r: Ray): Boolean = false

  def lineToAABox(l: Line, b: AABox):Boolean = RaySegmentToAABOX.lineToAABB(l.origin, l.direction, b).nonEmpty

  def lineToOBox(l: Line, b: OBox) = false

  def lineToTriangle(l: Line, t: Triangle): Boolean = LineRayToTrinagleIntersection.lineToTriangle(l, t).nonEmpty

  def lineToSoup(l: Line, p: TriangleSoup) = false

  def lineToPlane(l:Line, p:Plane):Boolean = l.planeIntersection(p).nonEmpty

  def lineToSphere(l:Line, s:Sphere):Boolean = LineRaySegmentToSphere.lineSphere(l.origin, l.direction, s).nonEmpty

  def rayToSphere(r:Ray, s:Sphere):Boolean = LineRaySegmentToSphere.testRaySphere(r.origin, r.direction, s)

  def rayToAABox(r: Ray, b: AABox):Boolean = RaySegmentToAABOX.rayToAABB(r.origin, r.direction, b).nonEmpty

  def rayToOBox(r: Ray, b: OBox) = false

  def rayToTriangle(r: Ray, t: Triangle): Boolean = LineRayToTrinagleIntersection.rayToTriangle(r, t).nonEmpty

  def rayToPlane(r:Ray, p:Plane):Boolean = r.planeIntersection(p).nonEmpty

  def rayToSoup(r: Ray, p: TriangleSoup) = false

  def segmentToAABox(s: Segment, b: AABox):Boolean = RaySegmentToAABOX.segmnetToAABB(s.start, s.end, b).nonEmpty

  def segmentToSphere(sg:Segment, s:Sphere):Boolean = LineRaySegmentToSphere.segmentSphere(sg.start, sg.end, s).nonEmpty

  def segmentToPlane(s:Segment, p:Plane):Boolean = s.planeIntersection(p).nonEmpty

  def segmentToTriangle(s:Segment, t:Triangle):Boolean = LineRayToTrinagleIntersection.segmentToTriangle(s, t).nonEmpty

  def sphereToSphere(a: Sphere, b: Sphere): Boolean = a.intersects(b)

  def aaBoxToAABox(a: AABox, b: AABox): Boolean = a.intersects(b)

  def shpereToPlane(s: Sphere, p: Plane): Boolean = {
    // For a normalized plane (|p.n| = 1), evaluating the plane equation
    // for a point gives the signed distance of the point to the plane
    val dist: Scalar = s.center ** p.normal - p.dot
    // If sphere center within +/-radius from plane, plane intersects sphere
    return math.abs(dist) <= s.r
  }

  def oboxToPlane(b: OBox, p: Plane): Boolean = {
    // Compute the projection interval radius of b onto L(t) = b.c + t * p.n
    val r: Scalar = b.halfExtents(0) * math.abs(p.normal ** b.axeIds(0)) +
      b.halfExtents(1) * math.abs(p.normal ** b.axeIds(1)) +
      b.halfExtents(2) * math.abs(p.normal ** b.axeIds(2))
    // Compute distance of box center from plane
    val s = p.normal ** b.center - p.dot
    // Intersection occurs when distance s falls within [-r,+r] interval
    return math.abs(s) <= r
  }

  def aaboxToPlane(b: AABox, p: Plane): Boolean = {
    // These two lines not necessary with a (center, extents) AABB representation
    val c = (b.max + b.min) * 0.5f; // Compute AABB center
    val e = b.max - c; // Compute positive extents
    // Compute the projection interval radius of b onto L(t) = b.c + t * p.n
    val r = e(0) * math.abs(p.normal(0)) + e(1) * math.abs(p.normal(1)) + e(2) * math.abs(p.normal(2))
    // Compute distance of box center from plane
    val s = p.normal ** c - p.dot
    // Intersection occurs when distance s falls within [-r,+r] interval
    return math.abs(s) <= r
  }

  def aaboxToTriange(b: AABox, t: Triangle): Boolean = {
    TriangleToBox.triangleToBoxIntersection(b.center, b.halfExtents, t)
  }

  def oboxToTriange(b: OBox, t: Triangle): Boolean = {
    val t2 = Triangle(b.toLocalAABoxSpacePoint(t.v1),
      b.toLocalAABoxSpacePoint(t.v2),
      b.toLocalAABoxSpacePoint(t.v3)
    )
    return TriangleToBox.triangleToBoxIntersection(ZERO, b.halfExtents, t2)
  }

  def aaboxToSphere(b: AABox, s: Sphere): Boolean = {
    // Compute squared distance between sphere center and AABB
    val sqDist = b.distanceSquared(s.center)
    // Sphere and AABB intersect if the (squared) distance
    // between them is less than the (squared) sphere radius
    return sqDist <= s.r * s.r
  }

  def aaBoxToOBox(a:AABox, o:OBox):Boolean = OBox.intersects(o, a.orientedBox)

  def oboxToOBox(a:OBox, o:OBox):Boolean = OBox.intersects(a, o)

  def oboxToSphere(b: OBox, s: Sphere): Boolean = {
    // Find point p on OBB closest to sphere center
    val p = b.closestPoint(s.center)
    // Sphere and OBB intersect if the (squared) distance from sphere
    // center to point p is less than the (squared) sphere radius
    val v = p - s.center
    return v ** v <= s.r * s.r
  }

  def triangleToSphere(t: Triangle, s: Sphere): Boolean = {
    // Find point P on triangle ABC closest to sphere center
    val p = t.closestPoint(s.center)
    // Sphere and triangle intersect if the (squared) distance from sphere
    // center to point p is less than the (squared) sphere radius
    val v = p - s.center
    return v ** v <= s.r * s.r
  }

  def triangleToTriangle(t1: Triangle, t2: Triangle):Boolean = TriangleToTriangle(t1.v1, t1.v2, t1.v3, t2.v1, t2.v2, t2.v3)

  def triangleSoupToPoint(t:TriangleSoup, p:V3): Boolean = t.data.values(AABox(p, p)).exists(t => Intersects.pointToTriangle(p, t))

  def triangleSoupToSphere(t:TriangleSoup, s:Sphere):Boolean = t.data.values(s.aabb).exists(t => Intersects.triangleToSphere(t, s))

  def triangleSoupToAABox(t:TriangleSoup, b:AABox):Boolean = t.data.values(b).exists(t => Intersects.aaboxToTriange(b, t))

  def triangleSoupToOBox(t:TriangleSoup, b:OBox):Boolean = t.data.values(b.aabb).exists(t => Intersects.oboxToTriange(b, t))

  def triangleSoupToTriangle(t:TriangleSoup, tr:Triangle):Boolean = t.data.values(tr.aabb).exists(t => Intersects.triangleToTriangle(t, tr))

  def triangleSoupToTriangleSoup(t1:TriangleSoup, t2:TriangleSoup):Boolean = t1.data.values(t2.aabb).exists(t => Intersects.triangleSoupToTriangle(t2, t))


}
