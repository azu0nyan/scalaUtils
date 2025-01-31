package utils.math.space.intersections

import utils.datastructures.spatial.TriangleSoup
import utils.math.*
import utils.math.space.*
import utils.math.space.intersections.RaySegmentCast.*

/**
  * **********|ray   |segmen|
  * sphere   |------|------|
  * aabb     |------|------|
  * obb      |------|------|
  * tri      |------|------|
  * soup     |------|------|
  * plane    |------|------|
  *
  **/
object RaySegmentCast extends RaycastTrait with SegmentCastTrait {

  def location(ln: LocationAndNormal): V3 = ln._1

  def normal(ln: LocationAndNormal): V3 = ln._2

  type LocationAndNormal = (V3, V3)
  type CastResult = Option[LocationAndNormal]
}

object Raycast extends RaycastTrait

object SegmentCast extends SegmentCastTrait

trait RaycastTrait {

  @inline def toSphere(r: Ray, s: Sphere): CastResult = LineRaySegmentToSphere.raySphere(r.origin, r.direction, s).map(l => (l, s.normalAt(l)))

  @inline def toAABox(r: Ray, aaBox: AABox): CastResult = RaySegmentToAABOX.rayToAABB(r.origin, r.direction, aaBox).map(l => (l, aaBox.normalAt(l)))

  @inline def toOBox(r: Ray, oBox: OBox): CastResult =
    toAABox(Ray(oBox.toLocalAABoxSpacePoint(r.origin), V3.toNormal(oBox.toLocalAABoxSpaceVector(r.direction))), oBox.localAABox)
      .map(hit => (oBox.fromLocalAABoxSpacePoint(location(hit)), oBox.fromLocalAABoxSpaceVector(normal(hit))))

  @inline def toTriangle(r: Ray, tr: Triangle): CastResult = LineRayToTrinagleIntersection.rayToTriangle(r, tr).map(l => (l, tr.normalAt(l)))

  @inline def toTriangleSoup(r: Ray, soup: TriangleSoup): CastResult = soup.triangles.to(LazyList).map(toTriangle(r, _)).find(_.nonEmpty).getOrElse(None)

  @inline def toPlane(r: Ray, p: Plane): CastResult = r.planeIntersection(p).map(l => (l, p.normalAt(l)))

}

trait SegmentCastTrait {

  @inline def toShape(s: Segment, shape: Shape): CastResult = shape match {
    case o@Triangle(_, _, _) => toTriangle(s, o)
    case o@TriangleSoup(_) => toTriangleSoup(s, o)
    case o@OBox(_, _, _) => toOBox(s, o)
    case o@AABox(_, _) => toAABox(s, o)
    case o@Sphere(_,_) => toSphere(s, o)
    //case o@Plane(_,_,_) => None//toPlane(s, o)
    case _ => None
  }

  @inline def toSphere(s: Segment, sp: Sphere): CastResult = LineRaySegmentToSphere.segmentSphere(s.start, s.end, sp).map(l => (l, sp.normalAt(l)))

  @inline def toAABox(s: Segment, aaBox: AABox): CastResult = RaySegmentToAABOX.segmnetToAABB(s.start, s.end, aaBox).map(l => (l, aaBox.normalAt(l)))

  @inline def toOBox(s: Segment, oBox: OBox): CastResult =
    toAABox(Segment(oBox.toLocalAABoxSpacePoint(s.start), oBox.toLocalAABoxSpacePoint(s.end)), oBox.localAABox)
      .map(hit => (oBox.fromLocalAABoxSpacePoint(location(hit)), oBox.fromLocalAABoxSpaceVector(normal(hit))))

  @inline def toTriangle(s: Segment, tr: Triangle): CastResult =
    LineRayToTrinagleIntersection.segmentToTriangle(s, tr).map { l =>
      val fromHitToStart: V3 = s.start - l
      (l, tr.normalAt(l).matchDirection(fromHitToStart))
    }

  @inline def toTriangleSoup(s: Segment, soup: TriangleSoup): CastResult = soup.triangles.to(LazyList).map(toTriangle(s, _)).find(_.nonEmpty).getOrElse(None)

  @inline def toPlane(s: Segment, p: Plane): CastResult = s.planeIntersection(p).map(l => (l, p.normalAt(l)))

}
