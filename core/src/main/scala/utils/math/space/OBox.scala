package utils.math.space

import utils.datastructures.spatial.TriangleSoup
import utils.math.*
import utils.math.space.Shape.PointShape
import utils.math.space.intersections.Intersects

object OBox {


  final val EPSILON = SMALL_NUMBER

  def intersects(box1: OBox, box2: OBox): Boolean = {
    // check if there's a separating plane in between the selected axes
    @inline def getSeparatingPlane(RPos: V3, Plane: V3, box1: OBox, box2: OBox): Boolean = {
      return abs(RPos ** Plane) >
        (abs((box1.axes._1 * box1.halfExtents.x) ** Plane) +
          abs((box1.axes._2 * box1.halfExtents.y) ** Plane) +
          abs((box1.axes._3 * box1.halfExtents.z) ** Plane) +
          abs((box2.axes._1 * box2.halfExtents.x) ** Plane) +
          abs((box2.axes._2 * box2.halfExtents.y) ** Plane) +
          abs((box2.axes._3 * box2.halfExtents.z) ** Plane))
    }

    val RPos = box2.center - box1.center

    return !(
      getSeparatingPlane(RPos, box1.axes._1, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._2, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._3, box1, box2) ||
        getSeparatingPlane(RPos, box2.axes._1, box1, box2) ||
        getSeparatingPlane(RPos, box2.axes._2, box1, box2) ||
        getSeparatingPlane(RPos, box2.axes._3, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._1 ^ box2.axes._1, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._1 ^ box2.axes._2, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._1 ^ box2.axes._3, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._2 ^ box2.axes._1, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._2 ^ box2.axes._2, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._2 ^ box2.axes._3, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._3 ^ box2.axes._1, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._3 ^ box2.axes._2, box1, box2) ||
        getSeparatingPlane(RPos, box1.axes._3 ^ box2.axes._3, box1, box2));
  }

  /*def intersects(a: OBox, b: OBox): Boolean = {
    // Compute rotation matrix expressing b in a’s coordinate frame
    val R: Matrix = Matrix.genByRule(3, 3, (i, j) => a.axes(i) ** a.axes(j))


    // Compute translation vector t
    var t = b.center - a.center
    // Bring translation into a’s coordinate frame
    t = V3(t ** a.axes(0), t ** a.axes(1), t ** a.axes(2)) //a.u(1) => a.u(2)
//    t = t.toBasis(a.axes)
    // Compute common subexpressions. Add in an epsilon term to
    // counteract arithmetic errors when two edges are parallel and
    // their cross product is (near) null (see text for details)
    val AbsR: Matrix = Matrix.genByRule(3, 3, (i, j) => R(i, j) + EPSILON)

    var ra: Scalar = 0f
    var rb: Float = 0f

    // Test axes L = A0, L = A1, L = A2
    {
      var i: Int = 0
      while (i < 3) {
        ra = a.halfExtents(i)
        rb = b.halfExtents(0) * AbsR(i, 0) + b.halfExtents(1) * AbsR(i, 1) + b.halfExtents(2) * AbsR(i, 2)
        if (math.abs(t(i)) > ra + rb) return false

        i += 1
      }
    }
    // Test axes L = B0, L = B1, L = B2
    {
      var i: Int = 0
      while (i < 3) {
        ra = a.halfExtents(0) * AbsR(0, i) + a.halfExtents(1) * AbsR(1, i) + a.halfExtents(2) * AbsR(2, i)
        rb = b.halfExtents(i)
        if (math.abs(t(0) * R(0, i) + t(1) * R(1, i) + t(2) * R(2, i)) > ra + rb) return false

        i += 1
      }
    }

    // Test axis L = A0 x B0// Test axis L = A0 x B0
    ra = a.halfExtents(1) * AbsR(2, 0) + a.halfExtents(2) * AbsR(1, 0)
    rb = b.halfExtents(1) * AbsR(0, 2) + b.halfExtents(2) * AbsR(0, 1)
    if (math.abs(t(2) * R(1, 0) - t(1) * R(2, 0)) > ra + rb) return false
    // Test axis L = A0 x B1// Test axis L = A0 x B1
    ra = a.halfExtents(1) * AbsR(2, 1) + a.halfExtents(2) * AbsR(1, 1)
    rb = b.halfExtents(0) * AbsR(0, 2) + b.halfExtents(2) * AbsR(0, 0)
    if (math.abs(t(2) * R(1, 1) - t(1) * R(2, 1)) > ra + rb) return false
    // Test axis L = A0 x B2
    ra = a.halfExtents(1) * AbsR(2, 2) + a.halfExtents(2) * AbsR(1, 2)
    rb = b.halfExtents(0) * AbsR(0, 1) + b.halfExtents(1) * AbsR(0, 0)
    if (math.abs(t(2) * R(1, 2) - t(1) * R(2, 2)) > ra + rb) return false
    // Test axis L = A1 x B0
    ra = a.halfExtents(0) * AbsR(2, 0) + a.halfExtents(2) * AbsR(0, 0)
    rb = b.halfExtents(1) * AbsR(1, 2) + b.halfExtents(2) * AbsR(1, 1)
    if (math.abs(t(0) * R(2, 0) - t(2) * R(0, 0)) > ra + rb) return false
    // Test axis L = A1 x B1// Test axis L = A1 x B1
    ra = a.halfExtents(0) * AbsR(2, 1) + a.halfExtents(2) * AbsR(0, 1)
    rb = b.halfExtents(0) * AbsR(1, 2) + b.halfExtents(2) * AbsR(1, 0)
    if (math.abs(t(0) * R(2, 1) - t(2) * R(0, 1)) > ra + rb) return false
    // Test axis L = A1 x B2
    ra = a.halfExtents(0) * AbsR(2, 2) + a.halfExtents(2) * AbsR(0, 2)
    rb = b.halfExtents(0) * AbsR(1, 1) + b.halfExtents(1) * AbsR(1, 0)
    if (math.abs(t(0) * R(2, 2) - t(2) * R(0, 2)) > ra + rb) return false
    // Test axis L = A2 x B0
    ra = a.halfExtents(0) * AbsR(1, 0) + a.halfExtents(1) * AbsR(0, 0)
    rb = b.halfExtents(1) * AbsR(2, 2) + b.halfExtents(2) * AbsR(2, 1)
    if (math.abs(t(1) * R(0, 0) - t(0) * R(1, 0)) > ra + rb) return false
    // Test axis L = A2 x B1
    ra = a.halfExtents(0) * AbsR(1, 1) + a.halfExtents(1) * AbsR(0, 1)
    rb = b.halfExtents(0) * AbsR(2, 2) + b.halfExtents(2) * AbsR(2, 0)
    if (math.abs(t(1) * R(0, 1) - t(0) * R(1, 1)) > ra + rb) return false
    // Test axis L = A2 x B2
    ra = a.halfExtents(0) * AbsR(1, 2) + a.halfExtents(1) * AbsR(0, 2)
    rb = b.halfExtents(0) * AbsR(2, 1) + b.halfExtents(1) * AbsR(2, 0)
    if (math.abs(t(1) * R(0, 2) - t(0) * R(1, 2)) > ra + rb) return false
    // Since no separating axis is found, the OBBs must be intersecting
    return true
  }*/

}


/**
 * Region R = { x | x = c+r*u[0]+s*u[1]+t*u[2] }, |r|<=e[0], |s|<=e[1], |t|<=e[2]
 *
 * @param center      OBB center point
 * @param axes        Local x-, y-, and z-axes
 * @param halfExtents Positive halfwidth extents of OBB along each axis
 */
case class OBox(
                 center: V3 = V3.ZERO,
                 axes: Axes = (V3.x, V3.y, V3.z),
                 halfExtents: V3 = V3(0.5d)
               ) extends Shape with NormalAndClosest {


  @inline def axeIds(i: Int): V3 = i match {
    case 0 => axes._1
    case 1 => axes._2
    case 2 => axes._3
  }

  //@formatter:off
  /*
   *     v6________ v7
   *    /|          /|
   *   / |         / |
   * v4__|________v5 |
   * |   |        |  |
   * |   v2______ v3
   * |  /         | /
   * | /          |/
   * v0___________v1
    */
  //@formatter:on

  lazy val angles: Seq[V3] =
    Seq(
      (fromLocalAABoxSpaceVector(V3(-1, -1, -1)) * halfExtents) + center,
      (fromLocalAABoxSpaceVector(V3(-1, -1, 1)) * halfExtents) + center,
      (fromLocalAABoxSpaceVector(V3(-1, 1, -1)) * halfExtents) + center,
      (fromLocalAABoxSpaceVector(V3(-1, 1, 1)) * halfExtents) + center,
      (fromLocalAABoxSpaceVector(V3(1, -1, -1)) * halfExtents) + center,
      (fromLocalAABoxSpaceVector(V3(1, -1, 1)) * halfExtents) + center,
      (fromLocalAABoxSpaceVector(V3(1, 1, -1)) * halfExtents) + center,
      (fromLocalAABoxSpaceVector(V3(1, 1, 1)) * halfExtents) + center,
    )
  //@formatter:off
  /*
   *     v6________ v7
   *    /|          /|
   *   / |         / |
   * v4__|________v5 |
   * |   |        |  |
   * |   v2______ | v3
   * |  /         | /
   * | /          |/
   * v0___________v1
    */
  //@formatter:on

  lazy val sides: Seq[Quad] = Seq(
    Quad(angles(0), angles(1), angles(2), angles(3)),
    Quad(angles(4), angles(5), angles(6), angles(7)),
    Quad(angles(0), angles(1), angles(4), angles(5)),
    Quad(angles(1), angles(3), angles(5), angles(7)),
    Quad(angles(3), angles(2), angles(7), angles(6)),
    Quad(angles(2), angles(0), angles(6), angles(4)),
  )

  /*Seq(
  (V3(-1, -1, -1) * halfExtents) + center,
  (V3(-1, -1, 1) * halfExtents) + center,
  (V3(-1, 1, -1) * halfExtents) + center,
  (V3(-1, 1, 1) * halfExtents) + center,
  (V3(1, -1, -1) * halfExtents) + center,
  (V3(1, -1, 1) * halfExtents) + center,
  (V3(1, 1, -1) * halfExtents) + center,
  (V3(1, 1, 1) * halfExtents) + center,
)*/

  override def intersects(other: Shape): Boolean = other match {
    case o@AABox(_, _) => Intersects.aaBoxToOBox(o, this)
    case o@Sphere(_, _) => Intersects.oboxToSphere(this, o)
    case o@OBox(_, _, _) => Intersects.oboxToOBox(this, o)
    case o@Triangle(_, _, _) => Intersects.oboxToTriange(this, o)
    case o@PointShape(p2) => Intersects.pointToOBox(p2, this)
    case o@TriangleSoup(_) => Intersects.triangleSoupToOBox(o, this)
    case _ => false
  }

  override def aabb: AABox = AABox.fromPoints(angles)

  override def boundingSphere: Sphere = Sphere(center, halfExtents.length)

  override def closestPoint(p: V3): V3 = {
    val d = p - center
    // Start result at center of box; make steps from there
    var q: V3 = center
    // For each OBB axis...
    var i: Int = 0
    while (i < 3) {
      // ...project d onto that axis to get the distance
      // along the axis of d from the box center
      var dist: Scalar = d ** axeIds(i)
      // If distance farther than the box extents, clamp to the box
      if (dist > halfExtents(i)) dist = halfExtents(i)
      if (dist < -halfExtents(i)) dist = -halfExtents(i)
      // Step that distance along the axis to get world coordinate
      q += dist * axeIds(i)

      i += 1
    }
    return q
  }


  override def transform(t: Transform): OBox = OBox(
    t.transformPosition(center),
    (t.transformVector(axes._1), t.transformVector(axes._2), t.transformVector(axes._3)),
    halfExtents)

  /**
   * subtracting center from v since v is point
   */
  def toLocalAABoxSpacePoint(v: V3): V3 = (v - center).toBasis(axes)

  /**
   * just projecting on basis vectors since v is vector
   */
  def toLocalAABoxSpaceVector(v: V3): V3 = v.toBasis(axes)

  /** v vector in local space so no center offset applied */
  def fromLocalAABoxSpaceVector(v: V3): V3 = (axes._1 * v.x) + (axes._2 * v.y) + (axes._3 * v.z)

  def fromLocalAABoxSpacePoint(v: V3): V3 = center + (axes._1 * v.x) + (axes._2 * v.y) + (axes._3 * v.z)

  def localAABox: AABox = new AABox(halfExtents * 2)

  override def normalAt(point: Scale): Normal = V3.toNormal(fromLocalAABoxSpaceVector(localAABox.normalAt(toLocalAABoxSpacePoint(point))))

}
