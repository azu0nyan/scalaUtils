package utils.datastructures.spatial

import utils.datastructures.spatial
import utils.math.space.Shape.PointShape
import utils.datastructures.spatial.Octree.OctreeStorage
import utils.math.space.intersections.Intersects
import utils.math.space.{AABox, OBox, Shape, Sphere, Transform, Triangle, V3}
import utils.math._

object TriangleSoup{
 // implicit val storage: (V3,V3) =>SpatialStorage[AABox, Triangle] = (min:V3, max:V3) => new OctreeStorage[AABox, Triangle](AABox(min, max))
  implicit val storage: (V3,V3) =>SpatialStorage[AABox, Triangle] = (min:V3, max:V3) => new spatial.SpatialStorage.NaiveSpatialStorageImplementation[AABox, Triangle]
}

case class TriangleSoup(triangles: Seq[Triangle])(implicit storage: (V3,V3) => SpatialStorage[AABox, Triangle]) extends Shape {
  val min:V3 = triangles.flatMap(tr => tr.points).reduce(minCords)

  val max:V3 = triangles.flatMap(tr => tr.points).reduce(maxCords)

  val data: SpatialStorage[AABox, Triangle] = storage(min, max).addAll(triangles.map(t => (t.aabb, t)))

  override def intersects(other: Shape): Boolean = other match {
    case o@Triangle(_, _, _) => Intersects.triangleSoupToTriangle(this, o)
    case o@AABox(_, _) =>  Intersects.triangleSoupToAABox(this, o)
    case o@OBox(_, _, _) => Intersects.triangleSoupToOBox(this, o)
    case o@Sphere(_, _) => Intersects.triangleSoupToSphere(this, o)
    case o@TriangleSoup(_) => Intersects.triangleSoupToTriangleSoup(this, o)
    case o@PointShape(p2) => Intersects.triangleSoupToPoint(this, p2)
    case _ => false
  }

  override lazy val aabb: AABox = triangles.map(_.aabb).reduce((a, b) => a.combine(b))

  //todo optimize and correct
  override def boundingSphere:Sphere = aabb.boundingSphere

  override def transform(t: Transform): TriangleSoup = TriangleSoup(triangles.map(tr => tr.transform(t)))

  override def toString: String = s"Triangle soup: ${triangles.length} triangles"
}
