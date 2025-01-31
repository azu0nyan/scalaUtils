package utils.datastructures.spatial.Octree

import utils.datastructures.spatial.SpatialStorage
import utils.math.space.{AABox, Shape}

import scala.collection.mutable

class OctreeStorage[A <: Shape, V](bounds: AABox) extends SpatialStorage[A, V] {

  var tree: Octree[V] = Leaf(bounds, Seq())

  val map: mutable.HashMap[V, A] = new mutable.HashMap()

  override def add(area: A, value: V): OctreeStorage[A, V] = {
    map += (value -> area)
    tree = tree.add(area.aabb, value)
    this
  }

  override def remove(value: V): OctreeStorage[A, V] = {
    map.remove(value) match {
      case Some(area) =>
        tree = tree.remove(area.aabb, value)
      case _ =>
    }
    this
  }

  override def areas: collection.Seq[A] = map.values.toSeq

  override def values: collection.Seq[V] = map.keys.toSeq

  override def values(area: A): collection.Seq[V] = tree.values(area.aabb).filter(v => map(v).intersects(area))
}
