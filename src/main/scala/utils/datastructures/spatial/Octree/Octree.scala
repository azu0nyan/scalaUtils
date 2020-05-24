package utils.datastructures.spatial.Octree

import utils.datastructures.spatial.SpatialStorage
import utils.math.space.AABox

object Octree {

  val maxValsInLeaf = 4

  val minVolume = 1

}

trait Octree[V] extends SpatialStorage[AABox, V] {

  def bounds: AABox

  def kvPairs: Seq[(AABox, V)]

  def remove(area: AABox, v:V):Octree[V]

  override def remove( v:V):Octree[V]

  override def add(area: AABox, value: V): Octree[V]
}





