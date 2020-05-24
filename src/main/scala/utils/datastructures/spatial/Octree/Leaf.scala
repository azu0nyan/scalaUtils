package utils.datastructures.spatial.Octree

import utils.datastructures.spatial.SpatialStorage
import utils.math.space.AABox

case class Leaf[V](bounds: AABox, kvPairs: Seq[(AABox, V)]) extends Octree[V] {

  override def remove(area: AABox, value: V): Octree[V] = Leaf(bounds, kvPairs.filter(kv => !kv._2.equals(value) && kv._1.intersectsWith(area)))

  override def add(area: AABox, value: V): Octree[V] = if (kvPairs.length < Octree.maxValsInLeaf) {
    Leaf(bounds, (area, value) +: kvPairs)
  } else if (bounds.volume / 8 >= Octree.minVolume) {
    val volumes = bounds.splitAllAxis.toIndexedSeq //disjoint volumes
    //todo faster splitting
    Branch(bounds, volumes.map(v => Leaf(v, kvPairs.filter(kv => v.contains(kv._1)))), kvPairs.filter(kv => !volumes.exists(v => v.contains(kv._1)))).add(area, value)
  } else {
    Leaf(bounds, (area, value) +: kvPairs)
  }

  override def remove(value: V): Leaf[ V] = Leaf(bounds, kvPairs.filter(kv => !kv._2.equals(value)))

  override def areas: Seq[AABox] = kvPairs.map(_._1)

  override def values: Seq[V] = kvPairs.map(_._2)

  override def values(area: AABox): Seq[V] = kvPairs.filter(_._1.intersectsWith(area)).map(_._2)
}
