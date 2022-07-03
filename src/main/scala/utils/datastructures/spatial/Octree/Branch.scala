package utils.datastructures.spatial.Octree

import utils.math.space.AABox

object Branch {

}

case class Branch[V](bounds: AABox, partitions: IndexedSeq[Octree[V]], kvPairs: Seq[(AABox, V)]) extends Octree[V] {

  def partitionId(area: AABox): Int = {
    var id = -1 // no leafs
    var i = 0
    while (i < partitions.length) {
      if (partitions(i).bounds.intersectsWith(area)) {
        if (id != -1) {
          return -2 //multiple leafs intersected
        } else {
          id = i
        }
      }
      i += 1
    }
    return id
  }

  override def add(area: AABox, value: V): Branch[V] = {
    val pId = partitionId(area)
    if (pId < 0) {
      Branch(bounds, partitions, (area, value) +: kvPairs)
    } else {
      Branch(bounds, partitions.updated(pId, partitions(pId).add(area, value)), kvPairs)
    }
  }

  override def remove(value: V): Branch[V] = Branch(bounds, partitions.map(_.remove(value)), kvPairs.filterNot(kv2 => kv2._2 == value))

  override def remove(area: AABox, v: V): Octree[V] = {
    val pId = partitionId(area)
    if (pId == -1) { //v not here
      this
    } else if (pId == -2) { //v here
      Branch(bounds, partitions, kvPairs.filterNot( kv => kv._2 == v && kv._1.intersects(area)))
    } else {
      Branch(bounds, partitions.updated(pId, partitions(pId).remove(area, v)), kvPairs)
    }
  }

  override def areas: Seq[AABox] = kvPairs.map(_._1) ++ partitions.flatMap(_.areas)

  override def values: Seq[V] = kvPairs.map(_._2) ++ partitions.flatMap(_.values)

  override def values(area: AABox): Seq[V] = if(area.intersectsWith(bounds)){
    kvPairs.filter(kv => kv._1.intersectsWith(area)).map(_._2) ++ partitions.flatMap(_.values(area))
  } else {
    Seq()
  }
}
