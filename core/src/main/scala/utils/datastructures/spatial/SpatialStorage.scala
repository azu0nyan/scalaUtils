package utils.datastructures.spatial

import utils.math.misc.Maps
import utils.math.space.{Shape, V3}

import scala.collection.mutable

object SpatialStorage {

  //implicit def toMapping[VD, HD, FD](spatialMap: SpatialMap[VD, HD, FD]): Mapping[V3, Seq[VD, HD, FD]] = spatialMap.values

  //implicit def toMapping[VD, HD, FD](spatialMap: SpatialMap[VD, HD, FD]): Mapping[Area, Seq[VD, HD, FD]] = spatialMap.values

  class NaiveSpatialStorageImplementation[A <: Shape, V] extends SpatialStorage[A, V] {

    val map: mutable.HashMap[V, A] = new mutable.HashMap[V, A]()

    override def add(area: A, value: V): this.type  = {
      map.put(value, area)
      this
    }

    override def remove(value: V): this.type  = {
      map.remove(value)
      this
    }

    override def areas: Seq[A] = map.values.toSeq

    override def values: Seq[V] = map.keys.toSeq

    override def values(area: A): Seq[V] = {
      values.filter(v => {
        val a = map.get(v)
        a.nonEmpty && a.get.intersects(area)
      })
    }
  }

}



trait SpatialStorage[A <: Shape, V] {

  def apply(a: A): collection.Seq[V] = values(a)

  def values(area: A): collection.Seq[V]

  def addAll(s: Seq[(A, V)]): SpatialStorage[A , V]  = s match {
    case Seq() => this
    case head +: tail => add(head._1, head._2).addAll(tail)
  }

  def add(area: A, value: V): SpatialStorage[A , V]

  def add(kv: (A, V)): SpatialStorage[A , V]  = add(kv._1, kv._2)

  def remove(value: V): SpatialStorage[A , V]

  def areas: collection.Seq[A]

  def values: collection.Seq[V]

  def valuesCount: Int = values.length

  def update(value: V, newArea: A): SpatialStorage[A , V] = remove(value).add(newArea, value)

  def update(value: V, oldArea: A, newArea: A): SpatialStorage[A , V] = update(value, newArea)

}
