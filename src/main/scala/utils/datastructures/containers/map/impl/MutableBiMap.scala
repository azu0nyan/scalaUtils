package utils.datastructures.containers.map.impl

import utils.datastructures.containers.map.BiMap

import scala.collection.mutable

class MutableBiMap[A, B] extends BiMap[A, B] with BiMap.Mutations[A, B] with BiMap.ShallowDupe[A, B, MutableBiMap[A, B]] {
  val ab: mutable.Map[A, B] = new mutable.LinkedHashMap[A, B]
  val ba: mutable.Map[B, A] = new mutable.LinkedHashMap[B, A]

  override def get(a: A): Option[B] = ab.get(a)
  override def teg(b: B): Option[A] = ba.get(b)
  override def containsA(a: A) = ab.contains(a)
  override def containsB(b: B) = ba.contains(b)
  override def put(a: A, b: B): Unit = {
    ab.put(a, b)
    ba.put(b, a)
  }
  override def removeA(a: A): Unit =
    ab
      .remove(a)
      .foreach(ba.remove)

  override def removeB(b: B): Unit =
    ba
      .remove(b)
      .foreach(ab.remove)

  override def shallowDupe(): MutableBiMap[A, B] = {
    val dup = new MutableBiMap[A, B]
    dup.ab ++= ab
    dup.ba ++= ba
    dup
  }

}
