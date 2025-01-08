package utils.datastructures.containers.map.impl

import utils.datastructures.containers.map.MultiMap

import scala.collection.mutable

class MutableMultiMap[A, B, C[_]](
                                   using co: MutableMultiMap.InMapContainerOps[B, C]
                                 ) extends MultiMap[A, B, C] with MultiMap.Mutations[A, B, C] {

  private val innerMap = new mutable.LinkedHashMap[A, C[B]]

  override def size: Int =
    innerMap.size

  override def isEmpty: Boolean =
    innerMap.isEmpty

  override def containsKey(a: A): Boolean =
    innerMap.contains(a)

  override def contains(a: A, b: B): Boolean =
    innerMap
      .get(a)
      .exists(_.contains(b))

  override def get(a: A): C[B] =
    innerMap.getOrElse(a, co.empty)

  override def getMultiple(a: A*): C[B] =
    a
      .flatMap(innerMap.get)
      .reduceOption(co.combine)
      .getOrElse(co.empty)

  override def keySet: Set[A] =
    innerMap.keySet.toSet

  override def values: C[B] =
    getMultiple(innerMap.keys.toSeq *)

  override def put(a: A, b: B): Unit =
    innerMap
      .getOrElseUpdate(a, co.empty)
      .put(b)

  override def remove(a: A)(b: B): Unit =
    innerMap
      .get(a)
      .foreach(
        map =>
          val res = map.remove(b)
          if (res.isEmpty)
            innerMap.remove(a)
          else
            innerMap.put(a, res)
      )

  override def removeA(a: A): C[B] =
    innerMap
      .remove(a)
      .getOrElse(co.empty)

  override def clear(): Unit =
    innerMap.clear()

  override def addEntriesFrom[C2[_]](m: MultiMap[A, B, C2])(using iterateElements: MultiMap.IterateElements[B, C2]): Unit =
    for{
      key <- m.keySet;
      value <- m.get(key).iterator
    }  put(key, value)

}

object MutableMultiMap {
  trait InMapContainerOps[B, C[_]] {

    def empty: C[B]

    def isEmptyC: C[B] => Boolean

    def containsC: C[B] => B => Boolean

    def combine(first: C[B], second: C[B]): C[B]

    def putC: C[B] => B => C[B]

    def removeC: C[B] => B => C[B]

    extension (c: C[B]) {

      def isEmpty: Boolean = isEmptyC(c)

      def contains(b: B): Boolean = containsC(c)(b)

      def ++(other: C[B]): C[B] = combine(c, other)

      def put(b: B): C[B] = putC(c)(b)

      def remove(b: B): C[B] = removeC(c)(b)
    }
  }

  given linkedHashSetOps[B]: InMapContainerOps[B, mutable.LinkedHashSet] with {
    override def empty: mutable.LinkedHashSet[B] = mutable.LinkedHashSet.empty
    override def isEmptyC: mutable.LinkedHashSet[B] => Boolean = _.isEmpty
    override def containsC: mutable.LinkedHashSet[B] => B => Boolean = _.contains
    override def combine(first: mutable.LinkedHashSet[B], second: mutable.LinkedHashSet[B]): mutable.LinkedHashSet[B] = first ++ second
    override def putC: mutable.LinkedHashSet[B] => B => mutable.LinkedHashSet[B] = _.+=
    override def removeC: mutable.LinkedHashSet[B] => B => mutable.LinkedHashSet[B] = (c: mutable.LinkedHashSet[B]) => (b: B) => {
      c.remove(b)
      c
    }
  }

  given hashSetOps[B]: InMapContainerOps[B, mutable.HashSet] with {
    override def empty: mutable.HashSet[B] = mutable.HashSet.empty
    override def isEmptyC: mutable.HashSet[B] => Boolean = _.isEmpty
    override def containsC: mutable.HashSet[B] => B => Boolean = _.contains
    override def combine(first: mutable.HashSet[B], second: mutable.HashSet[B]): mutable.HashSet[B] = first ++ second
    override def putC: mutable.HashSet[B] => B => mutable.HashSet[B] = _.+=
    override def removeC: mutable.HashSet[B] => B => mutable.HashSet[B] = (c: mutable.HashSet[B]) => (b: B) => {
      c.remove(b)
      c
    }
  }

  given arrayBufferOps[B]: InMapContainerOps[B, mutable.ArrayBuffer] with {
    override def empty: mutable.ArrayBuffer[B] = mutable.ArrayBuffer.empty
    override def isEmptyC: mutable.ArrayBuffer[B] => Boolean = _.isEmpty
    override def containsC: mutable.ArrayBuffer[B] => B => Boolean = _.contains
    override def combine(first: mutable.ArrayBuffer[B], second: mutable.ArrayBuffer[B]): mutable.ArrayBuffer[B] = first ++ second
    override def putC: mutable.ArrayBuffer[B] => B => mutable.ArrayBuffer[B] = _.+=
    override def removeC: mutable.ArrayBuffer[B] => B => mutable.ArrayBuffer[B] = (c: mutable.ArrayBuffer[B]) => (b: B) => {
      c.remove(b)
      c
    }
  }

  given seqOps[B]: InMapContainerOps[B, Seq] with {
    override def empty: Seq[B] = Seq.empty
    override def isEmptyC: Seq[B] => Boolean = _.isEmpty
    override def containsC: Seq[B] => B => Boolean = _.contains
    override def combine(first: Seq[B], second: Seq[B]): Seq[B] = first ++ second
    override def putC: Seq[B] => B => Seq[B] = _.put
    override def removeC: Seq[B] => B => Seq[B] = _.remove
  }


}
