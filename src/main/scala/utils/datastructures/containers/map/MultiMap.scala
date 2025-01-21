package utils.datastructures.containers.map

import scala.collection.mutable

/** Map A -> C[B] */
trait MultiMap[A, B, C[_]] {
  def size: Int
  def isEmpty: Boolean
  def containsKey(a: A): Boolean
  def contains(a: A, b: B): Boolean
  def get(a: A): C[B]
  def getMultiple(a: A*): C[B]
  def keySet: Set[A]
  def values: C[B]
}

object MultiMap {
  trait Mutations[A, B, C[_]] {
    final def +=(a: A, b: B): Unit = put(a, b)
    def put(a: A, b: B): Unit
    final def -=(a: A, b: B): Unit = remove(a)(b)
    def remove(a: A)(b: B): Unit
    final def -=(a: A): Unit = removeA(a)
    def removeA(a: A): C[B]
    def clear(): Unit
    def ++=[C2[_]](m: MultiMap[A, B, C2])(
      using iterateElements: IterateElements[B, C2]
    ): Unit = addEntriesFrom(m)
    def addEntriesFrom[C2[_]](
                               m: MultiMap[A, B, C2]
                             )(
                               using iterateElements: IterateElements[B, C2]
                             ): Unit
  }

  trait IterateElements[A, C[_]] {
    def elements: C[A] => Iterator[A]

    extension (c: C[A]) {
      def iterator: Iterator[A] = elements(c)
    }
  }

  given iterateLinkedHashSet[A]: IterateElements[A, mutable.LinkedHashSet] with {
    override def elements: mutable.LinkedHashSet[A] => Iterator[A] = _.iterator
  }

  given iterateHashSet[A]: IterateElements[A, mutable.HashSet] with {
    override def elements: mutable.HashSet[A] => Iterator[A] = _.iterator
  }

  given iterateArrayBuffer[A]: IterateElements[A, mutable.ArrayBuffer] with {
    override def elements: mutable.ArrayBuffer[A] => Iterator[A] = _.iterator
  }
}
