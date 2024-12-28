package utils.datastructures.containers.map

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
    def put(a: A, b: B): Unit
    def remove(a: A)(b: B): Unit
    def removeA(a: A): C[B]
    def clear(): Unit
  }
}
