package utils.datastructures.containers.map

trait BiMap[A, B] {
  def get(a: A): Option[B]
  def teg(b: B): Option[A]
  def containsA(a: A): Boolean
  def containsB(b: B): Boolean
}

object BiMap {
  trait Mutations[A, B] {
    def put(a: A, b: B): Unit
    def removeA(a: A): Unit
    def removeB(b: B): Unit
  }

  trait ShallowDupe[A, B, D] {
    def shallowDupe(): D
  }
}