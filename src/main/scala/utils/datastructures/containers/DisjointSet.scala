package utils.datastructures.containers

import scala.annotation.tailrec
import scala.collection.mutable

class DisjointSet[@specialized(Int, Long, Char, Byte, Short) T] {

  val parent: mutable.Map[T, T] = mutable.Map()
  val rank:mutable.Map[T, Int] = mutable.Map()

  def makeSet(t: T): Unit = {
    parent += t -> t
    rank += t -> 0
  }

  def contains(t: T): Boolean = parent.contains(t)

  final def findSet(t: T): T = {
    parent(t) match {
      case `t` => t
      case x =>
        val xParent = findSet(x)
        parent(x) = xParent
        xParent
    }
  }

  def containingSetOpt(t: T): Option[T] = Option.when(contains(t))(findSet(t))

  def union(a: T, b: T): Unit = {
    val aParent = findSet(a)
    val bParent = findSet(b)
    if (aParent != bParent) {
      val ra = rank(aParent)
      val rb = rank(bParent)
      if(ra < rb){
        parent(aParent) = bParent
      } else if(ra < rb){
        parent(bParent) = aParent
      } else {
        parent(bParent) = aParent
        rank(aParent) = ra + 1
      }
    }
  }

}
