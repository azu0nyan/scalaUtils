package utils.math.combinatorics

import utils.math.combinatorics.Combinations.next

import scala.collection.immutable.ArraySeq


object BitSet {

  def first(size:Int) :Array[Boolean] = Array.fill(size)(false)

  def hasNext(set: Array[Boolean]): Boolean = set.contains(false)


  def nextSeq(seq:Seq[Boolean], n:Int):Option[Seq[Boolean]] = {
    val arr = seq.toArray
    Option.when(hasNext(arr)){
      next(arr)
      ArraySeq.unsafeWrapArray(arr)
    }
  }

  /** returns true if has Boolean next */
  def next(set: Array[Boolean]): Unit = {
    for (i <- (set.length - 1) to 0 by -1) {
      if (set(i)) set(i) = false
      else {
        set(i) = true
        return
      }
    }
  }


}
