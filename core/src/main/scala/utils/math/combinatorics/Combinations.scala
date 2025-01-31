package utils.math.combinatorics

import scala.collection.immutable.ArraySeq

object Combinations {

  def first(n:Int, k:Int) :Array[Int] = Array.iterate[Int](0, k)(_ + 1)

  def nextSeq(seq:Seq[Int], n:Int):Option[Seq[Int]] = {
    val arr = seq.toArray
    Option.when(next(arr,n))(ArraySeq.unsafeWrapArray(arr))
  }

  /** from 0 to n-1 */
  def next(array: Array[Int], n: Int): Boolean = {
    val k = array.length - 1 //
    for (i <- k to 0 by -1) {
      val currMax = n - (k  - i + 1)
      if (array(i) < currMax) {
        array(i) += 1
        for (j <- (i + 1) to k) array(j) = array(j - 1) + 1
        return true
      }

    }
    return false
  }
}

