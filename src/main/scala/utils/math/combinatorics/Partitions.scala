package utils.math.combinatorics

import utils.math.combinatorics.BitSet.hasNext

import scala.collection.immutable.ArraySeq


/**with zeros*/
object Partitions {

  def first(n: Int): Array[Int] = Array.fill(n)(1)

  def nextSeq(seq:Seq[Int]):Option[Seq[Int]] = {
    val arr = seq.toArray
    Option.when(next(arr))(ArraySeq.unsafeWrapArray(arr))
  }

  def next(p: Array[Int]): Boolean = {
    for (i <- (p.length - 1) to 1 by -1) {
      if (p(i) != 0) {
        p(i) -= 1
        p(i - 1) += 1
        if (p(i) <= p(i - 1)) {
          p(i - 1) += p(i)
          p(i) = 0
        } else {
          var j = i
          while (p(j - 1) * 2 <= p(j)) {
            p(j + 1) = p(j) - p(j - 1)
            p(j) = p(j - 1)
            j += 1
          }
        }
        return true
      }
    }
    return false
  }

}
