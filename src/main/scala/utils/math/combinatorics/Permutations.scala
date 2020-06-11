package utils.math.combinatorics

object Permutations {

  def allPermutations(elems: Int): Iterator[IndexedSeq[Int]] = new Iterator[IndexedSeq[Int]] {
    var currentId: Long = 0
    val maxId:Long = factorial(elems)
    val currentArray: Array[Int] = (0 until elems) toArray
    override def hasNext: Boolean = currentId < maxId
    override def next(): IndexedSeq[Int] = {
      val res = currentArray.toIndexedSeq
      if (hasNext) nextPermutation(currentArray)
      currentId += 1
      res
    }
  }


  ////PERMUTATIONS
  def nextPermutation(per: Array[Int]): Unit = {
    for (i <- per.length - 2 to 0 by -1) {
      if (per(i) < per(i + 1)) {
        var j = 0
        j = per.length - 1
        var cont = true
        while (cont && j >= 0) {
          if (per(j) > per(i)) cont = false
          else j -= 1
        }
        //swap
        val t = per(i)
        per(i) = per(j)
        per(j) = t

        var l = i + 1
        var r = per.length - 1
        while ( {
          l < r
        }) {
          val t = per(l)
          per(l) = per(r)
          per(r) = t
          l += 1
          r -= 1
        }
        return
      }
    }
    var l = 0
    var r = per.length - 1
    while (l < r) {
      val t = per(l)
      per(l) = per(r)
      per(r) = t
      l += 1
      r -= 1
    }
  }

}
