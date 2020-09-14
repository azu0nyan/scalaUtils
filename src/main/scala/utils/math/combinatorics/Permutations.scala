package utils.math.combinatorics

import scala.collection.mutable

object Permutations {


  def nextPermutationSeq(per: Seq[Int]): Seq[Int] = {
    val arr = per.toArray
    nextPermutation(arr)
    arr.toSeq
  }

  def elemOrbit(per: Seq[Int], elem: Int): Seq[Int] = {
    var cur = elem
    val res: mutable.Buffer[Int] = mutable.Buffer()
    do {
      res += cur
      cur = per(cur)
    } while (cur != elem)
    res.toSeq
  }

  def allCycles(per: Seq[Int]): Seq[Seq[Int]] = {
    val acc: mutable.Buffer[Seq[Int]] = mutable.Buffer()
    val checked = Array.ofDim[Boolean](per.size)
    for (i <- checked.indices) {
      if (!checked(i)) {
        val cycle: mutable.Buffer[Int] = mutable.Buffer()
        var cur = i
        do {
          cycle += cur
          checked(cur) = true
          cur = per(cur)
        } while (!checked(cur))
        acc += cycle.toSeq
      }
    }
    acc.toSeq
  }

  def toCanonicalCyclicNotation(per: Seq[Int]): String = {
    val cycles = allCycles(per).filterNot(_.size == 1)
    if (cycles.isEmpty) "()"
    else cycles.map(_.mkString("(", ",", ")")).mkString("")
  }

  def fromCyclicNotation(notation:String, size:Int):Seq[Int] = {
    val res = (0 until size).toArray
    notation.split("\\)").map(_.replace("(","").split(",").flatMap(_.toIntOption))
      .map(c => c :+ c.head).foreach(c => c.sliding(2).foreach(p => res(p(0)) = p(1)))
    res.toSeq
  }


  def allPermutations(elems: Int): Iterator[IndexedSeq[Int]] = new Iterator[IndexedSeq[Int]] {
    var currentId: Long = 0
    val maxId: Long = factorial(elems)
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
