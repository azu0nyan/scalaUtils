package utils.math.planar.algo.straightSkeleton.helpers


import java.util

class ConsecutivePairs[E](var input: collection.Seq[E], var loop: Boolean) extends Iterator[(E, E)] {

  var a = 0
  override def size = input.size

  override def hasNext =
    if (size != 0 && (size != 1 || loop))
      a >= 0
    else
      false

  override def next =
    if (size == 0) null
    else {
      val out = (
        input(a % size),
        input((a + 1) % size)
      )
      a += 1
      if (!loop && a == size - 1) a = -1
      else if (loop && a == size) a = -1
      out
    }

}

