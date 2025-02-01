package utils.math.planar.algo.straightSkeleton


class ConsecutiveTriples[E](
                             var input: collection.Seq[E],
                             var loop: Boolean,
                           ) extends Iterator[(E, E, E)] {

  var a = 0
  override def size = input.size
  
  if (input.size < 3) a = -1

  override def hasNext = a >= 0
  override def next = {
    val out = (
      input(a % size),
      input((a + 1) % size),
      input((a + 2) % size)
    )
    a += 1
    if (!loop && a == size - 2) a = -1
    else if (loop && a == size) a = -1
    out
  }
}

