package utils.math.planar.algo.straightSkeleton.helpers


import java.util

class ConsecutivePairs[E](var input: List[E], var loop: Boolean) extends Iterator[Pair[E, E]] with Iterable[Pair[E, E]] {
  var a = 0
  var size = input.size
  override def hasNext =
    if (this.size != 0 && (this.size != 1 || this.loop))
      this.a >= 0
    else
      false
  override def next =
    if (this.size == 0) null
    else {
      val out = new Pair[E, E](this.input(this.a % this.size), this.input((this.a + 1) % this.size))
      this.a += 1
      if (!this.loop && this.a == this.size - 1) this.a = -1
      else if (this.loop && this.a == this.size) this.a = -1
      out
    }
  override def iterator = this
}

