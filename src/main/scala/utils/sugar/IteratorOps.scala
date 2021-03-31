package utils.sugar

object IteratorOps {
  def iterateOpt[T](t:Option[T])(f:T =>Option[T]):Iterator[T] = new Iterator[T] {
    var cur:Option[T] = t
    override def hasNext: Boolean = cur.nonEmpty
    override def next(): T = {
      val res = cur.get
      cur = f(cur.get)
      res
    }
  }

}
