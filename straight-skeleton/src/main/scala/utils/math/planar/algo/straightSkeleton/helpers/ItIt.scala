package utils.math.planar.algo.straightSkeleton.helpers

class ItIt[E](var list: Iterable[Iterable[E]]) extends Iterator[E] {
  var it: Iterator[Iterable[E]] = list.iterator
  var it2: Iterator[E] = null
  var nextE: Option[E] = None
  findNext()


  private def findNext(): Unit = {
    if (it2 != null && it2.hasNext)
      nextE = Some(it2.next)
    else if (it.hasNext) {
      it2 = it.next.iterator
      findNext()
    }
    else nextE = None
  }
  override def hasNext = nextE.nonEmpty

  override def next = {
    val out = nextE
    findNext()
    out
  }
}

