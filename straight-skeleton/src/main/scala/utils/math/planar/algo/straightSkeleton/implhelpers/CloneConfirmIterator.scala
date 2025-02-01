package utils.math.planar.algo.straightSkeleton.implhelpers

import scala.collection.mutable

/**
 * An iterator that wraps a set to give an interator that
 * checks for item removal from the original set before returning it.
 * Note that this does not check for addition to the set (you will only get
 * a subset of the objects in the original set, nothing that is added
 * after this iterator is constructed)
 *
 * @author twak
 */
class CloneConfirmIterator[E](original: collection.Set[E]) extends Iterator[E] {
  val it = {
    val set = new mutable.LinkedHashSet[E]()
    set ++= original
    set.iterator
  }

  var _next: Option[E] = None
  findNext()

  override def hasNext = _next.nonEmpty
  
  override def next: E = {
    val out = _next.get
    findNext()
    out
  }
  
  private def findNext(): Unit = {
    var cont = true
    while (cont) {
      if (!it.hasNext) {
        _next = None
        cont = false
      } else {
        _next = Some(it.next)
        cont = !original.contains(_next.get)
      }
    }
  }
  
}


