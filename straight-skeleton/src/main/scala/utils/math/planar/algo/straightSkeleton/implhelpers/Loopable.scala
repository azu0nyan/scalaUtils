package utils.math.planar.algo.straightSkeleton.implhelpers

import java.util

class Loopable[E](var me: E) extends Iterable[Loopable[E]] {
  var next: Loopable[E] = null
  var prev: Loopable[E] = null
  override def toString = s"L<${if (me == null) "null" else me}>"
  def get = me
  def set(e: E): Unit = {
    me = e
  }
  def getNext = next
  def getPrev = prev
  def setNext(s: Loopable[E]): Unit = {
    next = s
  }
  def setPrev(s: Loopable[E]): Unit = {
    prev = s
  }

  def move(dir: Int): Loopable[E] = {
    var dirCounter = dir
    var out = this
    while (dirCounter > 0) {
      dirCounter -= 1
      out = out.getNext
    }
    while (dirCounter < 0) {
      dirCounter += 1
      out = out.getPrev
    }
    out
  }

  override def iterator = new LoopableIterator(this)
  def count: Int = {
    var count = 0

    for (e <- this.iterator) {
      count += 1
    }
    count
  }

  class LoopableIterator(var start: Loopable[E]) extends Iterator[Loopable[E]] {
    var n: Loopable[E] = null

    override def hasNext: Boolean = {
      if (start == null) false
      else if (n == null) true
      else n ne start
    }

    override def next = {
      if (n == null) n = start
      val out = n
      n = n.getNext
      out
    }
  }

  class LoopIterator(start: Loopable[E]) extends Iterator[E] {
    var lit: LoopableIterator = new LoopableIterator(start)
    override def hasNext = lit.hasNext
    override def next = lit.next.me
  }

}
