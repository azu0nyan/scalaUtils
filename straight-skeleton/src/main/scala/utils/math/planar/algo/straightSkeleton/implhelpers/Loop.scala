package utils.math.planar.algo.straightSkeleton.implhelpers

import java.util as j
import scala.collection.mutable

/**
 * Double linked-list wrapper around an arbitrary object
 */
class Loop[E](
               es: E*
             ) extends Iterable[E] {
  var start: Loopable[E] = null
  var holes = mutable.Buffer[Loop[E]]()

  append(es *)


  def count: Int = {
    var count = 0
    for (e <- this.iterator) {
      count += 1
    }
    count
  }

  def removeAll(): Unit = {
    start = null
  }

  def append(append: E*) = {
    var last: Loopable[E] = null
    for (e <- append) {
      last = append_(e)
    }
    last
  }

  def append_(append: E): Loopable[E] =
    if (start == null) {
      start = new Loopable[E](append)
      start.setNext(start)
      start.setPrev(start)
      start
    } else {
      val toAdd = new Loopable[E](append)
      toAdd.setPrev(start.getPrev)
      toAdd.setNext(start)
      start.getPrev.setNext(toAdd)
      start.setPrev(toAdd)
      toAdd
    }

  def prepend(prepend: E): Loopable[E] = {
    start = append(prepend)
    start
  }

  def addAfter(loopable: Loopable[E], bar: E) = {
    val n = new Loopable[E](bar)
    if (loopable == null) {
      start = n
      start.setNext(start)
      start.setPrev(start)
    }
    else {
      n.setPrev(loopable)
      n.setNext(loopable.next)
      n.getPrev.setNext(n)
      n.getNext.setPrev(n)
    }
    n
  }

  def remove(toRemove: E): Unit = {
    val togo = find(toRemove)
    remove(togo)
  }

  def remove(togo: Loopable[E]): Unit = {
    if (togo == start)
      if (togo.prev == togo)
        start = null
      else
        start = togo.prev
    togo.prev.next = togo.next
    togo.next.prev = togo.prev
  }

  def find(toFind: E): Loopable[E] = {
    var n = start
    while (n.next != start) {
      if (n.me == toFind) return n
      n = n.next
    }
    if (n.me == toFind) return n
    null
  }

  def loopableIterator = new Iterable[Loopable[E]]() {
    override def iterator: Iterator[Loopable[E]] = new LoopableIterator
  }

  override def iterator = new LoopIterator
  def reverse: Loop[E] = {
    if (start == null)
      return this
    var m = start
    var first = true
    while (first || m != start) {
      first = false
      val tmp = m.next
      m.next = m.prev
      m.prev = tmp
      m = m.prev // reversed ;)

    }
    this
  }

  class LoopableIterator extends Iterator[Loopable[E]] {
    var s: Loopable[E] = start
    var n: Loopable[E] = null

    override def hasNext: Boolean = {
      if (s == null) false
      else if (n == null) true
      else n != start
    }
    override def next = {
      if (n == null) n = start
      val out = n
      n = n.getNext
      out
    }
  }

  class LoopIterator extends Iterator[E] {
    val lit = new LoopableIterator
    override def hasNext = lit.hasNext
    override def next = lit.next.me
  }


  def singleton: LoopL[E] = {
    val out = new LoopL[E]
    out.add(this)
    out
  }
}

