package utils.math.planar.algo.straightSkeleton.helpers

import java.{util, util as j}
import scala.collection.mutable


/**
 * a loop of loops, with an iterator for the contained primitive (corners!)
 */

class LoopL[E](
                loops: Loop[E]*
              ) {

  val inner: mutable.Buffer[Loop[E]] = mutable.Buffer.apply(loops *)

  def iterator = inner.iterator

  def eIterator = new EIterable
  class EIterable extends Iterable[E] {
    override def iterator = new ItIt[E](inner)
  }

  def add(loop: Loop[E]) = inner += loop

  def addLoopL(e: LoopL[E]): Unit = {
    inner.addAll(e.inner)
  }

  def count: Int = {
    var i = 0
    for (l <- inner) {
      for (e <- l) {
        i += 1
      }
    }
    i
  }

  def reverseEachLoop(): Unit = {
    for (loop <- inner) {
      loop.reverse
    }
  }

  def getCIterator = new ContextIt
  def getCIterable = new Iterable[LContext[E]]() {
    override def iterator: Iterator[LContext[E]] = getCIterator
  }
  class ContextIt extends Iterator[LContext[E]] {
    var loopIt: Iterator[Loop[E]] = LoopL.this.iterator
    var loopableIt: Iterator[Loopable[E]] = null

    // next values to return
    var loopable: Loopable[E] = null
    var loop: Loop[E] = null

    findNext()

    private def findNext(): Unit = {
      if (loopIt == null) () // finished!
      else if (loopableIt != null && loopableIt.hasNext) // start 
        loopable = loopableIt.next
      else if (loopIt.hasNext) {
        loop = loopIt.next
        loopableIt = loop.loopableIterator.iterator
        findNext()
      }
      else loopIt = null
    }
    override def hasNext = loopIt != null
    override def next = {
      val out = new LContext[E](loopable, loop)
      findNext()
      out
    }
  }

  def getLoopableIterator = new LoopableLIterator
  def getLoopableIterable = new Iterable[Loopable[E]]() {
    override def iterator: Iterator[Loopable[E]] = new LoopableLIterator
  }

  class LoopableLIterator extends Iterator[Loopable[E]] {
    var loopIt: Iterator[Loop[E]] = LoopL.this.iterator
    var loopableIt: Iterator[Loopable[E]] = null
    // next values to return
    var loopable: Loopable[E] = null
    var loop: Loop[E] = null

    findNext()
    private def findNext(): Unit = {
      if (loopIt == null) () // finished!
      else if (loopableIt != null && loopableIt.hasNext // start
      ) loopable = loopableIt.next
      else if (loopIt.hasNext) {
        loop = loopIt.next
        loopableIt = loop.loopableIterator.iterator
        findNext()
      }
      else loopIt = null
    }
    override def hasNext = loopIt != null
    override def next = {
      val out = loopable
      findNext()
      out
    }
  }

  abstract class Map[O] {
    def run = {
      val out = new LoopL[O]
      for (loopE <- LoopL.this.iterator) {
        val loopO = new Loop[O]
        out.add(loopO)
        for (e <- loopE.loopableIterator) {
          loopO.append(map(e))
        }
        for (hole <- loopE.holes) {
          val loopHoleO = new Loop[O]
          for (e <- hole.loopableIterator) {
            loopHoleO.append(map(e))
          }
          loopO.holes += loopHoleO
        }
      }
      out
    }
    def map(input: Loopable[E]): O
  }

  def find(e: E): LoopL.LoopLoopable[E] = {
    for (loop <- this.iterator) {
      val lp = loop.find(e)
      if (lp != null) return new LoopL.LoopLoopable[E](loop, lp)
    }
    null
  }

  def loop = {
    val out = new Loop[E]
    add(out)
    out
  }

  def newLoop = {
    val out = new Loop[E]
    add(out)
    out
  }

  def isEmpty: Boolean =
    iterator.exists(_.nonEmpty)
}

object LoopL {
  class LoopLoopable[E](var loop: Loop[E], var loopable: Loopable[E])
}

