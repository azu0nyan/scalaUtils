package utils.math.planar.algo.straightSkeleton

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}


/**
 * A Chain is a set of edges colliding at a point. Described by the first
 * corner of each face.
 *
 * @author twak
 */
object Chain { // marker for having degraided from a loop to a list
  private val DELOOP = new Chain(mutable.Buffer[Corner](), false)
  trait Condition[E] {
    def isTrue: Boolean
  }
}
class Chain(var chain: mutable.Buffer[Corner], var loop: Boolean) {

  def this(chain: mutable.Buffer[Corner]) = {
    this(chain, chain.last.nextC eq chain.head)
  }
  /**
   * @param index first element of the split
   */
  private def split(index: Int): Chain = // decompose a loop a the given index
    if (loop) {
      val nc = mutable.Buffer[Corner]()
      nc.addAll(chain.slice(index, chain.size))
      nc.addAll(chain.slice(0, index))
      loop = false
      chain = nc
      Chain.DELOOP
    } else if (index == 0) null // first element already split
    else {
      val nc = mutable.Buffer[Corner]()
      nc.addAll(chain.slice(0, index))
      chain = chain.slice(index, chain.size)
      new Chain(nc)
    }
  /**
   *
   * @param liveEdges
   * @return list of new chains, in order, that should be appended to the
   *         master list before this chain.
   */
  def removeCornersWithoutEdges(liveEdges: collection.Set[Edge]): mutable.Buffer[Chain] = {
    val newChains = mutable.Buffer[Chain]()

    breakable {
      while (true) {
        var i = 0
        while (i < chain.size) {
          val c = chain(i)
          if (!liveEdges.contains(c.nextL)) {
            val n = split(i)
            chain.remove(0) // removed the specified element

            if (n eq Chain.DELOOP) {
              //restart, next time first element will split with no effect
              newChains.addAll(removeCornersWithoutEdges(liveEdges))
              return newChains
            }
            if (n != null) newChains += n
            i = -1 // process element 0 next time

          }

          i += 1
        }
        // iterated entire chain - done!
        break
      }
    }
    newChains
  }
  /**
   * @return a list of additional chains after split has been performed
   */
  def splitChainsIfHorizontal(horizontals: Set[Corner]): mutable.Buffer[Chain] = {
    val newChains = mutable.Buffer[Chain]()

    breakable {
      while (true) {
        var i = 0
        while (i < chain.size) {
          val c = chain(i)
          if (horizontals.contains(c)) {
            val n = split(i)
            if (n eq Chain.DELOOP) {
              //restart, next time first element will split with no effect
              newChains.addAll(splitChainsIfHorizontal(horizontals))
              return newChains
            }
            if (n != null) newChains += n
            i = 0 // process element 1 next time

          }

          i += 1
        }
        // iterated entire chain - done!
        break

      }
    }
    newChains
  }
  override def toString = chain.toString
}

