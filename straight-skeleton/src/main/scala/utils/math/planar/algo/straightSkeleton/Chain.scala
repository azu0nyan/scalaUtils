package utils.math.planar.algo.straightSkeleton

import scala.util.control.Breaks.{break, breakable}
import java.{util => j}
import java.util.ArrayList
import java.util.{List, Set}


/**
 * A Chain is a set of edges colliding at a point. Described by the first
 * corner of each face.
 *
 * @author twak
 */
object Chain { // marker for having degraided from a loop to a list
  private val DELOOP = new Chain(new j.ArrayList[Corner], false)
  trait Condition[E] {
    def isTrue: Boolean
  }
}
class Chain(var chain: j.List[Corner], var loop: Boolean) {

  def this(chain: j.List[Corner]) = {
    this(chain, chain.get(chain.size - 1).nextC eq chain.get(0))
  }
  /**
   * @param index first element of the split
   */
  private def split(index: Int): Chain = // decompose a loop a the given index
    if (loop) {
      val nc = new j.ArrayList[Corner]
      nc.addAll(chain.subList(index, chain.size))
      nc.addAll(chain.subList(0, index))
      loop = false
      chain = nc
      Chain.DELOOP
    } else if (index == 0) null // first element already split
    else {
      val nc = new j.ArrayList[Corner]
      nc.addAll(chain.subList(0, index))
      chain = chain.subList(index, chain.size)
      new Chain(nc)
    }
  /**
   *
   * @param liveEdges
   * @return list of new chains, in order, that should be appended to the
   *         master list before this chain.
   */
  def removeCornersWithoutEdges(liveEdges: j.Set[Edge]): j.List[Chain] = {
    val newChains = new j.ArrayList[Chain]

    breakable {
      while (true) {
        var i = 0
        while (i < chain.size) {
          val c = chain.get(i)
          if (!liveEdges.contains(c.nextL)) {
            val n = split(i)
            chain.remove(0) // removed the specified element

            if (n eq Chain.DELOOP) {
              //restart, next time first element will split with no effect
              newChains.addAll(removeCornersWithoutEdges(liveEdges))
              return newChains
            }
            if (n != null) newChains.add(n)
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
  def splitChainsIfHorizontal(horizontals: j.Set[Corner]): j.List[Chain] = {
    val newChains = new j.ArrayList[Chain]

    breakable {
      while (true) {
        var i = 0
        while (i < chain.size) {
          val c = chain.get(i)
          if (horizontals.contains(c)) {
            val n = split(i)
            if (n eq Chain.DELOOP) {
              //restart, next time first element will split with no effect
              newChains.addAll(splitChainsIfHorizontal(horizontals))
              return newChains
            }
            if (n != null) newChains.add(n)
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

