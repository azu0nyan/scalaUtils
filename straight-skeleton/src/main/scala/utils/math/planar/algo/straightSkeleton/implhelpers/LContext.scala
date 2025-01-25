package utils.math.planar.algo.straightSkeleton.implhelpers

import scala.util.control.Breaks.{break, breakable}

/**
 * Loopl context - an item and it's location in a lopp of loops
 *
 * @author twak
 * @param < E> the type of item
 */
class LContext[E](var loopable: Loopable[E], var loop: Loop[E]) {
  var hook: AnyRef = null // attachement for misc extensions

  def get: E = loopable.get

  /**
   * Assumes target loop has same topology as us, and finds the corresponding LContext
   * for target
   *
   * @param target
   * @return
   */
  def tranlsate[E1](us: LoopL[E], target: LoopL[E1]): LContext[E1] = {
    val usit: Iterator[Loop[E]] = us.iterator
    val tait: Iterator[Loop[E1]] = target.iterator
    var result: LContext[E1] = null

    breakable {
      while (usit.hasNext) {
        val foundLoop: Loop[E1] = tait.next
        if (usit.next == loop) {
          val eIt: Iterator[Loopable[E]] = loop.loopableIterator.iterator
          val tIt: Iterator[Loopable[E1]] = foundLoop.loopableIterator.iterator
          while (eIt.hasNext) {
            val founfLoopable: Loopable[E1] = tIt.next
            if (eIt.next == loopable) {
              result = new LContext[E1](founfLoopable, foundLoop)
              break
            }
          }
          break
        }
      }
      break
    }

    result
  }
}
