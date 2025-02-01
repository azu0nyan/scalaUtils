package utils.math.planar.algo.straightSkeleton.implhelpers

import utils.datastructures.containers.map.impl.MutableMultiMap

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * When you have a many -> many correspondence, you want this class!
 *
 * @author twak
 */
class ManyManyMap[A, B] {
  private var forwards = new MutableMultiMap[A, B, ArrayBuffer]
  private var backwards = new MutableMultiMap[B, A, ArrayBuffer]

  def addForwards(from: A, to: B): Unit = {
    forwards.put(from, to)
    backwards.put(to, from)
  }

  def getNext(from: A) = forwards.get(from)
  def getPrev(to: B) = backwards.get(to)

  /**
   * //todo dupecheck
   * Given a specified cache will convert everything that is an A to C. That
   * is the element type in the from will go from A to C.
   *
   * @param < C> the replacement for A
   */
  def ConvertInputCollection[C, COL <: collection.Iterable[C]](converter: Cache[A, COL]) : ManyManyMap[C, B] =  {
      val out = new ManyManyMap[C, B]
      for {
        key <- forwards.keySet
        c <- converter.get(key)
        value <- forwards.get(key)
      } {
        out.forwards.put(c, value) //todo dupecheck
      }

      for {
        key <- backwards.keySet
        a <- backwards.get(key)
        value <- converter.get(a)
      } {
        out.backwards.put(key, value) //todo dupecheck
      }

      out
  }

  class ConvertOutputCollection[C](var converter: Cache[B, ArrayBuffer[C]]) {
    def get = {
      val out = new ManyManyMap[A, C]

      for {
        key <- forwards.keySet
        value <- forwards.get(key)
        c <- converter.get(value)
      } {
        out.forwards.put(key, c) //todo dupecheck
      }

      for {
        key <- backwards.keySet
        c <- converter.get(key)
        value <- backwards.get(key)
      } {
        out.backwards.put(c, value) //todo dupecheck
      }

      out
    }
  }


  def toSetCorrespondence = {
    val out = new SetCorrespondence[A, B]
    for (a <- forwards.keySet) {
      for (b <- forwards.get(a)) {
        out.put(a, b)
      }
    }
    for (b <- backwards.keySet) {
      for (a <- backwards.get(b)) {
        out.put(a, b)
      }
    }
    out
  }

  def getFlipShallow = {
    val out = new ManyManyMap[B, A]
    out.backwards = forwards
    out.forwards = backwards
    out
  }
}
