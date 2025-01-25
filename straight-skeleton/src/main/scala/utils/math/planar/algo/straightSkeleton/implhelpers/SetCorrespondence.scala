package utils.math.planar.algo.straightSkeleton.implhelpers

import utils.datastructures.containers.map.BiMap
import utils.datastructures.containers.map.impl.MutableBiMap

import scala.collection.mutable

class SetCorrespondence[A, B] {
  var whichA = new mutable.LinkedHashMap[A, mutable.Set[A]]
  var whichB = new mutable.LinkedHashMap[B, mutable.Set[B]]
  val setToSet = new MutableBiMap[mutable.Set[A], mutable.Set[B]]
  /**
   * If either a or b belongs to an existing set, both a and b collapse into that a/b set pair.
   * Symmetrical operation.
   *
   * @param a the A
   * @param b the B
   */
  def put(a: A, b: B): Unit = {
    var aSet = getSet(a, whichA)
    var bSet = getSet(b, whichB)
    val shouldBeA = setToSet.teg(bSet)
    val shouldBeB = setToSet.get(aSet)
    if (shouldBeA.nonEmpty && (shouldBeA ne aSet)) // existing B set already references another A set
    {
      shouldBeA.get ++= aSet
      for (a2 <- aSet) {
        whichA.put(a2, shouldBeA.get)
      }
      aSet = shouldBeA.get
    }
    else whichA.put(a, aSet) // either new set, or

    if (shouldBeB.nonEmpty && (shouldBeB ne bSet)) // a references another b (not bSet)
    {
      shouldBeB.get ++= bSet
      for (b2 <- bSet) {
        whichB.put(b2, shouldBeB.get)
      }
      bSet = shouldBeB.get
    }
    else whichB.put(b, bSet)
    setToSet.put(aSet, bSet)
  }

  def getSetA(a: A): mutable.Set[B] = {
    val aSet = getSet(a, whichA)
    val bSet = setToSet.get(aSet)
    bSet match
      case Some(bSet) => bSet
      case None => new mutable.HashSet[B]()
  }

  def getSetB(b: B): mutable.Set[A] = {
    val bSet = getSet(b, whichB)
    val aSet = setToSet.teg(bSet)
    aSet match
      case Some(aSet) => aSet
      case None => new mutable.HashSet[A]()
  }

  private def getSet[X](o: X, set: mutable.Map[X, mutable.Set[X]]): mutable.Set[X] = {
    set.get(o) match
      case Some(res) => res
      case None =>
        val res = new mutable.LinkedHashSet[X]
        res.add(o)
        set.put(o, res)
        res
  }

  def removeA(a: A): Unit = {
    val aSet = getSet(a, whichA)
    aSet.remove(a)
    whichA.remove(a)
    if (aSet.isEmpty) setToSet.removeA(aSet)
  }

  def asCache = new Cache[A, mutable.Set[B]]() {
    override def create(i: A): mutable.Set[B] = getSetA(i)
  }

}

