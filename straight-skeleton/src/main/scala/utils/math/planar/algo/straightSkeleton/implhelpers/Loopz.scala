package utils.math.planar.algo.straightSkeleton.implhelpers

import utils.math.space.V3

object Loopz {
  def area3(loop: Loop[V3]) = {
    val origin = loop.iterator.next
    var areaR: Double = 0

    for (pt <- loop.loopableIterator.iterator) {
      areaR += area(origin, pt.getNext.get, pt.get)
    }

    areaR
  }

  def area3(insideOutside: LoopL[V3]): Double =
    insideOutside.iterator
      .map((x: Loop[V3]) => area3(x))
      .sum

  def area(a: V3, b: V3, c: V3): Double = {
    val ab = b - a
    val ac = c - a
    val cross = ab ^ ac
    0.5d * cross.length
  }
}
