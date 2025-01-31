package utils.math.planar.algo.straightSkeleton

import utils.math.planar.V2
import utils.math.space.V3
import utils.math.space.V3
import utils.math.planar.algo.straightSkeleton.implhelpers.{Cache, Loop, LoopL}
import utils.math.space.V3

import scala.jdk.CollectionConverters.IteratorHasAsScala

/**
 *
 * @author twak
 */
object Corner {

  def replace(old: Corner, neu: Corner, skel: Skeleton): Unit = {
    old.prevL.currentCorners.remove(old)
    old.nextL.currentCorners.remove(old)
    old.nextC.prevC = neu
    old.prevC.nextC = neu
    neu.prevC = old.prevC
    neu.nextC = old.nextC
    neu.nextL = old.nextL
    neu.prevL = old.prevL
    neu.prevL.currentCorners.add(neu)
    neu.nextL.currentCorners.add(neu)
    skel.liveCorners.remove(old)
    skel.liveCorners.add(neu)
  }
  /**
   * Clones this set of corners.
   *
   * Creates new edges, corners and machines from the given set.
   * Preserves currentCorners.
   *
   * @param ribbon
   * @return
   */
  def dupeNewAll(ribbon: LoopL[Corner]) = {
    val cacheC = new Cache[Corner, Corner]() {
      val cacheM = new Cache[Machine, Machine]() {
        override def create(i: Machine) = new Machine(i.currentAngle)
      }
      val cacheE = new Cache[Edge, Edge]() {
        override def create(i: Edge) = {
          val out = new Edge(getCorner(i.start), getCorner(i.end))
          out.setAngle(i.getAngle)
          out.machine = cacheM.get(i.machine)
          for (c <- i.currentCorners) {
            out.currentCorners.add(getCorner(c))
          }
          out
        }
      }
      def getCorner(input: Corner) = get(input)

      override def create(i: Corner) = {
        val ner = new Corner(i.x, i.y)
        cache.put(i, ner)
        ner.nextC = get(i.nextC) // Cache<Corner,Corner>.get()

        ner.prevC = get(i.prevC)
        ner.nextL = cacheE.get(i.nextL)
        ner.prevL = cacheE.get(i.prevL)
        ner
      }
    }
    val loopl = new LoopL[Corner]

    for (pLoop <- ribbon.iterator) {
      val loop = new Loop[Corner]
      loopl.add(loop)

      for (c <- pLoop) {
        loop.append(cacheC.get(c))
      }
    }
    loopl
  }
  def dupeNewAllPoints(ribbon: LoopL[V3]) = {
    val cacheC = new Cache[V3, V3]() {
      override def create(i: V3) = i
    }
    val loopl = new LoopL[V3]

    for (pLoop <- ribbon.iterator) {
      val loop = new Loop[V3]
      loopl.add(loop)

      for (c <- pLoop) {
        loop.append(cacheC.get(c))
      }
    }
    loopl
  }
  def dupeNewAllPoints(ribbon: LoopL[V3], height: Double) = {
    val cacheC = new Cache[V3, V3]() {
      override def create(i: V3) = new V3(i.x, i.y, height)
    }
    val loopl = new LoopL[V3]

    for (pLoop <- ribbon.iterator) {
      val loop = new Loop[V3]
      loopl.add(loop)

      for (c <- pLoop) {
        loop.append(cacheC.get(c))
      }
    }
    loopl
  }
  //  def fromBar(ribbon: LoopL[Bar]) = {
  //    val loopl = new LoopL[Bar]
  //    val cache = new Cache[V2, Corner]() {
  //      override def create(i: V2) = new Corner(i.x, i.y)
  //    }
  //
  //    for (pLoop <- ribbon) {
  //      val loop = new Loop[Bar]
  //      loopl.add(loop)
  //
  //      for (bar <- pLoop) {
  //        loop.append(cache.get(bar.start))
  //      }
  //    }
  //
  //    for (loop <- loopl) {
  //
  //      for (loopable <- loop.loopableIterator) {
  //        val p = loopable.get
  //        val n = loopable.getNext.get
  //        p.nextC = n
  //        n.prevC = p
  //        val e = new Edge(p, n)
  //        p.nextL = e
  //        n.prevL = e
  //      }
  //    }
  //    loopl
  //  }

  def cornerToEdgeLoopL(in: LoopL[Edge]) = {
    val corners = new LoopL[Corner]

    for (le <- in.iterator) {
      val lc = new Loop[Corner]
      corners.add(lc)

      for (e <- le) {
        lc.append(e.start)
        e.start.nextL = e
        e.end.prevL = e
        e.start.nextC = e.end
        e.end.prevC = e.start
      }
    }
    corners
  }
}
class Corner(
              var x: Double,
              var y: Double,
              var z: Double
            ) {
  var nextL: Edge = null
  var prevL: Edge = null
  var nextC: Corner = null
  var prevC: Corner = null

  def this(in: V3) = this(in.x, in.y, in.z)

  def this(x: Double, y: Double) = this(x, y, 0)

  def this(p: V2) = this(p.x, p.y, 0)

  def distance(other: Corner) =
    scala.math.sqrt((x - other.x) * (x - other.x) + (y - other.y) * (y - other.y) + (z - other.z) * (z - other.z))

  def getLoc3 = new V3(x, y, 0)

  def asV3 = new V3(x, y, z)

  /**
   * Corners (unlike V3s) are only equal to themselves. We never move a point,
   * but can create multiple (uniques) at one location. We also change prev/next pointers to
   * edges and other corners but need to retain hashing behaviour. Therefore we revert to
   * the system hash.
   *
   * @param t1
   * @return
   */

  /**
   * We rely on the fact that we can shift the point's heights without changing
   * their locations in hashmaps.
   *
   * @return
   */
  def iterator = new CornerIterator(this)
  /**
   * Over all corners in the same loop as the one given
   */
  class CornerIterator(start: Corner) extends Iterator[Corner] {
    var n: Corner = null
    override def hasNext: Boolean = {
      if (start == null) false
      else if (n == null) true
      else n != start
    }
    override def next = {
      if (n == null) n = start
      val out = n
      n = n.nextC
      out
    }
  }
  override def toString = s"Corner(pos: ${asV3.toShortString}, nextLStart: ${nextL.start.asV3.toShortString}, prevLStart: ${prevL.start.asV3.toShortString}, nextC: ${nextC.asV3.toShortString}, prevC: {prevC.asV3.toShortString})"
}

