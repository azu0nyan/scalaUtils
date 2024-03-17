package utils.math.planar.algo.straightSkeleton

import utils.math.Scalar
import utils.math.planar.{AngleCCWPlanar, AngleOps, LinePlanar, V2}

import scala.collection.mutable


object StraightSkeleton {

  /** Vertex */
  class V(
           val position: V2,
           var bisector: LinePlanar,
           var prev: V = null,
           var next: V = null,
           var isActive: Boolean = true,
           val eventType: EventType = NoEvent,
         )
  /** Intersection */
  class I(
           val pos: V2,
           val a: V,
           val b: V,
           val dist: Scalar,
         )

  sealed trait EventType
  case object NoEvent extends EventType
  case class EdgeEvent(a: V, b: V) extends EventType
  case object SplitEvent extends EventType

  def edgeBisectorDir(as: V, ae: V, bs: V, be: V): V2 = {
    val a = ae.position - as.position
    val b = be.position - bs.position
    val dir = AngleOps.ccwBisector(a, b) //todo check
    dir
  }

  def prepareVertexData(vs: IndexedSeq[V2]): IndexedSeq[V] = {
    if (vs.length < 2) throw new IllegalArgumentException("must have at least 2 vertices")

    @inline def prev(id: Int) = if (id == 0) vs.length - 1 else id - 1
    @inline def next(id: Int) = if (id == vs.length - 1) 0 else id + 1

    val result = Array.tabulate(vs.size) { i =>
      val bisector = AngleCCWPlanar(vs(prev(i)), vs(i), vs(next(i))).bisector
      new V(vs(i), bisector)
    }
    for (i <- result.indices) {
      result(i).prev = result(prev(i))
      result(i).next = result(next(i))
    }
    result
  }

  def prepareIntersections(vs: IndexedSeq[V]): mutable.PriorityQueue[I] = {
    val result = new mutable.PriorityQueue[I]()(Ordering.by[I, Double](-_.dist))
    for (v <- vs) {

      val bim1Intersection = v.prev.bisector.intersection(v.bisector)
      val bip1Intersection = v.next.bisector.intersection(v.bisector)

      def edgeA = LinePlanar.fromTwoPoints(v.prev.position, v.position)
      def edgeB = LinePlanar.fromTwoPoints(v.position, v.next.position)

      def distToEdges(p: V2) = math.min(edgeA.distanceTo(p), edgeB.distanceTo(p))

      (bim1Intersection, bip1Intersection) match {
        case (Some(bim1Intersection), Some(bip1Intersection))
          if bim1Intersection.distance(v.position) < bip1Intersection.distance(v.position) =>
          result += new I(bim1Intersection, v.prev, v, distToEdges(bim1Intersection))
        case (Some(_), Some(bip1Intersection)) =>
          result += new I(bip1Intersection, v, v.next, distToEdges(bip1Intersection))
        case (Some(bim1Intersection), None) =>
          result += new I(bim1Intersection, v.prev, v, distToEdges(bim1Intersection))
        case (None, Some(bip1Intersection)) =>
          result += new I(bip1Intersection, v, v.next, distToEdges(bip1Intersection))
        case _ =>
      }
    }
    result
  }



  def buildSkeleton(vs: Seq[V2]) = {
    val vsData = prepareVertexData(vs.toIndexedSeq)
    val queue = prepareIntersections(vsData)
    var vertices = vsData.size
    vsData.foreach(emmitVertex)
    vsData.foreach(v => emmitEdge(v, v.next))

    def threeVerticesLeft: Boolean = vertices == 3

    def emmitVertex(v: V) = {
      println(s"new vertex $v")
      vertices += 1
    }


    def emmitEdge(a: V, b: V): Unit = {
      println(s"new edge $a $b")

    }

    def toCycle(v: V): mutable.Buffer[V] = {
      val res = mutable.Buffer(v)
      var cur = v.next
      while (cur != v) {
        res += cur
        cur = cur.next
      }
      res
    }

    var finished = false
    def processEvent(i: I) = {
      if (threeVerticesLeft) { //emmit edges IV_a IV_b IV_c from center of triangle to vertices
        val v = new V(i.pos, i.a.bisector) //last step of algo, bisector serves as placeholder
        emmitVertex(v)
        toCycle(i.a)
          .foreach(abc => emmitEdge(v, abc))
        finished = true
      } else { //emmit edges IV_a IV_b

        val bisectorDir = edgeBisectorDir(i.a.prev, i.a, i.b, i.b.next)
        val bisector = LinePlanar(i.pos, bisectorDir)

        val distToEi = LinePlanar.fromTwoPoints(i.a.position, i.b.position).distanceTo(i.pos)
        val v = new V(i.pos, bisector, prev = i.a.prev, next = i.b.next)
        emmitEdge(i.a, i.b)
      }
    }


    while (queue.nonEmpty && !finished) {
      val i = queue.dequeue()
      if (i.a.isActive && i.b.isActive) {
        processEvent(i)
      }
    }
  }


}
