package utils.math.planar.algo.polygonTriangulation

import utils.datastructures.CircullarOps
import utils.datastructures.containers.BinHeap

import utils.math.planar.{PointIntersection, SegmentIntersection, SegmentPlanar, TrianglePlanar, V2}
import utils.sugar.SeqOps

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Idea from:
  * A Robust and Ecient Implementation of a Sweep Line Algorithm for the Straight Line Segment Intersection
  * Problem
  * Ulrike BartuschkaFachbereich Mathematik und Informatik, Universitat Hal le
  * */
object PolygonContains {

  def contains(parent: Seq[Seq[V2]], child: Seq[Seq[V2]]): Boolean = {

    def less(v1: V2, v2: V2): Boolean = v1.x < v2.x || (v1.x == v2.x && v1.y < v2.y)
    def makeSegment(v1: V2, v2: V2, isParent:Boolean): SegmentPlanar =
      if (less(v1, v2)) SegmentPlanar(v1, v2) else SegmentPlanar(v2, v1)



    val parentSegments: Seq[SegmentPlanar] = parent.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y) }
    val childSegments: Seq[SegmentPlanar] = child.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y) }


    implicit val pointOrdering: Ordering[V2] = Ordering.fromLessThan(less)
    val xStructure = new mutable.PriorityQueue[V2]()
    val yStructure = new ArrayBuffer[SegmentPlanar]()
    val segmentQueue = new mutable.PriorityQueue[SegmentPlanar]()(
      Ordering.by[SegmentPlanar, V2](_.v1).orElse(Ordering.by(_.v2))
    )
    var contains = true

    for (s@SegmentPlanar(v1, v2) <- childSegments ++ parentSegments) {
      xStructure += v1
      segmentQueue += s
    }

    def orientation(v1: V2, v2: V2, v3: V2): Boolean = {
      val tri = TrianglePlanar(v1, v2, v3)
      (tri.nonDegenerate && tri.ccw) || (tri.degenerate && (v2 - v1).length < (v3 - v1).length)
    }

    /** assumes that both segments intersects L-structure */
    def lessAt(at: V2, s1: SegmentPlanar, s2: SegmentPlanar): Boolean = {
      val s1y = s1.yFromX(at.x)
      val s2y = s2.yFromX(at.x)
      (s1y, s2y) match {
        case (Some(y1), Some(y2)) => y1 < y2 || (y1 == y2 && orientation(at, s1.v2, s2.v2))
        case (Some(y1), None) => y1 <= at.y
        case (None, Some(y2)) => y2 > at.y
        case (None, None) => s1.v2.y < s2.v2.y //????
      }
    }

    def detectCollinearSubseq(from: Int): Int = {
      var res = from + 1
      while (res < yStructure.size && yStructure(res).body.collinear(yStructure(from).body))
        res = res + 1

      res - 1
    }

    def swap(e1: Int, e2: Int): Unit = {
      val t = yStructure(e1)
      yStructure(e1) = yStructure(e2)
      yStructure(e2) = t
    }

    def reverseY(from: Int, to: Int): Unit = {
      val l = to - from
      for (i <- 0 to l / 2)
        swap(from + i, to - i)
    }

    def addIntersectionsIfNeeded(sweepPoint: V2, s1: SegmentPlanar, s2: SegmentPlanar): Unit = {
      s1.intersection(s2) match {
        case Some(PointIntersection(p)) if less(sweepPoint, p) => xStructure += p
        case _ =>
      }
    }

    def checkContains(sweepPoint: V2):Boolean = {

    }

    while (xStructure.nonEmpty && contains) {
      val sweepPoint = xStructure.dequeue()
      while (xStructure.head ~= sweepPoint) xStructure.dequeue()
      //detect yStructure subSeq containing sweepPoint
      val start = yStructure.indexWhere(_.contains(sweepPoint))
      val end = yStructure.lastIndexWhere(_.contains(sweepPoint))

      //remove segments ending in sweepPoint and reverse order of segments passing sweepPoint
      if (start != -1 && end != -1) {
        //remove segments ending in sweepPoint
        var iter = start
        for (_ <- start to end)
          if (yStructure(iter).v2 ~= sweepPoint) yStructure.remove(iter)
          else iter += 1
        //iter becomes new end
        val newEnd = iter - 1
        if (start <= newEnd) {
          //reverse collinear segment bulks to prevent its reverse
          var i = start
          while (i < iter) {
            val collinearEnd = detectCollinearSubseq(i)
            if (i != collinearEnd) {
              reverseY(i, collinearEnd)
              i = collinearEnd + 1
            } else {
              i = i + 1
            }
          }
          //reverse all
          reverseY(start, newEnd)
        }
      }

      while (segmentQueue.nonEmpty && (segmentQueue.head.v1 ~= sweepPoint)) {
        //assumes (curSeg.length > 0)
        val curSeg = segmentQueue.dequeue()
        val insertionPoint = yStructure.indexWhere(ySeg => lessAt(sweepPoint, ySeg, curSeg))

        if (insertionPoint != -1) yStructure.insert(insertionPoint, curSeg)
        else yStructure += curSeg

        xStructure += curSeg.v2
      }

      val curStart = yStructure.indexWhere(_.contains(sweepPoint))
      val curEnd = yStructure.lastIndexWhere(_.contains(sweepPoint))
      if (curStart > 0) addIntersectionsIfNeeded(sweepPoint, yStructure(curStart - 1), yStructure(curStart))
      if (curEnd < yStructure.size - 1) addIntersectionsIfNeeded(sweepPoint, yStructure(curEnd), yStructure(curEnd + 1))


      contains = checkContains(sweepPoint)
    }

    contains

  }

  /*def contains(parent: Seq[Seq[V2]], child: Seq[Seq[V2]]): Boolean = {
    def less(v1: V2, v2: V2): Boolean = v1.x < v2.x || (v1.x == v2.x && v1.y < v2.y)
    def makeSegment(v1: V2, v2: V2): SegmentPlanar =
      if (less(v1, v2)) SegmentPlanar(v1, v2) else SegmentPlanar(v2, v1)

    sealed trait Event
    case class SegmentStart(s: SegmentPlanar) extends Event
    case class SegmentEnd(s: SegmentPlanar) extends Event
    case class SegmentIntersects(s1: SegmentPlanar, s2: SegmentPlanar) extends Event

    def lessE(e1: Event, e2: Event): Boolean = true
    implicit val o: Ordering[Event] = Ordering.fromLessThan(lessE)

    case class StatusData(segment: SegmentPlanar, isParent: Boolean)
    //--ROUTINE--
    val queue = new BinHeap[Event]()
    var status: Vector[SegmentPlanar] = Vector()
    val parentSegments: Seq[SegmentPlanar] = parent.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y) }
    val childSegments: Seq[SegmentPlanar] = child.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y) }
    var intersectionFound: Boolean = false

    for (s <- parentSegments ++ childSegments) {
      queue += SegmentStart(s)
      queue += SegmentEnd(s)
    }

    def fireEventsIfNeeded(at: V2, s1: SegmentPlanar, s2: SegmentPlanar): Unit = {
      s1.intersection(s2) match {
        case Some(PointIntersection(p)) if (less(at, p)) =>
          queue.add(SegmentIntersects(s1, s2)) //todo if intersects not by ends stop
//          intersectionFound = true
        case Some(SegmentIntersection(s)) =>
        //          queue.add(SegmentIntersects(s1, s2))
        case _ =>
      }
    }

    def addToStatus(seg: SegmentPlanar): Unit = {
      val i = status.indexWhere(inStatus => inStatus.yFromX(seg.v1.x) match {
        case Some(y) => y > seg.v1.y
        case None => inStatus.v2.y > seg.v2.y // vertical segments compared by higher end
      })
      val (f, s) = status.splitAt(i)
      status = f ++ Vector(seg) ++ s
      if (i > 0) fireEventsIfNeeded(seg.v1, status(i - 1), status(i))
      if (i < status.size - 1) fireEventsIfNeeded(seg.v1, status(i), status(i + 1))
    }

    def removeFromStatus(seg: SegmentPlanar): Unit = {

    }

    def processEvent(e: Event): Unit = e match {
      case SegmentStart(s) =>
        addToStatus(s)
      case SegmentEnd(s) => ???
      case SegmentIntersects(s1, s2) => ???
    }

    while (!intersectionFound && queue.nonEmpty) {
      processEvent(queue.poll())
    }

    !intersectionFound
  }*/
}
