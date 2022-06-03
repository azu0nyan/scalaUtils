package utils.math.planar.algo

import utils.datastructures.CircullarOps
import utils.math._
import utils.math.planar.{PointIntersection, SegmentPlanar, TrianglePlanar, V2}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Idea from:
  * A Robust and Ecient Implementation of a Sweep Line Algorithm for the Straight Line Segment Intersection
  * Problem
  * Ulrike BartuschkaFachbereich Mathematik und Informatik, Universitat Hal le
  * */
object PolygonContains {


  sealed trait Intersection {
    def isParent: Boolean
  }
  //  case class SegmentEnd(y: Scalar, /*facingUp: Boolean,*/ isParent: Boolean) extends Intersection
  case class SegmentStart(y: Scalar, /* facingUp: Boolean,*/ isParent: Boolean) extends Intersection
  case class SegmentMiddle(y: Scalar, /*facingUp: Boolean,*/ isParent: Boolean) extends Intersection
  case class VerticalSegment(minY: Scalar, maxY: Scalar, isParent: Boolean) extends Intersection


  def contains(parent: Seq[Seq[V2]], child: Seq[Seq[V2]]): Boolean = {
    //helpers
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
    def less(v1: V2, v2: V2): Boolean = v1.x < v2.x || (v1.x == v2.x && v1.y < v2.y)
    def makeSegment(v1: V2, v2: V2, isParent: Boolean): SegmentPlanar =
      if (less(v1, v2)) SegmentPlanar(v1, v2) else SegmentPlanar(v2, v1)


    //data
    val parentSegments: Set[SegmentPlanar] = parent.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y, true) }.toSet
    val childSegments: Set[SegmentPlanar] = child.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y, false) }.toSet

    implicit val pointOrdering: Ordering[V2] = Ordering.fromLessThan(less)
    val xStructure = new mutable.PriorityQueue[V2]()
    val yStructure = new ArrayBuffer[SegmentPlanar]()
    val segmentQueue = new mutable.PriorityQueue[SegmentPlanar]()(
      Ordering.by[SegmentPlanar, V2](_.v1).orElse(Ordering.by(_.v2))
    )
    var contains = true


    //push segment start events to queue
    for (s@SegmentPlanar(v1, v2) <- childSegments ++ parentSegments) {
      xStructure += v1
      segmentQueue += s
    }

    var currentXIntersections: mutable.Buffer[Intersection] = mutable.Buffer()
    var currentX = xStructure.head.x

    //data ops
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
    //reverse ordering of segments in Y structure
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

    def checkContains(sweepPoint: V2): Boolean = {
      //      checkInside()
      true
    }

    while (xStructure.nonEmpty && contains) {
      //dequeue points and all it's copies
      val sweepPoint = xStructure.dequeue()
      while (xStructure.head ~= sweepPoint) xStructure.dequeue()

      //if we moved sweep line to next vertical bar
      if (currentX != sweepPoint.x) {
        contains = checkInside(currentXIntersections.toSeq)
        currentXIntersections.clear()
        currentX = sweepPoint.x
      }

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

          //add intersection events for all segments passing
          var alreadyAdded: Set[SegmentPlanar] = Set()
          for (i <- start to end; seg = yStructure(i) if !alreadyAdded.contains(seg)) { //todo handle multiple same segments
            val yOpt = seg.yFromX(currentX)
            alreadyAdded += seg
            if (yOpt.nonEmpty) {
              if (parentSegments.contains(seg)) currentXIntersections += SegmentMiddle(yOpt.get, true)
              if (childSegments.contains(seg)) currentXIntersections += SegmentMiddle(yOpt.get, false)
            }
          }


          //reverse all
          reverseY(start, newEnd)
        }
      }


      var alreadyAdded: Set[SegmentPlanar] = Set() //todo handle multiple same segments

      while (segmentQueue.nonEmpty && (segmentQueue.head.v1 ~= sweepPoint)) {
        //assumes (curSeg.length > 0)
        val curSeg = segmentQueue.dequeue()
        val insertionPoint = yStructure.indexWhere(ySeg => lessAt(sweepPoint, ySeg, curSeg))

        if (insertionPoint != -1) yStructure.insert(insertionPoint, curSeg)
        else yStructure += curSeg

        xStructure += curSeg.v2

        //add start events for segment
        if (!alreadyAdded.contains(curSeg)) {
          curSeg.yFromX(currentX) match {
            case Some(y) =>
              if (parentSegments.contains(curSeg)) currentXIntersections += SegmentStart(y, true)
              if (childSegments.contains(curSeg)) currentXIntersections += SegmentStart(y, false)
            case None =>
//              if (parentSegments.contains(curSeg)) currentXIntersections += VerticalSegment(curSeg.v1.y, curSeg.v2.y, true)
//              if (childSegments.contains(curSeg)) currentXIntersections += VerticalSegment(curSeg.v1.y, curSeg.v2.y, false)
          }
        }
      }

      val curStart = yStructure.indexWhere(_.contains(sweepPoint))
      val curEnd = yStructure.lastIndexWhere(_.contains(sweepPoint))
      if (curStart > 0) addIntersectionsIfNeeded(sweepPoint, yStructure(curStart - 1), yStructure(curStart))
      if (curEnd < yStructure.size - 1) addIntersectionsIfNeeded(sweepPoint, yStructure(curEnd), yStructure(curEnd + 1))
    }

    if (currentXIntersections.nonEmpty) {
      contains = checkInside(currentXIntersections.toSeq)
    }

    contains

  }

  def checkInside(intersectionsOrdered: Seq[Intersection]): Boolean = {
//    val (parent, child) = intersectionsOrdered.partition(_.isParent == true)
    case class Status(parent:Boolean = false, child: Boolean = false, curY: Scalar = Double.MinValue, curParentPower: Scalar = 0, curChildPower: Scalar = 0, failed: Boolean = false )


    def processStatusToNewYTransition(status: Status, intersection: Intersection):Status = {
      val flipParent = status.curParentPower %  2 != 0
      val flipChild = status.curChildPower %  2 != 0
      val newParent = if (flipParent) !status.parent else status.parent
      val newChild = if (flipChild) !status.child else status.child
      val newFailed = newChild && !newParent
      intersection match {
        case SegmentStart(y, isParent) => incCounter(Status(newParent, newChild, y, 0, 0, newFailed), intersection)
        case SegmentMiddle(y, isParent) => incCounter(Status(newParent, newChild, y, 0, 0, newFailed), intersection)
        case VerticalSegment(minY, maxY, isParent) => throw new Exception("Impossible")/*imposible*/
      }

    }

    def incCounter(status: Status, intersection: Intersection): Status = (status, intersection) match {
      case (s@status, SegmentStart(y, isParent)) if isParent => status.copy(curParentPower = status.curParentPower + 1)
      case (s@status, SegmentStart(y, isParent)) if !isParent => status.copy(curChildPower = status.curChildPower + 1)
      case (s@status, SegmentMiddle(y, isParent)) if isParent => status.copy(curParentPower = status.curParentPower + 1)
      case (s@status, SegmentMiddle(y, isParent)) if !isParent => status.copy(curChildPower = status.curChildPower + 1)
      case (s@status, VerticalSegment(minY, maxY, isParent)) => /*Ignore vertical intersections*/
        status
    }

    def processEvent(status:Status, intersection: Intersection): Status = intersection match {
      case  VerticalSegment(_, _, _) => /*Ignore vertical intersections*/ status
      case  SegmentStart(y, _) if status.curY ~= y => incCounter(status, intersection)
      case  SegmentMiddle(y, _) if status.curY ~= y => incCounter(status, intersection)
      case  SegmentStart(y, _) => processStatusToNewYTransition(status, intersection)
      case  SegmentMiddle(y, _) => processStatusToNewYTransition(status, intersection)

    }


    var cur = intersectionsOrdered
    var curStatus = Status()
    while(!curStatus.failed && cur.nonEmpty) {
      curStatus = processEvent(curStatus, cur.head)
      cur = cur.tail
    }
    curStatus.failed

//    var parentCover: Seq[(Scalar, Scalar)] = Seq()
//    var (curMin, curMax) = parent.head match {
//      case SegmentStart(y, isParent) => (y, y)
//      case SegmentMiddle(y, isParent) => (y, y)
//      case VerticalSegment(minY, maxY, isParent) => (minY, maxY)
//    }
//    var countAt = 1
//    for (seg <- parent) {
//      seg match {
//        case SegmentStart(y, isParent) if y == curMax => countAt += 1
//        case SegmentStart(y, isParent) if y == curMax => countAt += 1
//        case SegmentMiddle(y, isParent) => ???
//        case VerticalSegment(minY, maxY, _) if minY > curMax => //segment on top
//          parentCover = parentCover :+ (curMin, curMax)
//          curMin = minY
//          curMax = maxY
//        case VerticalSegment(minY, maxY, _) if maxY > curMax
//      }
//    }
//    true
  }
}
