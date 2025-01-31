package utils.math.planar.algo

import utils.Logging
import utils.datastructures.CircullarOps
import utils.math.*
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
      case (Some(y1), Some(y2)) => y1 ~< y2 || ((y1 ~= y2) && orientation(at, s1.v2, s2.v2))
      case (Some(y1), None) => y1 <= at.y
      case (None, Some(y2)) => y2 > at.y
      case (None, None) => s1.v2.y < s2.v2.y //????
    }
  }

 case class SegmentIsParent(segment:SegmentPlanar, isParent:Boolean)
  def contains(parent: Seq[Seq[V2]], child: Seq[Seq[V2]], forbidParentBorderCrossing: Boolean = false): Boolean = {

    def less(v1: V2, v2: V2): Boolean = v1.x < v2.x || (v1.x == v2.x && v1.y < v2.y)
    def makeSegment(v1: V2, v2: V2, isParent: Boolean): SegmentIsParent =
      SegmentIsParent(if (less(v1, v2)) SegmentPlanar(v1, v2) else SegmentPlanar(v2, v1), isParent)


    //data
//    val parentSegs: Set[SegmentIsParent] = parent.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y, true) }.toSet
//    val childSegs: Set[SegmentIsParent] = child.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y, false) }.toSet

//    implicit val pointOrdering: Ordering[V2] = Ordering.fromLessThan(less)
    val v2Ordering = Ordering.by[V2, Scalar](-_.x).orElseBy[Scalar](-_.y)
    val xStructure = new mutable.PriorityQueue[V2]()(v2Ordering)

    val yStructure = new ArrayBuffer[SegmentIsParent]()
    val segmentQueue = new mutable.PriorityQueue[SegmentIsParent]()(
      Ordering.by[SegmentIsParent, V2](_.segment.v1)(v2Ordering).orElseBy[V2](_.segment.v2)(v2Ordering)
    )
    var contains: Boolean = true


    //push segment start events to queue
    for(s <- parent.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y, true) }){
      xStructure += s.segment.v1
      segmentQueue += s
    }
    for(s <- child.flatMap(CircullarOps.toCyclicPairs).map { case (x, y) => makeSegment(x, y, false) }){
      xStructure += s.segment.v1
      segmentQueue += s
    }
//    for (s@SegmentPlanar(v1, v2) <- childSegments ++ parentSegments) {
//      xStructure += v1
//      segmentQueue += s
//    }


    var currentX = xStructure.head.x

    //data ops
    def detectCollinearSubseq(from: Int): Int = {
      var res = from + 1
      while (res < yStructure.size && yStructure(res).segment.onSameLine(yStructure(from).segment))
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

    Logging.logger.info(s"Init finished")
//    Logging.logger.info(s"Parent segments: ${parentSegments.map{case SegmentPlanar(v1, v2) => s"(${v1.toShortString}->${v2.toShortString})"}.mkString(" | ")}")
//    Logging.logger.info(s"Child segments: ${childSegments.map{case SegmentPlanar(v1, v2) => s"(${v1.toShortString}->${v2.toShortString})"}.mkString(" | ")}  ")

    def logStats(): Unit = {
      Logging.logger.info(s"currentX: ${currentX}")
      Logging.logger.info(s"xStructure: ${xStructure.clone().dequeueAll[V2].map(_.toShortString)} ")
      Logging.logger.info(s"yStructure: ${yStructure.toSeq.map { case SegmentIsParent(SegmentPlanar(v1, v2), isParent) =>
        s"(${v1.toShortString} -> ${v2.toShortString} ${if(isParent) "PARENT" else "CHILD"})"}.mkString(" | ")}")
      Logging.logger.info(s"segmentQueue: ${segmentQueue.clone().dequeueAll[SegmentIsParent]
        .map { case SegmentIsParent(SegmentPlanar(v1, v2), isParent) =>
          s"(${v1.toShortString} -> ${v2.toShortString} ${if(isParent) "PARENT" else "CHILD"})" }.mkString(" | ")}")
    }
    logStats()
    while (xStructure.nonEmpty && contains) {
      //dequeue points and all it's copies
      val sweepPoint = xStructure.dequeue()
      Logging.logger.info(s"Dequeued new current sweep point: $sweepPoint")
      while (xStructure.nonEmpty && (xStructure.head ~= sweepPoint)) {
        Logging.logger.info(s"Dequeued copy of sweep point: ${xStructure.head}")
        xStructure.dequeue()
      }

      //if we moved sweep line to next vertical bar
      if (currentX != sweepPoint.x) {
        Logging.logger.info(s"New currentX found: ${sweepPoint.x}")
        Logging.logger.info(s"Running inside check for: ${currentX} ${yStructure.flatMap(s => s.segment.yFromX(currentX))}")
        contains &= checkInside(yStructure.flatMap(s => s.segment.yFromX(currentX) match {
          case Some(y) => Seq(SegmentMiddle(y, s.isParent))
          case _ => Seq()
        }).toSeq)
        currentX = sweepPoint.x
      }

      //detect yStructure subSeq containing sweepPoint
      val start = yStructure.indexWhere(_.segment.contains(sweepPoint))
      val end = yStructure.lastIndexWhere(_.segment.contains(sweepPoint))

      //remove segments ending in sweepPoint and reverse order of segments passing sweepPoint
      if (start != -1 && end != -1) {
        //remove segments ending in sweepPoint
        var iter = start
        for (_ <- start to end)
          if (yStructure(iter).segment.v2 ~= sweepPoint) {
            Logging.logger.info(s"Removing ending segment: ${yStructure.remove(iter)}")
          }
          else iter += 1
        //iter becomes new end
        val newEnd = iter - 1
        if (start <= newEnd) {
          //reverse collinear segment bulks to prevent its reverse
          var i = start

          //looking for parent to child noncollinear intersections
          if(forbidParentBorderCrossing) {
            var parentVisited = false
            var childVisited = false
            while (i < iter) {
              var pInt = false
              var cInt = false
              val collinearEnd = detectCollinearSubseq(i)
              if (i != collinearEnd) {
                reverseY(i, collinearEnd)
                for(j <- i to collinearEnd){
                  pInt |= yStructure(j).isParent
                  cInt |= !yStructure(j).isParent
                }
                i = collinearEnd + 1
              } else {
                pInt |= yStructure(i).isParent
                cInt |= !yStructure(i).isParent
                i = i + 1
              }
              if(pInt && !cInt){
                if(childVisited) contains = false
                else parentVisited = true
              } else if(!pInt &&cInt){
                if(parentVisited) contains = false
                else childVisited = true
              }
            }

          } else {
            while (i < iter) {
              val collinearEnd = detectCollinearSubseq(i)
              if (i != collinearEnd) {
                reverseY(i, collinearEnd)
                i = collinearEnd + 1
              } else {
                i = i + 1
              }
            }
          }

          //add intersection events for all segments passing
          /*var alreadyAdded: Set[SegmentPlanar] = Set()
          for (i <- start to end; seg = yStructure(i) if !alreadyAdded.contains(seg)) { //todo handle multiple same segments
            val yOpt = seg.yFromX(currentX)
            alreadyAdded += seg
            if (yOpt.nonEmpty) {
              if (parentSegments.contains(seg)) currentXIntersections += SegmentMiddle(yOpt.get, true)
              if (childSegments.contains(seg)) currentXIntersections += SegmentMiddle(yOpt.get, false)
            }
          }*/


          //reverse all
          reverseY(start, newEnd)
        }
      }


     // var alreadyAdded: Set[SegmentPlanar] = Set() //todo handle multiple same segments

      while (segmentQueue.nonEmpty && (segmentQueue.head.segment.v1 ~= sweepPoint)) {
        //assumes (curSeg.length > 0)
        val curSeg = segmentQueue.dequeue()
        val insertionPoint = yStructure.lastIndexWhere(ySeg => lessAt(sweepPoint, ySeg.segment, curSeg.segment))
        Logging.logger.info(s"Dequeue starting segment : $curSeg inserting at ${if(insertionPoint == -1) "BEGIN" else (insertionPoint + 1).toString}")

        if (insertionPoint != -1) yStructure.insert(insertionPoint + 1, curSeg)
        else yStructure.prepend(curSeg)

        xStructure += curSeg.segment.v2

        //add start events for segment
        /*if (!alreadyAdded.contains(curSeg)) {
          curSeg.yFromX(currentX) match {
            case Some(y) =>
              if (parentSegments.contains(curSeg)) currentXIntersections += SegmentStart(y, true)
              if (childSegments.contains(curSeg)) currentXIntersections += SegmentStart(y, false)
            case None =>
//              if (parentSegments.contains(curSeg)) currentXIntersections += VerticalSegment(curSeg.v1.y, curSeg.v2.y, true)
//              if (childSegments.contains(curSeg)) currentXIntersections += VerticalSegment(curSeg.v1.y, curSeg.v2.y, false)
          }
        }*/
      }

      val curStart = yStructure.indexWhere(_.segment.contains(sweepPoint))
      val curEnd = yStructure.lastIndexWhere(_.segment.contains(sweepPoint))
      if (curStart > 0) addIntersectionsIfNeeded(sweepPoint, yStructure(curStart - 1).segment, yStructure(curStart).segment)
      if (curEnd>= 0 && curEnd < yStructure.size - 1) addIntersectionsIfNeeded(sweepPoint, yStructure(curEnd).segment, yStructure(curEnd + 1).segment)

      logStats()
    }

    if (contains && yStructure.nonEmpty) {
      Logging.logger.info(s"WTFFF")
      contains &= checkInside(yStructure.flatMap(s => s.segment.yFromX(currentX) match {
        case Some(y) => Seq(SegmentMiddle(y, s.isParent))
        case _ => Seq()
      }).toSeq)
    }

    contains

  }

  def checkInside(intersectionsOrdered: Seq[Intersection]): Boolean = {

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
      case _ => throw  new Exception("")
    }

    def processEvent(status:Status, intersection: Intersection): Status = intersection match {
      case  VerticalSegment(_, _, _) => /*Ignore vertical intersections*/ status
      case  SegmentStart(y, _) if status.curY ~= y => incCounter(status, intersection)
      case  SegmentMiddle(y, _) if status.curY ~= y => incCounter(status, intersection)
      case  SegmentStart(y, _) => processStatusToNewYTransition(status, intersection)
      case  SegmentMiddle(y, _) => processStatusToNewYTransition(status, intersection)

    }

    Logging.logger.info(s"Checking inside for intersections: $intersectionsOrdered")
    var cur = intersectionsOrdered
    var curStatus = Status()
    Logging.logger.info(s"Initial status: $curStatus")
    while(!curStatus.failed && cur.nonEmpty) {
      Logging.logger.info(s"Checking inside for intersections: ${cur.head}")
      curStatus = processEvent(curStatus, cur.head)
      Logging.logger.info(s"Current status: $curStatus")
      cur = cur.tail
    }
    Logging.logger.info(s"contains: ${!curStatus.failed}")
    !curStatus.failed

  }
}
