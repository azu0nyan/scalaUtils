package utils.math.planar

import utils.datastructures.containers.ThreadedAVLTree.ThreadedAVLTree
import utils.math._
import utils.sugar._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object SegmentsIntersection {


  /**
    * @param point Point corresponding to event
    * @param upper Set of segments whose "u"pper endpoint is "p"
    */
  case class Event(point: V2, upper: mutable.HashSet[SegmentPlanar])


  /* implicit val SegmentsOrdering: Ordering[SegmentPlanar] = Ordering.by(s =>
     if (s.yProjSigned == 0) Float.PositiveInfinity
     else if (s.xProjSigned == 0) 0 else (-s.yProjSigned / s.xProjSigned)
   )*/

  /** works correct only if both segments intersected by sweep line */
  class SegmentsOrdering() extends Ordering[SegmentPlanar] {
    var sweepPoint: V2 = V2.ZERO

    override def compare(first: SegmentPlanar, second: SegmentPlanar): Int = {
      val firstX = first.xFromY(sweepPoint.y)
      val secondX = second.xFromY(sweepPoint.y)
      if (firstX.isDefined && secondX.isDefined) {
        val firstXVal = firstX.get
        val secondXVal = secondX.get

        if (firstXVal ~= secondXVal) {
          val firstK = first.kY.get
          val secondK = second.kY.get
          if (firstK ~< secondK) return -1
          else if (firstK ~> secondK) return 1
          else {
            //segments overlapping, since segments flipped v1 - top
            //topmost lesser
            if (first.v1.y ~> second.v1.y) return -1
            else if (first.v1.y ~< second.v1.y) return 1
            else {
              //shorter lesser
              return utils.math.compare(first.lengthSquared, second.lengthSquared)
            }
          }
        } else if (firstXVal ~< secondXVal) return -1 //leftmost lesser
        else /*if (firstXVal ~> secondXVal)*/ return 1
      } else if (firstX.isDefined && secondX.isEmpty) { //second parallel to ox
        /*if (first.intersects(second)) return -1 //todo change use sweepPoint.x to determine order
        else if (firstX.get < second.v1.x) return -1
        else return 1*/
        if (firstX.get ~<= sweepPoint.x) -1 else 1
      } else if (firstX.isEmpty && secondX.isDefined) { //first parallel to ox
        /*if (first.intersects(second)) return 1
        else if (secondX.get < first.v1.x) return 1
        else return -1*/
        if (secondX.get ~<= sweepPoint.x) 1 else -1
      } else { //both parallel to ox
        if (first.intersects(second)) {
          if (first.contains(sweepPoint) && second.contains(sweepPoint)) {
            if (first.v1.x < second.v1.x) return -1
            else if (first.v1.x > second.v1.x) return 1
            else return utils.math.compare(first.lengthSquared, second.lengthSquared)
          } else if (first.contains(sweepPoint)) {
            if (second.v2.x < sweepPoint.x) return 1 /*second leftmost*/ else -1 /*first leftmost*/
          } else if (second.contains(sweepPoint)) {
            if (first.v2.x < sweepPoint.x) return -1 else 1
          } else { //noone contains
            if (first.v1.x < second.v2.x) return -1
            else if (first.v1.x > second.v2.x) return 1
            else return utils.math.compare(first.lengthSquared, second.lengthSquared)
          }
        } else {
          if (first.v1.x < second.v1.x) -1 else 1
        }
      }
    }
  }

  implicit val pointOrdering: Ordering[V2] = (f: V2, s: V2) =>
    if (f.y ~= s.y) {
      if (f.x ~= s.x) 0
      else {
        if (f.x < s.x) -1 else 1
      }
    } else if (f.y < s.y) {
      -1
    } else 1

  /*  if (f.y < s.y)
      -1
    else if (f.y == s.y) {
      if (f.x < s.x)
        -1
      else if (f.x == s.x)
        0
      else
        1
    } else
      1
*/

  //def lt(f: V2, s: V2): Boolean = PointOrdering.lt(f, s)

  type LRNeighbouredSegments = Option[(SegmentPlanar, SegmentPlanar)]
  type LeftAndRightNeighbours = (LRNeighbouredSegments, LRNeighbouredSegments)

  implicit class LRNeighboursSugar(val lrb: LeftAndRightNeighbours) extends AnyVal {
    def leftPair: LRNeighbouredSegments = lrb._1

    def rightPair: LRNeighbouredSegments = lrb._2
  }


  trait Status {
    /** removes lower endpoint or interior points, upper endpoint shouldn't be added at the call time
      * all segments in status intersected by sweepLine
      * */
    def removeSegmentsContainsPoint(p: V2): collection.Iterable[SegmentPlanar]

    /** closest to p segment from left side and leftmost segment containing p,
      * rightmost segment containing p and closest to p segment from right side
      * None if no neighbour */
    def leftAndRightSegmentAndNeighbourAt(p: V2): LeftAndRightNeighbours

    def add(s: collection.Iterable[SegmentPlanar], sweepPoint: V2): Unit = {
      s.foreach(add(_, sweepPoint))
    }

    def add(s: SegmentPlanar, sweepPoint: V2): Unit

    /** segments in status shouldn't contain p, returns closest segments to p from left and right  */
    def lrNeighboursForEmptyPoint(p: V2): LRNeighbouredSegments

    def values: collection.Iterable[SegmentPlanar]
  }

  class StatusListImpl() extends Status {
    var storage: List[SegmentPlanar] = List()

    /** removes lower endpoint or interior points, upper endpoint shouldn't be added at the call time
      * all segments in status intersected by sweepLine
      * */
    override def removeSegmentsContainsPoint(p: V2): Iterable[SegmentPlanar] = {
      val split = storage.partition(s => s.contains(p))
      storage = split._2
      return split._1
    }

    val segmentsOrdering: SegmentsOrdering = new SegmentsOrdering

    /** closest to p segment from left side and leftmost segment containing p,
      * rightmost segment containing p and closest to p segment from right side
      * None if no neighbour */
    override def leftAndRightSegmentAndNeighbourAt(p: V2): (LRNeighbouredSegments, LRNeighbouredSegments) = {
      val left = storage.indexWhere(s => s.contains(p))
      val right = storage.indexWhere(s => !s.contains(p), left)
      (
        if (left > 0) Some((storage(left - 1), storage(left))) else None,
        if (right > 0) Some((storage(right - 1), storage(right))) else None //???
      )
    }

    override def add(s: SegmentPlanar, sweepPoint: V2): Unit = {
      segmentsOrdering.sweepPoint = sweepPoint //+ V2(0, SMALL_NUMBER)
      val i = storage.indexWhere(se => segmentsOrdering.lt(s, se))
      storage = storage.insert(if (i >= 0) i else storage.length, s)
    }

    /** segments in status shouldn't contain p, returns closest segments to p from left and right  */
    override def lrNeighboursForEmptyPoint(p: V2): LRNeighbouredSegments = {
      val i = storage.indexWhere(s => s.xFromY(p.y).getOrElse(p.x + 1) >= p.x)
      if (i > 0) { //-1 all lesser, no pair. 0 - first greater
        return Some((storage(i - 1), storage(i)))
      } else {
        return None
      }

    }

    override def values: Iterable[SegmentPlanar] = storage
  }

  class StatusAVlImpl() extends Status {
    val segmentsOrdering: SegmentsOrdering = new SegmentsOrdering
    val storage: ThreadedAVLTree[SegmentPlanar] = new ThreadedAVLTree[SegmentPlanar]()(segmentsOrdering)

    def descentPredicate(p: V2): SegmentPlanar => Int = { s =>
      s.xFromY(p.y) match {
        case Some(x) =>
          if (x ~= p.x) 0
          else if (x ~> p.x) -1
          else /*if (x ~< p.x)*/ 1

        case None =>
          if (s.v1.x ~= p.x) 0 // contains point since sweep line intersects horizontal lines
          else if (s.v1.x ~> p.x) -1 //point at left
          else /*if (s.v2.x ~< p.x)*/ 1 //point at right
      }
    }

    /** removes lower endpoint or interior points, upper endpoint shouldn't be added at the call time
      * all segments in status intersected by sweepLine
      * */
    def removeSegmentsContainsPoint(p: V2): collection.Iterable[SegmentPlanar] = {
      segmentsOrdering.sweepPoint = p //- V2(0f, SMALL_NUMBER)

      val pred = descentPredicate(p)
      var removed: Option[SegmentPlanar] = None
      var res = new ArrayBuffer[SegmentPlanar]()
      do {
        removed = storage.removeByPredicate(pred)
        removed.foreach(s => res += s)
      } while (removed.isDefined)
      return res
    }

    /** closest to p segment from left side and leftmost segment containing p,
      * rightmost segment containing p and closest to p segment from right side
      * None if no neighbour */
    def leftAndRightSegmentAndNeighbourAt(p: V2): LeftAndRightNeighbours = {
      storage.findByPredicate(descentPredicate(p)) match {
        case Some(node) =>
          ( {
            var current = node
            var prev = current.prev
            while (prev.isDefined && prev.get.value.contains(p)) {
              current = prev.get
              prev = prev.get.prev
            }
            prev.map(p => (p.value, current.value))
          }, {
            var current = node
            var next = current.next
            while (next.isDefined && next.get.value.contains(p)) {
              current = next.get
              next = next.get.next
            }
            next.map(n => (current.value, n.value))
          })
        case None => (None, None)
      }
    }

    //  def remove(s: collection.Iterable[SegmentPlanar], sweepPoint: V2)

    override def add(s: collection.Iterable[SegmentPlanar], sweepPoint: V2): Unit = {
      segmentsOrdering.sweepPoint = sweepPoint + V2(0, SMALL_NUMBER)
      s.foreach(storage.add)
    }

    def add(s: SegmentPlanar, sweepPoint: V2): Unit = {
      segmentsOrdering.sweepPoint = sweepPoint
      storage.add(s)
    }

    /** segments in status shouldn't contain p, returns closest segments to p from left and right  */
    def lrNeighboursForEmptyPoint(p: V2): LRNeighbouredSegments =
      storage.closestNodeByPredicate(descentPredicate(p)).flatMap { node =>
        node.value.xFromY(p.y) match {
          case Some(x) => if (x < p.x) { //take next
            node.next.map(n => (node.value, n.value))
          } else if (x > p.x) { //take prev
            node.prev.map(p => (node.value, p.value))
          } else {
            throw new Exception("segment containing point found wtf")
          }
          case None => None
        }
      }

    override def values: Iterable[SegmentPlanar] = storage.values.iterator.to(Iterable)
  }

  //  implicit val eventsOrdering: Ordering[Event] = Ordering.by(_.point)
  //implicit val eventsOrdering: Ordering[(V2, mutable.HashSet[SegmentPlanar])] = Ordering.by(_._1)

  type SegmentsIntersections = (V2, Set[SegmentPlanar])

  /**
    * find intersection between segments using Bentley-Ottoman algorithm
    *
    * @param segments
    * @param statusAndQueueCallback    report status for debugging
    * @param beforeHandleEventCallback report event for debugging
    * @param intersectionCallback      report intersection for debugging
    * @return
    */
  def findIntersection(
                        segments: Seq[SegmentPlanar],
                        statusAndQueueCallback: Option[(Status, java.util.TreeMap[V2, mutable.HashSet[SegmentPlanar]]) => Unit] = None,
                        beforeHandleEventCallback: Option[(V2, Set[SegmentPlanar]) => Unit] = None,
                        afterRemoveHandleEventCallback: Option[(V2, Set[SegmentPlanar], Set[SegmentPlanar], Set[SegmentPlanar]) => Unit] = None,
                        afterAddHandleEventCallback: Option[(V2, Set[SegmentPlanar], Set[SegmentPlanar]) => Unit] = None,
                        afterNewEventsHandleEventCallback: Option[V2 => Unit] = None,
                        intersectionCallback: Option[(V2, Set[SegmentPlanar]) => Unit] = None,
                      ): collection.Seq[SegmentsIntersections] = {
    val result: ArrayBuffer[SegmentsIntersections] = new ArrayBuffer[SegmentsIntersections]()
    // mapping point -> segments whose upper end starts at that point
    val queue = new java.util.TreeMap[V2, mutable.HashSet[SegmentPlanar]](pointOrdering) //new mutable.TreeMap[V2, mutable.HashSet[SegmentPlanar]]()
    //init queue
    val segmentsFlipped = segments.map(s => if (pointOrdering.lt(s.v1, s.v2)) s else s.flip)
    segmentsFlipped.foreach {
      segment =>
        //add start to existing set if exists create new if not
        if (!queue.containsKey(segment.v1)) {
          queue.put(segment.v1, new mutable.HashSet[SegmentPlanar]())
        }
        queue.get(segment.v1) += segment
        //add end
        if (!queue.containsKey(segment.v2)) {
          queue.put(segment.v2, new mutable.HashSet[SegmentPlanar]())
        }
    }

    val status: Status = new StatusListImpl
    statusAndQueueCallback.foreach(c => c(status, queue))

    def reportIntersectionIfNeeded(point: V2, upper: collection.Iterable[SegmentPlanar], lower: collection.Iterable[SegmentPlanar], center: collection.Iterable[SegmentPlanar]): Unit = {
      val union: Set[SegmentPlanar] = (upper ++ lower ++ center).toSet
      if (union.size > 1) {
        result.append((point, union))
        intersectionCallback.foreach(_.apply(point, union))
      }
    }

    def findNewEvent(sl: SegmentPlanar, sr: SegmentPlanar, sweepPoint: V2): Unit = {
      sl.intersection(sr).foreach {
        case PointIntersection(p) => if (pointOrdering.gt(p, sweepPoint) && !queue.containsKey(p)) {
          queue.put(p, new mutable.HashSet[SegmentPlanar]())
        }
        case SegmentIntersection(s) =>
      }
    }

    //true = continue
    def handleEvent(sweepPoint: V2, upper: Set[SegmentPlanar]): Unit = {
      /*DEBUG*/
      beforeHandleEventCallback.foreach(_.apply(sweepPoint, upper))

      val (lower, center) = status.removeSegmentsContainsPoint(sweepPoint).partition(s => s.v2 ~= sweepPoint)

      /*DEBUG*/
      afterRemoveHandleEventCallback.foreach(_.apply(sweepPoint, upper, center.toSet, lower.toSet))

      reportIntersectionIfNeeded(sweepPoint, upper, lower, center)
      // status.remove(lower, sweepPoint)
      // status.remove(center, sweepPoint)
      status.add(center, sweepPoint)
      status.add(upper, sweepPoint)

      /*DEBUG*/
      afterAddHandleEventCallback.foreach(_.apply(sweepPoint, upper, center.toSet))

      if (upper.isEmpty && center.isEmpty) { //no insertion only removal
        status.lrNeighboursForEmptyPoint(sweepPoint).foreach(lr => findNewEvent(lr._1, lr._2, sweepPoint))
      } else {
        val lrNeighbours = status.leftAndRightSegmentAndNeighbourAt(sweepPoint)
        lrNeighbours.leftPair.foreach(p => findNewEvent(p._1, p._2, sweepPoint))
        lrNeighbours.rightPair.foreach(p => findNewEvent(p._1, p._2, sweepPoint))
        //if(lrNeighbours.leftPair.isDefined && lrNeighbours.rightPair)
      }
      afterNewEventsHandleEventCallback.foreach(_.apply(sweepPoint))
    }

    //process queue
    while (!queue.isEmpty) {
      val currentEvent = queue.pollFirstEntry()
      handleEvent(currentEvent.getKey, currentEvent.getValue.toSet)
    }

    return result
  }
}

//BentleyOttoman
/*case class SegmentsIntersection(segments: Seq[SegmentPlanar]) {

  private lazy val queue = new collection.mutable.PriorityQueue[Event]()(SegmentsIntersection.EventsOrdering)

  private val status: ArrayBuffer[SegmentPlanar] = new ArrayBuffer[SegmentPlanar]()

  private lazy val segmentsFlipped = segments.map(s => if (SegmentsIntersection.PointOrdering.lt(s.v1, s.v2)) s else s.flip)

  def initQueue(): Unit = segmentsFlipped.foreach {
    s =>
      queue += Start(s.v1, s)
      queue += End(s.v2, s)
  }


  def lineSweep(): Set[SegmentPlanarIntersection] = if (queue.nonEmpty) {
    processEvent(queue.dequeue()) ++ lineSweep()
  } else Set()

  def findIndexToInsert(point: V2): Int = {
    //todo indexWhere
    var lastLesserIndex: Option[Int] = None
    breakable {
      for (i <- status.indices) {
        status(i).line.y(point.x) match {
          case Some(segmentY) =>
            if (segmentY < point.y) {
              lastLesserIndex = Some(i)
            } else {
              break
            }
          case None => //vertical segment
            if (status(i).v1.y < point.y) {
              lastLesserIndex = Some(i)
            } else {
              break
            }
        }
      }
    }
    lastLesserIndex.getOrElse(0)
  }

  //def filter(minLeq:V2): V2=>Boolean = v => v

  def updateIntersections(minLeq: V2, i1: Int, i2: Int): Set[SegmentPlanarIntersection] =
    if (i1 < 0 || i2 < 0 || i1 >= status.length || i2 >= status.length)
      Set()
    else {
      println(status + " " + i1 + " " + i2 + " " + minLeq)
      println(status(i1).intersection(status(i2)))
      status(i1).intersection(status(i2)).filter {
        _ match {
          case PointIntersection(p) => SegmentsIntersection.PointOrdering.gteq(p, minLeq)
          case SegmentIntersection(s) => SegmentsIntersection.PointOrdering.gteq(s.v1, minLeq) && SegmentsIntersection.PointOrdering.gteq(s.v2, minLeq)
        }
      }.map {
        case pi@PointIntersection(p) =>
          queue.enqueue(Intersection(p, status(i1), status(i2)))
          pi
        case pi@SegmentIntersection(s) =>
          //TODO correct
          queue.enqueue(Intersection(s.v1, status(i1), status(i2)))
          queue.enqueue(Intersection(s.v2, status(i1), status(i2)))
          pi
      }.toSet
    }


  def updateNeighboursAfterInsert(minLeq: V2, insertedIndex: Int): Set[SegmentPlanarIntersection] = {
    updateIntersections(minLeq, insertedIndex, insertedIndex - 1) ++
      updateIntersections(minLeq, insertedIndex, insertedIndex + 1)
  }

  def updateNeighboursAfterRemove(minLeq: V2, removedIndex: Int): Set[SegmentPlanarIntersection] = {
    updateIntersections(minLeq, removedIndex - 1, removedIndex)
  }

  def updateNeighboursAfterSwap(minLeq: V2, lesserIndex: Int): Set[SegmentPlanarIntersection] = {
    updateIntersections(minLeq, lesserIndex, lesserIndex - 1) ++
      updateIntersections(minLeq, lesserIndex + 1, lesserIndex + 2)
  }

  def addToStatus(point: V2, s: SegmentPlanar): Set[SegmentPlanarIntersection] = {
    val i: Int = findIndexToInsert(point)
    status.insert(i, s)
    updateNeighboursAfterInsert(point, i)
  }

  def removeFromStatus(point: V2, s: SegmentPlanar): Set[SegmentPlanarIntersection] = {
    val index = status.indexOf(s)
    if (index >= 0) {
      status.remove(index)
      //status -= s
      updateNeighboursAfterRemove(s.v2, index)
    } else {
      Logging.logger.log(Level.SEVERE, "Remove from status: FATAL ERROR cant find segment to delete")
      Set()
    }
  }

  def swapInStatus(intersectionPoint: V2, s1: SegmentPlanar, s2: SegmentPlanar): Set[SegmentPlanarIntersection] = {
    val i1 = status.indexOf(s1)
    val i2 = status.indexOf(s2)
    if (i1 < 0 || i2 < 0) {
      Logging.logger.log(Level.SEVERE, "Remove from status: FATAL ERROR cant find segments to delete")
      return Set()
    }
    //swap
    val tmp = status(i1)
    status(i1) = status(i2)
    status(i2) = tmp
    updateNeighboursAfterSwap(intersectionPoint: V2, Math.min(i1, i2))
  }

  /*s.tan match {
    case Some(_) =>


    case None => //TODO vertical segment
  }*/


  def processEvent(e: Event): Set[SegmentPlanarIntersection] = {
    Logging.logger.log(Level.SEVERE, s"event: $e")
    e match {
      case Start(p, s) => addToStatus(p, s)
      case End(p, s) => removeFromStatus(p, s)
      case Intersection(p, s1, s2) => swapInStatus(p, s1, s2)
    }
  }

  lazy val result: Set[SegmentPlanarIntersection] = {
    initQueue()
    lineSweep()
  }
}

*/


