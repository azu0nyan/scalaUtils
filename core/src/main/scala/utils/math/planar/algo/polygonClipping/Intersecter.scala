package utils.math.planar.algo.polygonClipping

import utils.datastructures.containers.DoubleLinkedList
import utils.datastructures.containers.DoubleLinkedList.{DoubleLinkedList, Node}
import utils.math.planar.V2

import scala.collection.mutable.ListBuffer

class Intersecter(var selfIntersection: Boolean)(implicit epsilon: Epsilon) {
  var event_root: DoubleLinkedList[Event] = new DoubleLinkedList[Event]

  //add regions
  def addRegion(region: Region): Unit = {
    // regions are a list of points:
    //  [ [0, 0], [100, 0], [50, 100] ]
    // you can add multiple regions before running calculate
    var pt1: V2 = null
    var pt2: V2 = region.last
    for (i <- region.indices) {
      pt1 = pt2
      pt2 = region(i)

      val forward = EpsilonOps.pointsCompare(pt1, pt2)
      if (forward == 0) { // points are equal, so we have a zero-length segment
        // continue
        // just skip it
      } else {
        eventAddSegment(
          new Segment(
            if (forward < 0) pt1 else pt2,
            if (forward < 0) pt2 else pt1
          ),
          true
        )
      }
    }
  }


  def eventCompare(p1_isStart: Boolean, p1_1: V2, p1_2: V2, p2_isStart: Boolean, p2_1: V2, p2_2: V2): Int = {
    // compare the selected points first
    val comp: Int = EpsilonOps.pointsCompare(p1_1, p2_1)
    if (comp != 0) {
      return comp
    } else {
      // the selected points are the same
      if (EpsilonOps.pointsSame(p1_2, p2_2)) { // if the non-selected points are the same too...
        return 0 // then the segments are equal
      } else if (p1_isStart != p2_isStart) { // if one is a start and the other isn't...
        return if (p1_isStart) 1 else -1 // favor the one that isn't the start
      } else {
        // otherwise, we'll have to calculate which one is below the other manually
        return if (EpsilonOps.pointAboveOrOnLine(
          p1_2,
          if (p2_isStart) p2_1 else p2_2, // order matters
          if (p2_isStart) p2_2 else p2_1
        )) 1 else -1
      }
    }
  }


  def eventAdd(ev: Event, other_pt: V2): Unit = {
    ev.myNode = event_root.insertBefore(ev, here => {
      // should ev be inserted before here?
      val comp = eventCompare(
        ev.isStart, ev.pt, other_pt,
        here.isStart, here.pt, here.other.pt
      )
      comp < 0
    })
  }

  def eventAddSegmentStart(seg: Segment, primary: Boolean): Event = {
    val ev_start = new Event(
      isStart = true,
      pt = seg.start,
      seg = seg,
      primary = primary,
      other = null
    )
    eventAdd(ev_start, seg.end)
    return ev_start
  }

  def eventAddSegmentEnd(ev_start: Event, seg: Segment, primary: Boolean): Unit = {
    val ev_end = new Event(
      isStart = false,
      pt = seg.end,
      seg = seg,
      primary = primary,
      other = ev_start,
    )
    ev_start.other = ev_end
    eventAdd(ev_end, ev_start.pt)
  }

  def eventAddSegment(seg: Segment, primary: Boolean): Event = {
    val ev_start = eventAddSegmentStart(seg, primary)
    eventAddSegmentEnd(ev_start, seg, primary)
    return ev_start
  }

  def eventUpdateEnd(ev: Event, end: V2): Unit = {
    // slides an end backwards
    //   (start)------------(end)    to:
    //   (start)---(end)

    //    if (buildLog)
    //      buildLog.segmentChop(ev.seg, end);

    ev.other.myNode.removeMe();
    ev.seg.end = end;
    ev.other.pt = end;
    eventAdd(ev.other, ev.pt);
  }

  def eventDivide(ev: Event, pt: V2): Event = {
    val ns = ev.seg.segmentCopy(pt, ev.seg.end);
    eventUpdateEnd(ev, pt);
    return eventAddSegment(ns, ev.primary);
  }

  ///calculate
  def calculate(inverted: Boolean): Iterable[Segment] = {
    // is the polygon inverted?
    // returns segments
    return calculate(inverted, false)
  }

  def calculate(primaryPolyInverted: Boolean, secondaryPolyInverted: Boolean): Iterable[Segment] = {
    // if selfIntersection is true then there is no secondary polygon, so that isn't used

    //
    // status logic
    //

    val status_root: DoubleLinkedList[Event] = new DoubleLinkedList[Event]

    def statusCompare(ev1: Event, ev2: Event): Int = {
      val a1 = ev1.seg.start
      val a2 = ev1.seg.end
      val b1 = ev2.seg.start
      val b2 = ev2.seg.end

      if (EpsilonOps.pointsCollinear(a1, b1, b2)) {
        if (EpsilonOps.pointsCollinear(a2, b1, b2))
          return 1; //eventCompare(true, a1, a2, true, b1, b2);
        return if (EpsilonOps.pointAboveOrOnLine(a2, b1, b2)) 1 else -1
      }
      return if (EpsilonOps.pointAboveOrOnLine(a1, b1, b2)) 1 else -1
    }

    def statusFindSurrounding(ev: Event): (Option[Node[Event]], Option[Node[Event]], Event => Node[Event]) =
      status_root.findTransition(here => {
        val comp = statusCompare(ev, here)
        comp > 0;
      })


    def checkIntersection(ev1: Event, ev2: Event): Option[Event] = {
      // returns the segment equal to ev1, or false if nothing equal

      val seg1 = ev1.seg
      val seg2 = ev2.seg
      val a1 = seg1.start
      val a2 = seg1.end
      val b1 = seg2.start
      val b2 = seg2.end

      //      if (buildLog)
      //        buildLog.checkIntersection(seg1, seg2);

      val lineIntersection = EpsilonOps.linesIntersect(a1, a2, b1, b2);

      if (lineIntersection.isEmpty) {
        // segments are parallel or coincident

        // if points aren't collinear, then the segments are parallel, so no intersections
        if (!EpsilonOps.pointsCollinear(a1, a2, b1))
          return None;
        // otherwise, segments are on top of each other somehow (aka coincident)

        if (EpsilonOps.pointsSame(a1, b2) || EpsilonOps.pointsSame(a2, b1))
          return None; // segments touch at endpoints... no intersection

        val a1_equ_b1 = EpsilonOps.pointsSame(a1, b1);
        val a2_equ_b2 = EpsilonOps.pointsSame(a2, b2);

        if (a1_equ_b1 && a2_equ_b2)
          return Some(ev2); // segments are exactly equal

        val a1_between = !a1_equ_b1 && EpsilonOps.pointBetween(a1, b1, b2);
        val a2_between = !a2_equ_b2 && EpsilonOps.pointBetween(a2, b1, b2);

        // handy for debugging:
        // buildLog.log({
        //	a1_equ_b1: a1_equ_b1,
        //	a2_equ_b2: a2_equ_b2,
        //	a1_between: a1_between,
        //	a2_between: a2_between
        // });

        if (a1_equ_b1) {
          if (a2_between) {
            //  (a1)---(a2)
            //  (b1)----------(b2)
            eventDivide(ev2, a2);
          } else {
            //  (a1)----------(a2)
            //  (b1)---(b2)
            eventDivide(ev1, b2);
          }
          return Some(ev2)
        } else if (a1_between) {
          if (!a2_equ_b2) {
            // make a2 equal to b2
            if (a2_between) {
              //         (a1)---(a2)
              //  (b1)-----------------(b2)
              eventDivide(ev2, a2);
            } else {
              //         (a1)----------(a2)
              //  (b1)----------(b2)
              eventDivide(ev1, b2);
            }
          }

          //         (a1)---(a2)
          //  (b1)----------(b2)
          eventDivide(ev2, a1);
        }
      } else {
        // otherwise, lines intersect at i.pt, which may or may not be between the endpoints
        val i = lineIntersection.get
        // is A divided between its endpoints? (exclusive)
        if (i.alongA == 0) {
          if (i.alongB == -1) // yes, at exactly b1
            eventDivide(ev1, b1);
          else if (i.alongB == 0) // yes, somewhere between B's endpoints
            eventDivide(ev1, i.pt);
          else if (i.alongB == 1) // yes, at exactly b2
            eventDivide(ev1, b2);
        }

        // is B divided between its endpoints? (exclusive)
        if (i.alongB == 0) {
          if (i.alongA == -1) // yes, at exactly a1
            eventDivide(ev2, a1);
          else if (i.alongA == 0) // yes, somewhere between A's endpoints (exclusive)
            eventDivide(ev2, i.pt);
          else if (i.alongA == 1) // yes, at exactly a2
            eventDivide(ev2, a2);
        }
      }
      return None
    }

    //
    // main event loop
    //
    val segments: ListBuffer[Segment] = new ListBuffer[Segment];


    while (event_root.nonEmpty) {
      import util.control.Breaks.*
      breakable {
        var ev = event_root.first //no remove??????

        //      if (buildLog)
        //        buildLog.vert(ev.pt[0]);

        if (ev.isStart) {

          //        if (buildLog)
          //          buildLog.segmentNew(ev.seg, ev.primary);

          val surrounding = statusFindSurrounding(ev);
          val above: Option[Node[Event]] = surrounding.before //.orNull //isDefined ? surrounding.before.ev : null;
          val below: Option[Node[Event]] = surrounding.after //.orNull// ? surrounding.after.ev : null;

          /*  if (buildLog){
            buildLog.tempStatus(
              ev.seg,
              above ? above.seg : false,
              below ? below.seg : false
            );
          }*/

          def checkBothIntersections(): Option[Event] = {
            if (above.isDefined) {
              val eve = checkIntersection(ev, above.get.value);
              if (eve.isDefined) return eve
            }
            if (below.isDefined) return checkIntersection(ev, below.get.value);
            return None
          }

          val eve = checkBothIntersections();
          if (eve.isDefined) {
            // ev and eve are equal
            // we'll keep eve and throw away ev

            // merge ev.seg's fill information into eve.seg

            if (selfIntersection) {
              val toggle = // are we a toggling edge?
                if (ev.seg.myFill.below.isEmpty) true
                else ev.seg.myFill.above != ev.seg.myFill.below //todo corect compare null = false etc

              // merge two segments that belong to the same polygon
              // think of this as sandwiching two segments together, where `eve.seg` is
              // the bottom -- this will cause the above fill flag to toggle
              if (toggle) eve.get.seg.myFill.above = eve.get.seg.myFill.notAbove /*!eve.seg.myFill.above;*/
            } else {
              // merge two segments that belong to different polygons
              // each segment has distinct knowledge, so no special logic is needed
              // note that this can only happen once per segment in this phase, because we
              // are guaranteed that all self-intersections are gone
              eve.get.seg.otherFill = ev.seg.myFill
            }

            //          if (buildLog)
            //            buildLog.segmentUpdate(eve.seg);

            ev.other.myNode.removeMe();
            ev.myNode.removeMe();
          }

          if (event_root.first != ev) {
            // something was inserted before us in the event queue, so loop back around and
            // process it before continuing
            //          if (buildLog)
            //            buildLog.rewind(ev.seg);
            // continue;
            break()
          }

          //
          // calculate fill flags
          //
          if (selfIntersection) {
            val toggle = // are we a toggling edge?
              (if (ev.seg.myFill.below.isEmpty) // if we are a new segment...
                true // then we toggle
              else // we are a segment that has previous knowledge from a division
                ev.seg.myFill.above != ev.seg.myFill.below) // calculate toggle

            // next, calculate whether we are filled below us
            if (below.isEmpty) { // if nothing is below us...
              // we are filled below us if the polygon is inverted
              ev.seg.myFill.below = Some(primaryPolyInverted)
            } else {
              // otherwise, we know the answer -- it's the same if whatever is below
              // us is filled above it
              ev.seg.myFill.below = below.get.value.seg.myFill.above
            }

            // since now we know if we're filled below us, we can calculate whether
            // we're filled above us by applying toggle to whatever is below us
            if (toggle) ev.seg.myFill.above = ev.seg.myFill.notBelow //ev.seg.myFill.above = !ev.seg.myFill.below;
            else ev.seg.myFill.above = ev.seg.myFill.below
          } else {
            // now we fill in any missing transition information, since we are all-knowing
            // at this point
            if (ev.seg.otherFill == null) {
              // if we don't have other information, then we need to figure out if we're
              // inside the other polygon
              val inside = if (below.isEmpty) { //!below
                // if nothing is below us, then we're inside if the other polygon is
                // inverted
                Some(if (ev.primary) secondaryPolyInverted else primaryPolyInverted)
              } else { // otherwise, something is below us
                // so copy the below segment's other polygon's above
                if (ev.primary == below.get.value.primary) below.get.value.seg.otherFill.above
                else below.get.value.seg.myFill.above
              }
              ev.seg.otherFill = new Fill(inside, inside)
            }
          }

          /*if (buildLog){
          buildLog.status(
            ev.seg,
            above ? above.seg : false,
            below ? below.seg : false
          );
        }*/

          // insert the status and remember it for later removal
          ev.other.status = surrounding.insert(ev)
        } else { //this is end
          val st = ev.status;

          if (st == null) {
            throw new Exception("PolyBool: Zero-length segment detected; your epsilon is probably too small or too large");
          }

          // removing the status will create two new adjacent edges, so we'll need to check
          // for those
          if (st.prev != null && st.next != null /*status_root.exists(st.prev) && status_root.exists(st.next)*/ )
            checkIntersection(st.prev.value, st.next.value);

          //        if (buildLog)          buildLog.statusRemove(st.ev.seg);

          // remove the status
          st.removeMe();

          // if we've reached this point, we've calculated everything there is to know, so
          // save the segment for reporting
          if (!ev.primary) {
            // make sure `seg.myFill` actually points to the primary polygon though
            val s = ev.seg.myFill;
            ev.seg.myFill = ev.seg.otherFill;
            ev.seg.otherFill = s;
          }
          segments += ev.seg
        }

        // remove the event and continue
        //      event_root.getHead().remove();
        event_root.removeFirst()
      }
    }

    //    if (buildLog)
    //      buildLog.done();

    return segments;
  }

//  // return the appropriate API depending on what we're doing
//  if (!selfIntersection) {
//    // performing combination of polygons, so only deal with already-processed segments
//    return {
//      calculate: function
//      (segments1, inverted1, segments2, inverted2) {
//        // segmentsX come from the self-intersection API, or this API
//        // invertedX is whether we treat that list of segments as an inverted polygon or not
//        // returns segments that can be used for further operations
//        segments1.forEach(function(seg) {
//          eventAddSegment(segmentCopy(seg.start, seg.end, seg), true);
//        });
//        segments2.forEach(function(seg) {
//          eventAddSegment(segmentCopy(seg.start, seg.end, seg), false);
//        });
//        return calculate(inverted1, inverted2);
//      }
//    };
//  }
}
