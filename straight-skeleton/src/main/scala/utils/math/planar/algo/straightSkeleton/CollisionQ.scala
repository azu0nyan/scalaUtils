package utils.math.planar.algo.straightSkeleton


/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import utils.math.Scalar
import utils.math.space.V3
import utils.math.space.V3

import scala.collection.mutable
import scala.util.Try
import scala.util.control.Breaks.{break, breakable}


/**
 * @param corners   the input set of corners
 * @param liveEdges the (continuously updated) set of edges that still feature
 *                  in the live corner list. Caller has to update this set :)
 */
class CollisionQ(var skel: Skeleton) {
  /**
   * Collisions between edges
   */
  var faceEvents: mutable.PriorityQueue[EdgeCollision] =
    new mutable.PriorityQueue[EdgeCollision]()(Ordering.by[EdgeCollision, Scalar](- _.getHeight))
  /**
   * Other control events (gradient changes...)
   */
  private var miscEvents: mutable.PriorityQueue[HeightEvent] = new mutable.PriorityQueue[HeightEvent]()(Ordering.by[HeightEvent, Scalar](- _.getHeight))
  // this is an acceleration structure - we don't process seen triples of edges twice. A similar structure occurs in Skeleton ~ they need merging...
  private val seen = new mutable.HashSet[EdgeCollision]
  private def nextEvent(): Option[HeightEvent] = {
    var ec: Option[EdgeCollision] = None

    breakable {
      while (true) {
        ec = Try(faceEvents.dequeue()).toOption
        if (ec.isEmpty)
          break
        // valid if we haven't seen it, and it's height is "greater" than the current skeleton height
        if (!skel.seen.contains(ec.get) && ec.get.loc.z - skel.height > -0.001)
          break
      }
    }
    ec match
      case None =>
        Try(miscEvents.dequeue()).toOption
      case Some(ec) =>
        miscEvents.headOption match
          case None =>
            skel.seen.add(ec)
            Some(ec)
          case Some(he) =>
            if (he.getHeight <= ec.getHeight) {
              faceEvents.enqueue(ec)
              Try(miscEvents.dequeue()).toOption // return he
            } else {
              skel.seen.add(ec)
              Some(ec) // return ec
            }
  }


  var currentCoHeighted: Option[HeightCollision] = None
  def poll: Option[HeightEvent] = {
    currentCoHeighted = None // now working at a new height

    val next = nextEvent()
    if (next.nonEmpty && next.get.isInstanceOf[EdgeCollision]) {
      val coHeighted = mutable.Buffer[EdgeCollision]()
      val ec = next.get.asInstanceOf[EdgeCollision]
      coHeighted += ec
      var height = ec.getHeight

      breakable {
        while (true) {
          faceEvents.headOption match
            case None => break
            case Some(higher) =>
              if (higher.getHeight - height < 0.00001) // ephemeral random constant #34 was 0.00001
              {
                faceEvents.dequeue() //same as higher

                if (!skel.seen.contains(higher)) {
                  height = higher.getHeight
                  skel.seen.add(higher)
                  coHeighted += higher
                }
              }
              else break

        }
      }
      currentCoHeighted = Some(new HeightCollision(coHeighted))
      currentCoHeighted
    }
    else next
  }
  def add(he: HeightEvent): Unit = {
    he match
      case collision: EdgeCollision => faceEvents += collision
      case _ => miscEvents += he
  }
  /**
   * Collide the new edge (toAdd.prev, toAdd.next) against
   * all other edges. Will also add 3 consecutive edges.
   *
   * @param toAdd
   */
  def addCorner(toAdd: Corner, postProcess: HeightCollision): Unit = {
    addCorner(toAdd, postProcess, false)
  }

  def addCorner(toAdd: Corner, postProcess: HeightCollision, useCache: Boolean): Unit = {
    // check these two edges don't share the same face
    if (!skel.preserveParallel && toAdd.prevL.sameDirectedLine(toAdd.nextL)) {
      removeCorner(toAdd)

    } else if (toAdd.prevL eq toAdd.nextC.nextL) { // loop of two - dissolves to a ridge
      skel.output.addOutputSideTo(toAdd.asV3, toAdd.nextC.asV3, toAdd.prevL, toAdd.nextL)
      toAdd.nextL.currentCorners.remove(toAdd) // we really should automate this

      toAdd.nextL.currentCorners.remove(toAdd.nextC)
      toAdd.prevL.currentCorners.remove(toAdd)
      toAdd.prevL.currentCorners.remove(toAdd.nextC)
      if (toAdd.nextL.currentCorners.isEmpty) skel.liveEdges.remove(toAdd.nextL)
      if (toAdd.prevL.currentCorners.isEmpty) skel.liveEdges.remove(toAdd.prevL)
      skel.liveCorners.remove(toAdd)
      skel.liveCorners.remove(toAdd.nextC)
    } else if (toAdd.prevL.isCollisionNearHoriz(toAdd.nextL)) { // Horizontal bisectors are rounded up and evaluated before leaving the current height event
      // if not a peak, add as a unsolved horizontal bisector
      if (toAdd.nextL.direction.angle(toAdd.prevL.direction) < 0.01)
        postProcess.newHoriz += toAdd
      // if just a peak, assume the loops-of-two-rule will finish it awf
    } else for (e <- skel.liveEdges) {
      val ex = new EdgeCollision(null, toAdd.prevL, toAdd.nextL, e)
      if ((!useCache) || !seen.contains(ex)) {
        seen.add(ex)
        cornerEdgeCollision(toAdd, e)
      }
    }
  }

  private def cornerEdgeCollision(corner: Corner, edge: Edge): Unit = {
    // check for the uphill vector of both edges being too similar (parallel edges)
    // also rejects e == corner.nextL or corner.prevL updated to take into account vertical edges - will always have same uphill! - (so we check edge direction too)
    if (skel.preserveParallel) {
      //			if ( edge.start.equals( corner ) || edge.end.equals( corner ) )
      //				return;
      if (CollisionQ.isParallel(edge, corner.prevL) && CollisionQ.isParallel(edge, corner.nextL)) return
      if ((corner.nextL eq edge) || (corner.prevL eq edge)) return
    }
    else {
      if (CollisionQ.isParallel(edge, corner.prevL) || CollisionQ.isParallel(edge, corner.nextL)) return //					( edge.uphill.angle( corner.prevL.uphill ) < 0.0001 && edge.direction().angle( corner.prevL.direction() ) < 0.0001 ) ||
      //				 ( edge.uphill.angle( corner.nextL.uphill ) < 0.0001 && edge.direction().angle( corner.nextL.direction() ) < 0.0001 ) )

    }
    var res: Option[V3] = None
    try {
      // sometimes locks up here if edge.linear form has NaN components.
      if (
        corner.prevL.linearForm.hasNaN ||
          corner.nextL.linearForm.hasNaN ||
          edge.linearForm.hasNaN
      ) throw new Error

      res = edge.linearForm.collide(corner.prevL.linearForm, corner.nextL.linearForm)
    } catch {
      case f: Throwable =>
        if (skel.preserveParallel) if (corner.prevL.uphill == edge.uphill && corner.prevC.prevL == edge)
          res = corner.nextL.linearForm.collide(corner.prevC.asV3, corner.prevL.uphill)
        else if (corner.nextL.uphill == edge.uphill && corner.nextC.nextL == edge)
          res = corner.prevL.linearForm.collide(corner.nextC.asV3, corner.nextL.uphill)
        else if (corner.nextL.uphill == corner.prevL.uphill)
          res = edge.linearForm.collide(corner.asV3, corner.nextL.uphill)
    }
    res match
      case Some(res) =>
        // cheap reject: if collision is equal or below (not the correct place to check) the corner, don't bother with it
        if (res.z < corner.z || res.z < edge.start.z)
          ()
        else {
          val ec = new EdgeCollision(res, corner.prevL, corner.nextL, edge)
          if (!skel.seen.contains(ec))
            faceEvents += (ec)
          ()
        }
      case None => ()
  }

  var _holdRemoves = false
  val removes = mutable.Buffer[Corner]()
  def holdRemoves(): Unit = {
    removes.clear()
    _holdRemoves = true
  }
  def resumeRemoves(): Unit = {
    _holdRemoves = false

    for (c <- removes) {
      if (skel.liveCorners.contains(c)) 
        removeCorner(c) // if hasn't been removed by horiz decomp
    }
    removes.clear()
  }
  /**
   * Given corner should be fully linked into the network. Needs to be removed as
   * it connects two parallel faces. We remove toAdd.nextL
   *
   *
   */
  private def removeCorner(toAdd: Corner): Unit = {
    if (_holdRemoves) {
      removes += toAdd
    } else {
      //    DebugDevice.dump("about to delete " + toAdd.toString, skel)
      // update corners
      toAdd.prevC.nextC = toAdd.nextC
      toAdd.nextC.prevC = toAdd.prevC
      // update edges
      toAdd.nextC.prevL = toAdd.prevL
      // update main corner list
      skel.liveCorners.remove(toAdd)
      //brute force search for all references to old edge (if this was on a per face basis it'd be much nicer)

      for (lc <- skel.liveCorners) {
        if (lc.nextL eq toAdd.nextL) lc.nextL = toAdd.prevL
        if (lc.prevL eq toAdd.nextL) lc.prevL = toAdd.prevL
      }
      if (toAdd.prevL ne toAdd.nextL) {
        // update live edge list
        skel.liveEdges.remove(toAdd.nextL)
        // update output edge list (the two input edges give one output face)
        //            skel.inputEdges.remove(toAdd.nextL);
        // update edges's live corners

        for (c <- toAdd.nextL.currentCorners) {
          if (toAdd.prevL.currentCorners.add(c)) { // also adds toAdd.nextC
          }
        }
        // merge output corner lists
        // add to the results map likewise
        skel.output.merge(toAdd.prevC, toAdd) //toAdd.prevL.addOutputSidesFrom (toAdd.nextL);

        // all collisions need recalculation. This situation could be avoided if collisions occur strictly with infinite faces.
        // recurse through all consecutive colinear faces...?
        skel.refindAllFaceEventsLater()
      }
      // update edges's live corners (might have copied this over from nextL)
      toAdd.prevL.currentCorners.remove(toAdd)
      // todo: we've merged two machines! (pick an arbitrary one?)
      //        assert ( toAdd.prevL.machine == toAdd.nextL.machine );
    }
  }

  def dump(): Unit = {
    var i = 0

    for (ec <- faceEvents) {
      System.out.println(String.format("%d : %s ", {
        i += 1;
        i - 1
      }, ec))
    }
  }
  def clearFaceEvents(): Unit = {
    faceEvents.clear()
  }
  def clearOtherEvents(): Unit = {
    miscEvents.clear()
  }
}


object CollisionQ {
  private def isParallel(a: Edge, b: Edge) = a.uphill.angle(b.uphill) < 0.0001 && a.direction.angle(b.direction) < 0.0001

  //  def angle(v0:V3, v1: V3) = {
  //    var vDot = this.dot(v1) / (this.length * v1.length)
  //    if (vDot < -1.0F.toDouble) vDot = -1.0F.toDouble
  //    if (vDot > 1.0F.toDouble) vDot = 1.0F.toDouble
  //    Math.acos(vDot)
  //  }
}
