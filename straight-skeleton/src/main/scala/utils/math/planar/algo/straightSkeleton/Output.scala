package utils.math.planar.algo.straightSkeleton


import utils.datastructures.containers.map.impl.MutableMultiMap
import utils.math.planar.algo.straightSkeleton.helpers.{AngleAccumulator, Cache, GraphMap, Loop, LoopL, Loopable}
import utils.math.space.V3

import scala.collection.mutable
import scala.util.boundary
import boundary.break
import scala.concurrent.Future


/**
 * @author twak
 */
object Output {
  // marker for horizontal input edges (other edges can also be horizontal...)
  var isCreatedHorizontal = new Tag("horizontal")

  class LoopNormal(
                    var loopl: LoopL[_ <: V3],
                    var norm: V3
                  )

  class SharedEdge(
                    var start: V3,
                    var end: V3
                  ) {
    var left: Output#Face = null
    var right: Output#Face = null
    var cLeft: Option[Loopable[Output.SharedEdge]] = None
    var cRight: Option[Loopable[Output.SharedEdge]] = None
    val features = new mutable.HashSet[Tag]()

    def getStart(ref: Output#Face): Option[V3] =
      Option.when(ref eq left)(end) orElse
        Option.when(ref eq right)(start)

    def getEnd(ref: Output#Face): Option[V3] =
      Option.when(ref eq left)(start) orElse
        Option.when(ref eq right)(end)

    /**
     * Symetric wrt start, end!
     */
    override def equals(obj: Any): Boolean =
      obj match
        case o: SharedEdge =>
          if (o.start == start) o.end == end
          else if (o.end == start) o.start == end
          else false
        case _ =>
          false

    /**
     * Symetric wrt start, end!
     */
    override def hashCode: Int = {
      var hash = 7
      hash += 71 * (
        if (this.start != null) this.start.hashCode
        else 0)
      hash += 71 * (
        if (this.end != null) this.end.hashCode
        else 0)
      hash
    }

    def getOther(ref: Output#Face): Option[Output#Face] =
      Option.when(ref eq left)(right) orElse
        Option.when(ref eq right)(left)

    def setLeft(start: V3, ctxLeft: Option[Loopable[Output.SharedEdge]], left: Output#Face): Unit = {
      if (this.start == start) {
        this.left = left
        this.cLeft = ctxLeft
      }
      else if (this.end == start) {
        this.right = left
        this.cRight = ctxLeft
      }
      else throw new Error
    }

    override def toString = "{" + start + " to " + end + "}"

    def getAdjEdge(f: Output#Face, next: Boolean): Option[Output.SharedEdge] =
      if (f eq left)
        cLeft.map(cLeft =>
          if (next) cLeft.getNext.get
          else cLeft.getPrev.get)
      else
        cRight.map(cRight =>
          if (next) cRight.getNext.get
          else cRight.getPrev.get
        )

    def dir(f: Output#Face): Option[V3] = {
      val p = getEnd(f)
      val ps = getStart(f)
      Option.when(p.nonEmpty && ps.nonEmpty)(p.get - ps.get)
    }

    def leftFromStart(x: V3): Option[Output#Face] =
      Option.when(x == start)(left) orElse
        Option.when(x == end)(right)
  }
}


class Output(var skeleton: Skeleton) {
  var faces = new mutable.LinkedHashMap[Corner, Output#Face] // the faces represented by each edge

  var nonSkelFaces = mutable.Buffer[Output.LoopNormal]()
  var nonSkelFaces2 = mutable.Buffer[Output.LoopNormal]()
  var edges = new mutable.HashMap[Output.SharedEdge, Output.SharedEdge]() // edge ensure that each output edge only exists once

  /**
   * One edge may start in two locations at the same time. To accommodate this, you call
   * newEdge once per new edge, then new Defining Segment for each corner that references that
   * edge.
   *
   * @param startCorner   The corner at the start of the edge at the base of this edge
   * @param aParentCorner A corner whose
   * @param profileFeatures
   */
  def newEdge(e: Edge, aParentLeadingCorner: Corner, profileFeatures: mutable.Set[Tag]): Unit = {
    val face = new Face(
      edge = e,
      parent = None,
      points = new LoopL[V3],
    )
    if (profileFeatures != null) face.profile = profileFeatures
    if (aParentLeadingCorner != null) {
      val parentFace = faces.get(aParentLeadingCorner)
      face.parent = parentFace
      skeleton.parent(face, parentFace.get) //todo safe
    } // an originator - an edge in the plan

    // we assume that start locations for edges are unique!
    assert(!faces.contains(e.start))
    e.start.nextL = e // these are always true?! - we rely on them for indexing, below

    e.end.prevL = e
    faces.put(e.start, face)
  }
  /**
   * see newEdge.
   */
  def newDefiningSegment(leadingCorner: Corner): Unit = {
    val face = faces.get(leadingCorner.nextL.start).get // todo safe
    val se = createEdge(leadingCorner.asV3, leadingCorner.nextC.asV3)
    face.definingSE.add(se)
    se.setLeft(leadingCorner.asV3, None, face)
    face.results.add(se.start, se.end)
    se.features.add(Output.isCreatedHorizontal)
    face.definingCorners.add(leadingCorner)
  }
  def addOutputSideTo(a: V3, b: V3, edges: Edge*): Unit =
    addOutputSideTo_(false, a, b, edges *)

  def addOutputSideTo_(isTop: Boolean, a: V3, b: V3, edges: Edge*): Unit = {
    for (edge <- edges) {
      val c = edge.start // assumption: start of edge will always be a leading corner on the face!

      val f = faces(c) //todo safe
      if (isTop)
        f.topSE.add(createEdge(a, b))
      // just check those tuple's aren't corners....
      f.results.add(a, b)
    }
  } // a.y == b.yequals(b)

  /**
   * Some faces (such as base-plates for globals) can't really be classified nicely.
   * They live here.
   *
   * @param geom
   */
  def addNonSkeletonOutputFace(points: LoopL[V3], norm: V3): Unit =
    nonSkelFaces += new Output.LoopNormal(points, norm)

  def addNonSkeletonSharedEdges(profileTags: Tag*): Unit = {

    for (ln <- nonSkelFaces) {

      for (loop <- ln.loopl.iterator) {
        val f = new Face(new LoopL[V3], null, None)
        for (t <- profileTags) {
          f.profile.add(t)
        }
        val seloop = new Loop[Output.SharedEdge]
        f.edges.add(seloop)
        f.points = new LoopL[V3]
        val fLoop = new Loop[V3]
        f.points.add(fLoop)

        for (loopable <- loop.loopableIterator) {
          val e = createEdge(loopable.get, loopable.getNext.get)
          fLoop.append(loopable.get)
          if (f.definingSE.isEmpty) {
            f.definingSE.add(e)
            f.edge = new Edge(new Corner(e.start), new Corner(e.end))
            val dir = f.edge.direction.normalize
            f.edge.uphill = new V3(-dir.y, dir.x, 0)
            faces.put(new Corner(loopable.get), f)
          }
          e.setLeft(loopable.get, Some(seloop.append(e)), f)
        }
      }
    }
  }

  def addNonSkeletonOutputFace2(points: LoopL[V3], norm: V3): Unit = {
    nonSkelFaces2 += new Output.LoopNormal(points, norm)
  }

  def setParent(neu: Corner, old: Corner): Unit = {
    val nF = faces(neu)
    val oF = faces(old)
    skeleton.parent(nF, oF)
    nF.parent = Some(oF)
  }

  def calculate(skel: Skeleton): Unit =
    for (face <- faces.values) try {
      calculateForFace(face)
    } catch
      case t: Throwable =>
        t.printStackTrace()


  private def calculateForFace(face: Output#Face): Unit =
    boundary:
      {
        enum Continue {
          case Edge, No
        }

        val notVisited = new mutable.LinkedHashSet[V3]()
        notVisited ++= face.results.map.keySet
        val faceWithHoles = new LoopL[V3] // first entry here is outer boundary

        face.points = faceWithHoles

        val edgeStart = face.definingSE.iterator.next.getStart(face).get //todo safe
        while (notVisited.nonEmpty) {
          // associated face input polygon
          val poly = new Loop[V3]
          //                faceWithHoles.add( poly );
          val isOuter = notVisited.contains(edgeStart)
          val start = if (isOuter) edgeStart
          else notVisited.iterator.next
          var pos = start
          var last = face.results.map.get(start)(0) // arb. direction

          whileBody() match
            case Continue.Edge => break()
            case Continue.No => ()

          def whileBody(): Continue = boundary:
            var first: V3 = null
            var lastAdded: V3 = null
            val ac = new AngleAccumulator(isOuter, face.edge.getPlaneNormal)
            var count = 0
            //pointsInLoop
            var firstIter = true
            while (firstIter || (pos ne start)) {
              firstIter = false
              val choice = face.results.map.get(pos)
              assert(choice != null)


              var breaked = false
              for (c <- choice if !breaked) {
                if ( {
                  count += 1;
                  count - 1
                } > 1000) break(Continue.Edge) // continue edge// handbrake turn!

                if (!(last == c) && !(pos == c)) {
                  if (first == null) first = c
                  notVisited.remove(c)
                  // remove short edges between the previous corners, and between the current corner and the startstart (bad hack)
                  if ((lastAdded == null || lastAdded.distance(c) > 0.01) && ((first eq c) || first.distance(c) > 0.01)) {
                    poly.append(c)
                    ac.add(c)
                    lastAdded = c
                  }
                  last = pos
                  pos = c
                  breaked = true //continue pointsInLoop

                }

              }
              break(Continue.Edge) // continue edge

            }
            // inner loops go counter clockwise
            if (!ac.correctAngle())
              poly.reverse
            removeStraights(poly)
            // as we remove degenerately small polygons *
            if (poly.count >= 3)
              faceWithHoles.add(poly)

            Continue.No
        }
      }


  def calculate_(skel: Skeleton): Unit = {
    object edge extends Exception("edge")
    object pointsInLoop extends Exception("pointsInLoop")

    // collect identical edges in different polygons
    for (face <- faces.values) try {
      val notVisited = new mutable.LinkedHashSet[V3]()
      notVisited ++= face.results.map.keySet
      val faceWithHoles = new LoopL[V3] // first entry here is outer boundary

      face.points = faceWithHoles
      try {
        val edgeStart = face.definingSE.iterator.next.getStart(face).get //todo safe
        while (!notVisited.isEmpty) {
          // associated face input polygon
          val poly = new Loop[V3]
          //                faceWithHoles.add( poly );
          val isOuter = notVisited.contains(edgeStart)
          val start = if (isOuter) edgeStart
          else notVisited.iterator.next
          var pos = start
          var last = face.results.map.get(start)(0) // arb. direction

          var first: V3 = null
          var lastAdded: V3 = null
          val ac = new AngleAccumulator(isOuter, face.edge.getPlaneNormal)
          var count = 0
          //pointsInLoop
          var firstIter = true
          while (firstIter || (pos ne start)) try {
            firstIter = false
            val choice = face.results.map.get(pos)
            assert(choice != null)

            for (c <- choice) {
              if ( {
                count += 1;
                count - 1
              } > 1000) throw edge // continue edge// handbrake turn!

              if (!(last == c) && !(pos == c)) {
                if (first == null) first = c
                notVisited.remove(c)
                // remove short edges between the previous corners, and between the current corner and the startstart (bad hack)
                if ((lastAdded == null || lastAdded.distance(c) > 0.01) && ((first eq c) || first.distance(c) > 0.01)) {
                  poly.append(c)
                  ac.add(c)
                  lastAdded = c
                }
                last = pos
                pos = c
                throw pointsInLoop //continue pointsInLoop

              }
            }
            throw edge // continue edge

          } catch {
            case e: pointsInLoop.type =>
              print("")
          }
          // inner loops go counter clockwise
          if (!ac.correctAngle())
            poly.reverse
          removeStraights(poly)
          // as we remove degenerately small polygons *
          if (poly.count >= 3) faceWithHoles.add(poly)
        }
      } catch {
        case e: Throwable if e != edge =>
          e.printStackTrace()
        //continue
      }
    } catch {
      case e: edge.type => //catch continue edge
    }

    // * so we remove faces without polygons
    val nullFaces = mutable.Buffer[Output#Face]()

    for (f <- faces.values) {
      if (f.points.count <= 0)
        nullFaces += f
    }

    for (f <- faces.values) {
      f.findSharedEdges()
    }
  }
  /**
   * Two parallel faces have become consecutive, remove info about toGo,
   * add to toKeep
   *
   * @toKeep leading corner to keep
   * @toGo leading corner to go
   */
  def merge(toKeep: Corner, toGo: Corner): Unit = { //todo safe
    val toGoFace = faces.get(toGo.nextL.start)
    val toKeepFace = faces.get(toKeep.nextL.start)
    if (toGoFace.isEmpty) {
      System.err.println("three consecutive parallel edges in input?")
    } else {
      toKeepFace.get.definingSE ++= toGoFace.get.definingSE
      if (toKeepFace.get.results != toGoFace.get.results) { //todo check needed????
        toKeepFace.get.results.map ++= toGoFace.get.results.map
      }
      toKeepFace.get.definingCorners ++= toGoFace.get.definingCorners
      // forward any further face requests to the new one
      //        faces.put (toGo.nextL.start, toKeepFace);
      faces.put(toGo, toKeepFace.get)
    }
  }

  private def createEdge(start: V3, end: V3) = {
    var newEdge = new Output.SharedEdge(start, end)
    newEdge = edges.getOrElseUpdate(newEdge, newEdge) // identity lookup - only one edge!

    newEdge
  }

  def getSegmentOriginator = new Cache[Corner, mutable.Buffer[Corner]]() {
    override def get(aCorner: Corner) = {
      var f = faces.get(aCorner.nextL.start).get //todo safe
      while (f.parent.nonEmpty)
        f = f.parent.get
      mutable.Buffer[Corner](f.definingCorners.toSeq *)
    }
    override def create(i: Corner): mutable.Buffer[Corner] = ???
  }

  def getGreatestGrandParent(f: Option[Output#Face]): Option[Output#Face] = {
    f.map { f =>
      var cur = f
      while (cur.parent.nonEmpty)
        cur = cur.parent.get
      cur
    }
  }

  private def removeStraights(poly: Loop[V3]): Unit = {
    // this will be filtered out later. some of this assumes >= 3 edges.
    if (poly.count < 3) return
    val togo = new mutable.HashSet[Loopable[V3]]

    for ((a, b, c) <- new ConsecutiveTriples[Loopable[V3]](poly.loopableIterator.toSeq, true)) {
      val ab = b.get - a.get
      val bc = c.get - b.get

      val angle = ab.angle(bc)
      val small = 0.001
      if (angle < small || angle > Math.PI - small) togo.add(b)
    }

    for (lpb <- togo) {
      poly.remove(lpb)
    }
  }

  class Face(
              // first is outside, others are holes
              var points: LoopL[V3],
              // a typical edge that defines the plane normal
              var edge: Edge,
              // face below us in the skeleton - can be traced back to an originator
              var parent: Option[Output#Face] = None
            ) {
    var plan = new mutable.HashSet[Tag]()
    var profile: mutable.Set[Tag] = new mutable.HashSet[Tag]()
    // bottom edges
    var definingSE = new mutable.LinkedHashSet[Output.SharedEdge]
    // defining edges of child (top) edges
    var topSE = new mutable.LinkedHashSet[Output.SharedEdge]

    var results = new GraphMap()

    // subset of results who are horizontal edges and whose nextL are edge, or similar.
    var definingCorners = new mutable.LinkedHashSet[Corner]
    var edges = new LoopL[Output.SharedEdge]

    def getLoopL = points
    def pointCount = points.count
    // is a defining edges of the above (child) edges
    def isTop(edge: Output.SharedEdge) = topSE.contains(edge)
    // is a defining edge
    def isBottom(edge: Output.SharedEdge) = definingSE.contains(edge)
    // isn't a top or bottom edges
    def isSide(edge: Output.SharedEdge) = !(isTop(edge) || isBottom(edge))

    /**
     * When caculating an offset, we can assume that all edges add a face at every interval
     * this returns the number of faces below this face.
     *
     * @return
     */
    def getParentCount: Int = {
      var count = -1
      var f: Option[Output#Face] = Some(this)
      while (f.nonEmpty) {
        count += 1
        f = f.get.parent
      }
      count
    }

    def findSharedEdges(): Unit = {
      edges = new LoopL[Output.SharedEdge]

      for (ptLoop <- points.iterator) {
        val loop = new Loop[Output.SharedEdge]
        edges.add(loop)
        // start and end points of SharedEdges **aren't** shared

        for (loopable <- ptLoop.loopableIterator) {
          val e = createEdge(loopable.get, loopable.getNext.get)
          e.setLeft(loopable.get, Some(loop.append(e)), this)
        }
      }
    }
  }
  def dupeEdgesOnly(): Output = {
    val out = new Output(null)
    val fCache = new Cache[Output#Face, Output#Face]() {
      override def create(old: Output#Face) = {
        val face = new Face(
          parent = old.parent.map(get),
          points = new LoopL[V3],
          edge = new Edge(old.edge.start, old.edge.end)
        )
        val outGM = new GraphMap()
        outGM.map ++= old.results.map
        face.results = outGM
        face.definingSE = new mutable.LinkedHashSet[Output.SharedEdge]

        for (se <- old.definingSE) {
          val neu = new Output.SharedEdge(se.start, se.end)
          neu.setLeft(se.start, null, face)
          face.definingSE.add(neu)
        }
        face
      }
    }

    for (c <- faces.keySet) {
      out.faces.put(c, fCache.get(faces.get(c).get)) // todo safe
    }

    out
  }

}

