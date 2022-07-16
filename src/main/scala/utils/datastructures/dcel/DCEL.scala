package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL._
import utils.math.planar.PolygonRegion
import utils.system.Event.{Event, EventImpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DCEL {
  type DCELData = {
    type VertexData
    type HalfEdgeData
    type FaceData
  }


  class DCELException(text: String) extends RuntimeException(text)

  class MalformedDCELException(text: String) extends DCELException(text)

  class MultipleEdgesBetweenTwoVerticesException(v1: Vertex[_], v2: Vertex[_]) extends MalformedDCELException(
    s"Multiple edges between two vertices v1: ${v1.data.toString} v2: ${v2.data.toString}"
  )

  class CantDoOpOnDCELException(text: String) extends DCELException(text)

  /**
    * @param _data               data associated with face
    * @param _incidentEdge       starting point to traverse in CCW order
    * @param _holesIncidentEdges own face's of edges that points to hole in face, one per hole,
    *                            holes bound is PolygonRegion, not Polygon
    *
    */
  class Face[D <: DCELData] private[DCEL](
                                           private[this] var _data: D#FaceData,
                                           private[dcel] var _incidentEdge: Option[HalfEdge[D]] = None,
                                           private[dcel] var _holesIncidentEdges: Set[HalfEdge[D]],
                                         ) {

    //    def parent:Option[Face[D]] = incidentEdge.map(_.allReachableFaces().filter(_.holes.nonEmpty))

    def holesContours: Iterator[Iterator[HalfEdge[D]]] = _holesIncidentEdges.iterator.map(_.traverseEdges)

    def data: D#FaceData = _data

    def data_=(value: D#FaceData): Unit = {
      _data = value
    }

    def incidentEdge: Option[HalfEdge[D]] = _incidentEdge

    /** own face's of edges that points to hole in face, one per hole */
    def holesIncidentEdges: Set[HalfEdge[D]] = _holesIncidentEdges

    def neighbourFaces: Iterator[Face[D]] = edges.map(_.twin.leftFace).distinct

    def outsideVertices: Iterator[Vertex[D]] = incidentEdge.map(_.traverseEdges.map(_.origin)).getOrElse(Iterator.empty)

    def borderEdges: Iterator[HalfEdge[D]] = incidentEdge.map(_.traverseEdges).getOrElse(Iterator.empty)

    def holesVertices: Iterator[Vertex[D]] = _holesIncidentEdges.iterator.flatMap(_.traverseEdges.map(_.origin))

    /** all own edges adjanced to holes */
    def holesEdges: Iterator[HalfEdge[D]] = _holesIncidentEdges.iterator.flatMap(_.traverseEdges)

    def holes: Iterator[Face[D]] = _holesIncidentEdges.iterator.map(_.twin._leftFace)

    def vertices: Iterator[Vertex[D]] = outsideVertices ++ holesVertices

    /** Includes border edges and hole edges */
    def edges: Iterator[HalfEdge[D]] = borderEdges ++ holesEdges

    /** If forEdge is in hole border, finds holeIncidentEdge that refers to forEdge's hole */
    def holeIncidentEdge(forEdge: HalfEdge[D]): Option[HalfEdge[D]] = _holesIncidentEdges.find(_.traverseEdges.contains(forEdge))

    /** requires O(twin.holeEdges) */
    def isHole: Boolean = incidentEdge.exists(_.twin.isHoleHalfSide)

    override def toString: String = s"F: ${_data} iE: ${incidentEdge.map(_.data)} holes: ${holes.toSeq.map(_.data)}"
  }

  /**
    * @param _data    data associated with half-edge
    * @param origin   start
    * @param twin     opposite half-edge
    * @param leftFace [D]  face to its left
    * @param next     next half‐edge on the boundary of incidentFace
    * @param prev     previous half-edge
    */
  class HalfEdge[D <: DCELData] private[dcel](
                                               private[dcel] var _data: D#HalfEdgeData,
                                               private[dcel] var _origin: Vertex[D],
                                               private[dcel] var _twin: HalfEdge[D],
                                               private[dcel] var _leftFace: Face[D],
                                               private[dcel] var _prev: HalfEdge[D],
                                               private[dcel] var _next: HalfEdge[D],
                                             ) {
    def origin: Vertex[D] = _origin

    def ending: Vertex[D] = _next.origin

    def twin: HalfEdge[D] = _twin


    def leftFace: Face[D] = _leftFace

    def next: HalfEdge[D] = _next

    def prev: HalfEdge[D] = _prev

    /**
      * Is this halfEdge - edge of the hole from hole's owner's side
      * O(leftFace.holeEdges) */
    def isHoleHalfSide: Boolean = leftFace.holesIncidentEdges.nonEmpty && leftFace.holesEdges.contains(this)

    def dest: Vertex[D] = twin.origin

    def data: D#HalfEdgeData = _data

    def data_=(value: D#HalfEdgeData): Unit = {
      _data = value
    }

    def nextAdjacent: HalfEdge[D] = twin.next

    def traverseEdges: Iterator[HalfEdge[D]] = new Iterator[HalfEdge[D]] {
      var cur: HalfEdge[D] = HalfEdge.this
      var first = true

      override def hasNext: Boolean = first || cur != HalfEdge.this

      override def next(): HalfEdge[D] = {
        val res = cur
        cur = cur.next
        first = false
        res
      }
    }

    def traverseAllReachableEdges(bfs: Boolean = true): Iterator[HalfEdge[D]] = new Iterator[HalfEdge[D]] {
      val visited: mutable.Set[HalfEdge[D]] = mutable.Set[HalfEdge[D]](HalfEdge.this)
      val queue = new mutable.ArrayDeque[HalfEdge[D]]()
      queue += HalfEdge.this

      @inline def addIfNotContains(h: HalfEdge[D]): Unit = if (!visited.contains(h)) {
        visited += h
        queue.append(h)
      }

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): HalfEdge[D] = {
        val res = if (bfs) queue.removeHead() else queue.removeLast()
        addIfNotContains(res.next)
        addIfNotContains(res.prev)
        addIfNotContains(res.twin)
        res
      }
    }

    def allReachableFaces(bfs: Boolean = true): Iterator[Face[D]] = traverseAllReachableEdges(bfs).map(_.leftFace).distinct

    def allReachableVertices(bfs: Boolean = true): Iterator[Vertex[D]] = traverseAllReachableEdges(bfs).map(_.origin).distinct

    override def toString: String = s"HE: $data $origin->$ending n: ${_next.data} p: ${_prev.data} t: ${_twin.data} f: ${leftFace.data}"
  }


  class Vertex[D <: DCELData] private[dcel](
                                             private[this] var _data: D#VertexData,
                                             private[dcel] var _incidentEdge: Option[HalfEdge[D]] = None
                                           ) {

    def incidentEdge: Option[HalfEdge[D]] = _incidentEdge

    def data: D#VertexData = _data

    def data_=(value: D#VertexData): Unit = {
      _data = value
    }

    def adjacentFaces(): Set[Face[D]] = edgesWithOriginHere.map(_.leftFace).toSet

    def edgesWithOriginHere: Iterator[HalfEdge[D]] =
      if (_incidentEdge.isEmpty) Iterator.empty
      else new Iterator[HalfEdge[D]] {
        val start: HalfEdge[D] = _incidentEdge.get
        var cur: HalfEdge[D] = _incidentEdge.get
        var first: Boolean = true

        override def hasNext: Boolean = first || cur != start

        override def next(): HalfEdge[D] = {
          val res = cur
          cur = cur.twin.next
          first = false
          res
        }
      }

    def edgesWithEndHere: Iterator[HalfEdge[D]] = edgesWithOriginHere.map(_.twin)

    def edgeTo(ot: Vertex[D]): Option[HalfEdge[D]] = {
      val res = edgesWithOriginHere.filter(_.ending == ot).toSeq
      res.size match {
        case 0 => None
        case 1 => res.headOption
        case 2 => throw new MultipleEdgesBetweenTwoVerticesException(this, ot)
      }
    }
    override def toString: String = s"V: $data iE: ${incidentEdge.map(_.data)}"

    def toLongString: String = toString + s" edges: ${edgesWithOriginHere.map(_.data).mkString("(", ",", ")")}"
  }
}

/** Double connected edge list */
class DCEL[D <: DCELData](
                           outerFaceData: D#FaceData
                         ) {

  val outerFace: Face[D] = new Face(outerFaceData, None, Set())

  val innerFaces: mutable.Set[Face[D]] = mutable.Set[Face[D]]()

  val halfEdges: mutable.Set[HalfEdge[D]] = mutable.Set[HalfEdge[D]]()

  val vertices: mutable.Set[Vertex[D]] = mutable.Set[Vertex[D]]()

  val onNewFace: Event[Face[D]] = new EventImpl[Face[D]]
  val onFaceRemoved: Event[Face[D]] = new EventImpl[Face[D]]

  val onNewHalfEdge: Event[HalfEdge[D]] = new EventImpl[HalfEdge[D]]
  val onHalfEdgeRemoved: Event[HalfEdge[D]] = new EventImpl[HalfEdge[D]]
  val onEdgeSplit: Event[(HalfEdge[D], HalfEdge[D])] = new EventImpl[(HalfEdge[D], HalfEdge[D])]

  val onNewVertex: Event[Vertex[D]] = new EventImpl[Vertex[D]]
  val onVertexRemoved: Event[Vertex[D]] = new EventImpl[Vertex[D]]


  def makeVertex(d: D#VertexData): Vertex[D] = {
    val res = new Vertex(d, None)
    vertices += res
    onNewVertex(res)
    res
  }

  def makeFace(f: D#FaceData): Face[D] = {
    val res = new Face[D](f, None, Set())
    innerFaces += res
    onNewFace(res)
    res
  }

  def makeFace(f: D#FaceData, edge: HalfEdge[D], redirectOldFaceTo: HalfEdge[D]): Face[D] = {
    val res = new Face[D](f, Some(edge), Set())
    innerFaces += res
    edge.traverseEdges.foreach { e =>
      if (e.leftFace.incidentEdge.contains(e)) {
        e.leftFace._incidentEdge = Some(redirectOldFaceTo)
      }
      if (e.leftFace._holesIncidentEdges.contains(e)) {
        e.leftFace._holesIncidentEdges -= e
        e.leftFace._holesIncidentEdges += redirectOldFaceTo
      }
      e._leftFace = res
    }
    onNewFace(res)
    res
  }

  def makeEdge(from: Vertex[D], to: Vertex[D], leftFace: Face[D], rightFace: Face[D], leftData: D#HalfEdgeData, rightData: D#HalfEdgeData,
               leftPrevOpt: Option[HalfEdge[D]] = None, leftNextOpt: Option[HalfEdge[D]] = None, rightPrevOpt: Option[HalfEdge[D]] = None, rightNextOpt: Option[HalfEdge[D]] = None
              ): HalfEdge[D] = {
    val leftMain = new HalfEdge(leftData, from, null, leftFace, null, null)
    val rightTwin = new HalfEdge(rightData, to, leftMain, rightFace, null, null)
    halfEdges += leftMain
    halfEdges += rightTwin

    leftMain._twin = rightTwin

    val leftPrev = leftPrevOpt.orElse(from.edgesWithEndHere.find(_.leftFace == leftFace))
    val leftNext = leftNextOpt.orElse(to.edgesWithOriginHere.find(_.leftFace == leftFace))
    val rightPrev = rightPrevOpt.orElse(to.edgesWithEndHere.find(_.leftFace == rightFace))
    val rightNext = rightNextOpt.orElse(from.edgesWithOriginHere.find(_.leftFace == rightFace))

    leftPrev match {
      case Some(prev) =>
        prev._next = leftMain
        leftMain._prev = prev
      case None =>
        leftMain._prev = rightTwin
    }

    leftNext match {
      case Some(next) =>
        next._prev = leftMain
        leftMain._next = next
      case None =>
        leftMain._next = rightTwin
    }
    rightPrev match {
      case Some(prev) =>
        rightTwin._prev = prev
        prev._next = rightTwin
      case None =>
        rightTwin._prev = leftMain
    }
    rightNext match {
      case Some(next) =>
        rightTwin._next = next
        next._prev = rightTwin
      case None =>
        rightTwin._next = leftMain
    }

    if (from.incidentEdge.isEmpty) from._incidentEdge = Some(leftMain)
    if (to.incidentEdge.isEmpty) to._incidentEdge = Some(rightTwin)
    if (outerFace != leftFace && leftFace.incidentEdge.isEmpty) leftFace._incidentEdge = Some(leftMain)
    if (outerFace != rightFace && rightFace.incidentEdge.isEmpty) rightFace._incidentEdge = Some(rightTwin)
    onNewHalfEdge(leftMain)
    onNewHalfEdge(rightTwin)
    leftMain
  }

  /** e and twin become shorter, creates new vertex and two half-edges */
  def split(oldEdge: HalfEdge[D], at: D#VertexData, newLeftData: D#HalfEdgeData, newRightData: D#HalfEdgeData): Vertex[D] = {
    val res = makeVertex(at)
    val newNext = new HalfEdge(newLeftData, res, null, oldEdge.leftFace, oldEdge, oldEdge._next)
    halfEdges += newNext

    val newNextTwin = new HalfEdge(newRightData, oldEdge.dest, newNext, oldEdge.twin.leftFace, oldEdge.twin.prev, oldEdge.twin)
    halfEdges += newNextTwin

    //we should update incidentEdge
    if (oldEdge.twin._origin.incidentEdge.contains(oldEdge.twin)) {
      oldEdge.twin._origin._incidentEdge =
        oldEdge.twin._origin.edgesWithOriginHere.filter(_ != oldEdge.twin).nextOption()
    }
    oldEdge.twin._origin = res

    //fix old next connection
    oldEdge.next._prev = newNext

    //fix old twin prev connection
    oldEdge.twin._prev._next = newNextTwin
    oldEdge.twin._prev = newNextTwin

    oldEdge._next = newNext

    res._incidentEdge = Some(newNext)
    newNext._twin = newNextTwin
    onNewHalfEdge(newNext)
    onNewHalfEdge(newNextTwin)
    onEdgeSplit((oldEdge, newNext))
    res
  }

  /** Removes links from faces and vertices to this edge in it's twin, where possible changes link to next, */
  private[dcel] def unparentEdge(e: HalfEdge[D]): Unit = {
    if (e.leftFace.incidentEdge.contains(e))
      e.leftFace._incidentEdge = Option.when(e.next != e)(e.next)
    if (e.leftFace.holesIncidentEdges.contains(e)) {
      e.leftFace._holesIncidentEdges -= e // e.leftFace.holesIncidentEdges.filter(_ != e)
      if (e.next != e) e.leftFace._holesIncidentEdges += e.next
    }

    if (e.twin.leftFace.incidentEdge.contains(e.twin))
      e.twin.leftFace._incidentEdge = Option.when(e.twin.next != e)(e.twin.next)
    if (e.twin.leftFace.holesIncidentEdges.contains(e.twin)) {
      e.twin.leftFace._holesIncidentEdges -= e.twin
      if (e.twin.next != e.twin) e.twin.leftFace._holesIncidentEdges += e.twin.next
    }

    if (e.origin.incidentEdge.contains(e))
      e.origin._incidentEdge = Option.when(e.nextAdjacent != e)(e.nextAdjacent)
    if (e.twin.origin.incidentEdge.contains(e.twin))
      e.twin.origin._incidentEdge = Option.when(e.twin.nextAdjacent != e.twin)(e.twin.nextAdjacent)
  }


  //  def collapseEdge(e: HalfEdge[D]) : Unit = {
  //    unparentEdge(e)
  //    e.prev._next = e.next
  //    e.next._prev = e.prev
  //    e.twin.prev._next = e.twin.next
  //    e.twin.next._prev = e.twin.prev
  //    halfEdges -= e
  //    halfEdges -= e.twin
  //    onHalfEdgeRemoved(e)
  //    onHalfEdgeRemoved(e.twin)
  //  }

  //todo check
  @deprecated("Since unchecked") def deleteEdge(e: HalfEdge[D]): Unit = {
    val f1 = e._leftFace
    val f2 = e.twin._leftFace
    if (f1 != f2) {
      if (f1 != outerFace && f2 != outerFace) {
        for (e <- f2.edges) e._leftFace = f1
        innerFaces -= f2
        onFaceRemoved(f2)
      } else if (f1 == outerFace) {
        for (e <- f2.edges) e._leftFace = f1
        innerFaces -= f2
        onFaceRemoved(f2)
      } else { //f2 == outerFace
        for (e <- f1.edges) e._leftFace = f2
        innerFaces -= f1
        onFaceRemoved(f1)
      }
    }
    deleteEdgeUnsafe(e)

  }

  /** Ignores possibility of merging faces */
  def deleteEdgeUnsafe(e: HalfEdge[D]): Unit = {
    unparentEdge(e)

    /*

    |                                  /\
    |e.prev                            |
    |                                  | e.next
    \/                  e              |
    *--------------------------------->*
    *<---------------------------------*
    |             e.twin               /\
    |                                  |
    |                                  |
    |                                  |e.twin.prev
    |e.twin.next                       |
    \/
    */

    e.prev._next = e._twin._next
    e.twin.next._prev = e.prev

    e._next._prev = e._twin._prev
    e.twin.prev._next = e.next

    halfEdges -= e
    halfEdges -= e.twin
    onHalfEdgeRemoved(e)
    onHalfEdgeRemoved(e.twin)
  }

  //todo test
  /**
    * This procedure assumes that toMerge can't be neighbour and hole at the same time
    * routine:
    * - mergeFaceDatas called once before merge
    * - face data on main updated
    * - then toMerge rewritten as main
    * - then for every edge
    * - - halfEdge detached
    * */
  def mergeAdjancedFaces(main: Face[D], toMerge: Face[D], mergeFaceDatas: MergeFaceDatas[D]): Boolean = {
    //todo remove hanging vertices
    if (main == toMerge) false
    else {
      val commonBorderEdges = main.edges.filter(_.twin.leftFace == toMerge).toSeq
      if (commonBorderEdges.nonEmpty) { //we're simple neighbours
        val isMergingWithHole = main.holesEdges.map(_.twin.leftFace).contains(toMerge)
        val isMainHole = toMerge.holesEdges.map(_.twin.leftFace).contains(main)

        val newFaceData = mergeFaceDatas.mergeFaceDatas(main, toMerge)
        main.data = newFaceData
        //          main._holesIncidentEdges = main._holesIncidentEdges.filter(_.twin.leftFace != toMerge) ++
        //            toMerge._holesIncidentEdges.filter(_.twin.leftFace != main)
        main._holesIncidentEdges = main._holesIncidentEdges ++ toMerge._holesIncidentEdges
        for (e <- toMerge.edges) e._leftFace = main
        if (isMainHole) main._incidentEdge = toMerge._incidentEdge

        innerFaces -= toMerge
        onFaceRemoved(toMerge)

        for (e <- if (isMainHole) commonBorderEdges.map(_.twin) else commonBorderEdges) {
          deleteEdgeUnsafe(e)
          //deletion of edge may create hole
          val next = e.next
          val prev = e.prev
          if (e.twin == next || e.twin == prev) { //removing end of some chain
            if (next == prev) { //removing single hanging edge
              main._holesIncidentEdges = main._holesIncidentEdges.filter(h => h != e && h != e.twin)
            } else {
              //do nothing
            }
          } else {
            val chainBroken = !next.traverseEdges.contains(prev)
            //              println(dcel.asInstanceOf[PlanarDCEL[D]].pos(e._origin), dcel.asInstanceOf[PlanarDCEL[D]].pos(e.ending))
            //              println(chainBroken, isMergingWithHole)
            if (chainBroken && !isMergingWithHole) { //if we created hole
              this match {
                case p: PlanarDCEL[D] => //ugly typecast
                  val nextPoly = PolygonRegion(next.traverseEdges.map(e => p.position(e.origin)).toSeq)
                  val prevPoly = PolygonRegion(prev.traverseEdges.map(e => p.position(e.origin)).toSeq)
                  if (nextPoly.contains(prevPoly)) {
                    main._holesIncidentEdges = main._holesIncidentEdges + prev
                    if (main.incidentEdge.isDefined && prev.traverseEdges.contains(main.incidentEdge.get)) main._incidentEdge = Some(next)
                  } else if (prevPoly.contains(nextPoly)) {
                    main._holesIncidentEdges = main._holesIncidentEdges + next
                    if (main.incidentEdge.isDefined && next.traverseEdges.contains(main.incidentEdge.get)) main._incidentEdge = Some(prev)
                  } else {
                    //todo
                  }
                case _ =>
                  main._holesIncidentEdges = main._holesIncidentEdges + (if (next.traverseEdges.contains(main.incidentEdge)) prev else next)
              }
            } else if (chainBroken && isMergingWithHole) { //we split hole to two parts
              if (!main.holesEdges.contains(prev)) main._holesIncidentEdges = main._holesIncidentEdges + prev
              if (!main.holesEdges.contains(next)) main._holesIncidentEdges = main._holesIncidentEdges + next
            } else if (!chainBroken && isMergingWithHole) {

            }
          }
          //            println(main._holesIncidentEdges.map(_.data))
        }

        true
      } else false
    }
  }


  /**Checking some invariants throw MalformedDCELException*/
  def sanityCheck() : Unit = {
    for(v <- vertices){
      for(e <- v.edgesWithOriginHere)
        if(e.origin != v) throw new MalformedDCELException(s"Edge $e with origin at $v does not starts here, starts at ${e.origin}")
    }

    for(e <- halfEdges){
      if(!vertices.contains(e.origin)) throw new MalformedDCELException(s"Vertices $vertices does not contains origin of $e - ${e.origin}" )
      if(!vertices.contains(e.ending)) throw new MalformedDCELException(s"Vertices $vertices does not contains origin of $e - ${e.origin}" )
      if(e.origin.incidentEdge.isEmpty) throw new MalformedDCELException(s"Vertex ${e.origin} have edge with origin $e but does not have incident edge ${e.origin.incidentEdge}")
      if(e.twin.origin.incidentEdge.isEmpty) throw new MalformedDCELException(s"Vertex ${e.origin} have edge with origin ${e.twin} but does not have incident edge ${e.twin.origin.incidentEdge}")
      if(!e.origin.edgesWithOriginHere.contains(e)) throw new MalformedDCELException(s"Edges starting at ${e.origin} - ${e.origin.edgesWithOriginHere} does not contains $e - edges starting at ${e.origin}")
      if(!e.origin.edgesWithEndHere.contains(e.twin)) throw new MalformedDCELException(s"Edges ending at ${e.origin} - ${e.origin.edgesWithEndHere} does not contains ${e.twin} - edges ending at ${e.origin} twin of $e starting here/")
    }

    for(f <- innerFaces | Set(outerFace)) {
      for(e <- f.edges) {
        if(e.leftFace != f) throw new MalformedDCELException(s"Edges $e left face ${e.leftFace} != $f face that edge belons to $e")
      }
    }


  }

}
