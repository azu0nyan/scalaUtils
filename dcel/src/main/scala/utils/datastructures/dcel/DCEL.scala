package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL._
import utils.math.planar.PolygonRegion
import utils.system.Event.{Event, EventImpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DCEL {
  class DCELException(text: String) extends RuntimeException(text)

  class MalformedDCELException(text: String) extends DCELException(text)

  class MultipleEdgesBetweenTwoVerticesException(v1: Vertex[?, ?, ?], v2: Vertex[?, ?, ?]) extends MalformedDCELException(
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
  class Face[VD, HD, FD] private[DCEL](
                                        private[this] var _data: FD,
                                        private[dcel] var _incidentEdge: Option[HalfEdge[VD, HD, FD]] = None,
                                        private[dcel] var _holesIncidentEdges: Set[HalfEdge[VD, HD, FD]],
                                      ) {

    //    def parent:Option[Face[VD, HD, FD]] = incidentEdge.map(_.allReachableFaces().filter(_.holes.nonEmpty))

    def holesContours: Iterator[Iterator[HalfEdge[VD, HD, FD]]] = _holesIncidentEdges.iterator.map(_.traverseEdges)

    def data: FD = _data

    def data_=(value: FD): Unit = {
      _data = value
    }

    def incidentEdge: Option[HalfEdge[VD, HD, FD]] = _incidentEdge

    /** own face's of edges that points to hole in face, one per hole */
    def holesIncidentEdges: Set[HalfEdge[VD, HD, FD]] = _holesIncidentEdges

    def neighbourFaces: Iterator[Face[VD, HD, FD]] = edges.map(_.twin.leftFace).distinct

    def outsideVertices: Iterator[Vertex[VD, HD, FD]] = incidentEdge.map(_.traverseEdges.map(_.origin)).getOrElse(Iterator.empty)
    /** Edges that belongs to face's border, not hole edges */
    def borderEdges: Iterator[HalfEdge[VD, HD, FD]] = incidentEdge.map(_.traverseEdges).getOrElse(Iterator.empty)

    def holesVertices: Iterator[Vertex[VD, HD, FD]] = _holesIncidentEdges.iterator.flatMap(_.traverseEdges.map(_.origin))

    /** all own edges adjanced to holes */
    def holesEdges: Iterator[HalfEdge[VD, HD, FD]] = _holesIncidentEdges.iterator.flatMap(_.traverseEdges)

    def holes: Iterator[Face[VD, HD, FD]] = _holesIncidentEdges.iterator.map(_.twin._leftFace)

    def vertices: Iterator[Vertex[VD, HD, FD]] = outsideVertices ++ holesVertices

    /** Includes border edges and hole edges */
    def edges: Iterator[HalfEdge[VD, HD, FD]] = borderEdges ++ holesEdges

    /** If forEdge is in hole border, finds holeIncidentEdge that refers to forEdge's hole */
    def holeIncidentEdge(forEdge: HalfEdge[VD, HD, FD]): Option[HalfEdge[VD, HD, FD]] = _holesIncidentEdges.find(_.traverseEdges.contains(forEdge))

    /** requires O(twin.holeEdges) */
    def isHole: Boolean = incidentEdge.exists(_.twin.isHoleHalfSide)


    /** Hole can consist from several adjanced faces, returns all faces that reachable from holeIncidentEdges exept this */
    def allHoleFaces: Seq[Face[VD, HD, FD]] = {
      val faces = mutable.Set[Face[VD, HD, FD]]()
      for (hIE <- holesIncidentEdges;
           e <- hIE.traverseAllReachableEdges() if e.leftFace != this) {
        faces += e.leftFace
      }
      println(faces)
      faces.toSeq
    }

    override def toString: String = s"F: ${_data} iE: ${incidentEdge.map(_.data)} holes: ${holes.toSeq.map(_.data)}"
  }

  /**
   * @param _data    data associated with half-edge
   * @param origin   start
   * @param twin     opposite half-edge
   * @param leftFace [VD, HD, FD]  face to its left
   * @param next     next half‐edge on the boundary of incidentFace
   * @param prev     previous half-edge
   */
  class HalfEdge[VD, HD, FD] private[dcel](
                                            private[dcel] var _data: HD,
                                            private[dcel] var _origin: Vertex[VD, HD, FD],
                                            private[dcel] var _twin: HalfEdge[VD, HD, FD],
                                            private[dcel] var _leftFace: Face[VD, HD, FD],
                                            private[dcel] var _prev: HalfEdge[VD, HD, FD],
                                            private[dcel] var _next: HalfEdge[VD, HD, FD],
                                          ) {
    def origin: Vertex[VD, HD, FD] = _origin

    def ending: Vertex[VD, HD, FD] = _next.origin

    def twin: HalfEdge[VD, HD, FD] = _twin


    def leftFace: Face[VD, HD, FD] = _leftFace

    def next: HalfEdge[VD, HD, FD] = _next

    def prev: HalfEdge[VD, HD, FD] = _prev

    /**
     * Is this halfEdge - edge of the hole from hole's owner's side
     * O(leftFace.holeEdges) */
    def isHoleHalfSide: Boolean = leftFace.holesIncidentEdges.nonEmpty && leftFace.holesEdges.contains(this)

    def dest: Vertex[VD, HD, FD] = twin.origin

    def data: HD = _data

    def data_=(value: HD): Unit = {
      _data = value
    }

    def nextAdjacent: HalfEdge[VD, HD, FD] = twin.next

    def traverseEdges: Iterator[HalfEdge[VD, HD, FD]] = new Iterator[HalfEdge[VD, HD, FD]] {
      var cur: HalfEdge[VD, HD, FD] = HalfEdge.this
      var first = true

      override def hasNext: Boolean = first || cur != HalfEdge.this

      override def next(): HalfEdge[VD, HD, FD] = {
        val res = cur
        cur = cur.next
        first = false
        res
      }
    }

    def traverseAllReachableEdges(bfs: Boolean = true): Iterator[HalfEdge[VD, HD, FD]] = new Iterator[HalfEdge[VD, HD, FD]] {
      val visited: mutable.Set[HalfEdge[VD, HD, FD]] = mutable.Set[HalfEdge[VD, HD, FD]](HalfEdge.this)
      val queue = new mutable.ArrayDeque[HalfEdge[VD, HD, FD]]()
      queue += HalfEdge.this

      @inline def addIfNotContains(h: HalfEdge[VD, HD, FD]): Unit = if (!visited.contains(h)) {
        visited += h
        queue.append(h)
      }

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): HalfEdge[VD, HD, FD] = {
        val res = if (bfs) queue.removeHead() else queue.removeLast()
        addIfNotContains(res.next)
        addIfNotContains(res.prev)
        addIfNotContains(res.twin)
        res
      }
    }

    def allReachableFaces(bfs: Boolean = true): Iterator[Face[VD, HD, FD]] = traverseAllReachableEdges(bfs).map(_.leftFace).distinct

    def allReachableVertices(bfs: Boolean = true): Iterator[Vertex[VD, HD, FD]] = traverseAllReachableEdges(bfs).map(_.origin).distinct

    override def toString: String = s"HE: $data $origin->$ending n: ${_next.data} p: ${_prev.data} t: ${_twin.data} f: ${leftFace.data}"
  }


  class Vertex[VD, HD, FD] private[dcel](
                                          private[this] var _data: VD,
                                          private[dcel] var _incidentEdge: Option[HalfEdge[VD, HD, FD]] = None
                                        ) {

    def incidentEdge: Option[HalfEdge[VD, HD, FD]] = _incidentEdge

    def data: VD = _data

    def data_=(value: VD): Unit = {
      _data = value
    }

    def adjacentFaces(): Set[Face[VD, HD, FD]] = edgesWithOriginHere.map(_.leftFace).toSet

    def edgesWithOriginHere: Iterator[HalfEdge[VD, HD, FD]] =
      if (_incidentEdge.isEmpty) Iterator.empty
      else new Iterator[HalfEdge[VD, HD, FD]] {
        val start: HalfEdge[VD, HD, FD] = _incidentEdge.get
        var cur: HalfEdge[VD, HD, FD] = _incidentEdge.get
        var first: Boolean = true

        override def hasNext: Boolean = first || cur != start

        override def next(): HalfEdge[VD, HD, FD] = {
          val res = cur
          cur = cur.twin.next
          first = false
          res
        }
      }

    def edgesWithEndHere: Iterator[HalfEdge[VD, HD, FD]] = edgesWithOriginHere.map(_.twin)

    def edgeTo(ot: Vertex[VD, HD, FD]): Option[HalfEdge[VD, HD, FD]] = {
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
class DCEL[VD, HD, FD](
                        outerFaceData: FD
                      ) {

  val outerFace: Face[VD, HD, FD] = new Face(outerFaceData, None, Set())

  val innerFaces: mutable.Set[Face[VD, HD, FD]] = mutable.Set[Face[VD, HD, FD]]()

  val halfEdges: mutable.Set[HalfEdge[VD, HD, FD]] = mutable.Set[HalfEdge[VD, HD, FD]]()

  val vertices: mutable.Set[Vertex[VD, HD, FD]] = mutable.Set[Vertex[VD, HD, FD]]()

  val onNewFace: Event[Face[VD, HD, FD]] = new EventImpl[Face[VD, HD, FD]]
  val onFaceRemoved: Event[Face[VD, HD, FD]] = new EventImpl[Face[VD, HD, FD]]

  val onNewHalfEdge: Event[HalfEdge[VD, HD, FD]] = new EventImpl[HalfEdge[VD, HD, FD]]
  val onHalfEdgeRemoved: Event[HalfEdge[VD, HD, FD]] = new EventImpl[HalfEdge[VD, HD, FD]]
  val onEdgeSplit: Event[(HalfEdge[VD, HD, FD], HalfEdge[VD, HD, FD])] = new EventImpl[(HalfEdge[VD, HD, FD], HalfEdge[VD, HD, FD])]

  val onNewVertex: Event[Vertex[VD, HD, FD]] = new EventImpl[Vertex[VD, HD, FD]]
  val onVertexRemoved: Event[Vertex[VD, HD, FD]] = new EventImpl[Vertex[VD, HD, FD]]


  def makeVertex(d: VD): Vertex[VD, HD, FD] = {
    val res = new Vertex[VD, HD, FD](d, None)
    vertices += res
    onNewVertex(res)
    res
  }

  def makeFace(f: FD): Face[VD, HD, FD] = {
    val res = new Face[VD, HD, FD](f, None, Set())
    innerFaces += res
    onNewFace(res)
    res
  }

  def makeFace(f: FD, edge: HalfEdge[VD, HD, FD], redirectOldFaceTo: HalfEdge[VD, HD, FD]): Face[VD, HD, FD] = {
    val res = new Face[VD, HD, FD](f, Some(edge), Set())
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

  def makeEdge(from: Vertex[VD, HD, FD], to: Vertex[VD, HD, FD], leftFace: Face[VD, HD, FD], rightFace: Face[VD, HD, FD], leftData: HD, rightData: HD,
               leftPrevOpt: Option[HalfEdge[VD, HD, FD]] = None, leftNextOpt: Option[HalfEdge[VD, HD, FD]] = None, rightPrevOpt: Option[HalfEdge[VD, HD, FD]] = None, rightNextOpt: Option[HalfEdge[VD, HD, FD]] = None
              ): HalfEdge[VD, HD, FD] = {
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

  /** e and twin become shorter, creates new vertex and two half-edges, makes new vertex */
  def split(oldEdge: HalfEdge[VD, HD, FD], at: VD, newLeftData: HD, newRightData: HD): Vertex[VD, HD, FD] = {
    /*

        oldEdge
    *------------------------------------------------------>*
    *<------------------------------------------------------*
                  oldEdge.twin

        oldEdge               res      newNext
    *------------------------>*----------------------------->*
    *<------------------------*<-----------------------------*
        oldEdge.twin                  newNext.twin
     */

    val res = makeVertex(at)
    val newNext = new HalfEdge(newLeftData, res, null, oldEdge.leftFace, oldEdge, oldEdge._next)
    halfEdges += newNext

    val newNextTwin = new HalfEdge(newRightData, oldEdge.dest, newNext, oldEdge.twin.leftFace, oldEdge.twin.prev, oldEdge.twin)
    halfEdges += newNextTwin

    newNext._twin = newNextTwin
    //as for now old edge and old edge.twin unchanged
    //we should update incidentEdge
    if (oldEdge.twin._origin.incidentEdge.contains(oldEdge.twin)) {
      oldEdge.twin._origin._incidentEdge = Some(newNextTwin)
      //oldEdge.twin._origin.edgesWithOriginHere.filter(_ != oldEdge.twin).nextOption()//!!!!!!!!!!
    }
    oldEdge.twin._origin = res

    //fix old next connection
    oldEdge.next._prev = newNext
    oldEdge._next = newNext

    //fix old twin prev connection
    oldEdge.twin._prev._next = newNextTwin
    oldEdge.twin._prev = newNextTwin


    res._incidentEdge = Some(newNext)

    onNewHalfEdge(newNext)
    onNewHalfEdge(newNextTwin)
    onEdgeSplit((oldEdge, newNext))
    res
  }

  /** Removes links from faces and vertices to this edge in it's twin, where possible changes link to next, */
  private[dcel] def unparentEdge(e: HalfEdge[VD, HD, FD]): Unit = {
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


  //  def collapseEdge(e: HalfEdge[VD, HD, FD]) : Unit = {
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
  @deprecated("Since unchecked") def deleteEdge(e: HalfEdge[VD, HD, FD]): Unit = {
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
  def deleteEdgeUnsafe(e: HalfEdge[VD, HD, FD]): Unit = {
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
  def mergeAdjancedFaces(main: Face[VD, HD, FD], toMerge: Face[VD, HD, FD], mergeFaceDatas: MergeFaceDatas[VD, HD, FD]): Boolean = {
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
            //              println(dcel.asInstanceOf[PlanarDCEL[VD, HD, FD]].pos(e._origin), dcel.asInstanceOf[PlanarDCEL[VD, HD, FD]].pos(e.ending))
            //              println(chainBroken, isMergingWithHole)
            if (chainBroken && !isMergingWithHole) { //if we created hole
              this match {
                case p: PlanarDCEL[VD, HD, FD] => //ugly typecast
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


  /** Checking some invariants throw MalformedDCELException */
  def sanityCheck(): Unit = {
    for (v <- vertices) {
      for (e <- v.edgesWithOriginHere)
        if (e.origin != v) throw new MalformedDCELException(s"Edge $e with origin at $v does not starts here, starts at ${e.origin}")
    }

    for (e <- halfEdges) {
      if (!vertices.contains(e.origin)) throw new MalformedDCELException(s"Vertices $vertices does not contains origin of $e - ${e.origin}")
      if (!vertices.contains(e.ending)) throw new MalformedDCELException(s"Vertices $vertices does not contains origin of $e - ${e.origin}")
      if (e.origin.incidentEdge.isEmpty) throw new MalformedDCELException(s"Vertex ${e.origin} have edge with origin $e but does not have incident edge ${e.origin.incidentEdge}")
      if (e.twin.origin.incidentEdge.isEmpty) throw new MalformedDCELException(s"Vertex ${e.origin} have edge with origin ${e.twin} but does not have incident edge ${e.twin.origin.incidentEdge}")
      if (!e.origin.edgesWithOriginHere.contains(e)) throw new MalformedDCELException(s"Edges starting at ${e.origin} - ${e.origin.edgesWithOriginHere} does not contains $e - edges starting at ${e.origin}")
      if (!e.origin.edgesWithEndHere.contains(e.twin)) throw new MalformedDCELException(s"Edges ending at ${e.origin} - ${e.origin.edgesWithEndHere} does not contains ${e.twin} - edges ending at ${e.origin} twin of $e starting here/")
    }

    for (f <- innerFaces | Set(outerFace)) {
      for (e <- f.edges) {
        if (e.leftFace != f) throw new MalformedDCELException(s"Edges $e left face ${e.leftFace} != $f face that edge belons to $e")
      }
    }


  }


  override def toString = s"DCEL($outerFace, $innerFaces, $halfEdges, $vertices)"

  def toLongSting = s"DCEL:\n" +
    s"outerFace: $outerFace" + "\n" +
    vertices.mkString("\n") + "\n" +
    halfEdges.mkString("\n") + "\n" +
    innerFaces.mkString("\n")
}
