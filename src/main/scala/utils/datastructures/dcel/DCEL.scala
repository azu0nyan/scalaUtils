package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{RawFace, RawHalfEdge, RawVertex}
import utils.system.Event
import utils.system.Event.{Event, EventImpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DCEL {


  /**
    * @param _data               data associated with face
    * @param _incidentEdge       starting point to traverse in CCW order
    * @param _holesIncidentEdges own face's of edges that points to hole in face, one per hole
    *
    */
  class RawFace[VertexData, HalfEdgeData, FaceData] private[DCEL](
                                                                   private[this] var _data: FaceData,
                                                                   private[dcel] var _incidentEdge: Option[RawHalfEdge[VertexData, HalfEdgeData, FaceData]] = None,
                                                                   private[dcel] var _holesIncidentEdges: Set[RawHalfEdge[VertexData, HalfEdgeData, FaceData]],
                                                                 ) {


    type HalfEdge = RawHalfEdge[VertexData, HalfEdgeData, FaceData]
    type Vertex = RawVertex[VertexData, HalfEdgeData, FaceData]
    type Face = RawFace[VertexData, HalfEdgeData, FaceData]

    //    def parent:Option[Face] = incidentEdge.map(_.allReachableFaces().filter(_.holes.nonEmpty))

    def holesContours: Iterator[Iterator[HalfEdge]] = _holesIncidentEdges.iterator.map(_.traverseEdges)

    def data: FaceData = _data
    def data_=(value: FaceData): Unit = {
      _data = value
    }
    def incidentEdge: Option[HalfEdge] = _incidentEdge

    /** own face's of edges that points to hole in face, one per hole */
    def holesIncidentEdges: Set[HalfEdge] = _holesIncidentEdges

    def neighbourFaces: Iterator[Face] = edges.map(_.twin.leftFace).distinct

    def outsideVertices: Iterator[Vertex] = incidentEdge.map(_.traverseEdges.map(_.origin)).getOrElse(Iterator.empty)
    def borderEdges: Iterator[HalfEdge] = incidentEdge.map(_.traverseEdges).getOrElse(Iterator.empty)

    def holesVertices: Iterator[Vertex] = _holesIncidentEdges.iterator.flatMap(_.traverseEdges.map(_.origin))
    /** all own edges adjanced to holes */
    def holesEdges: Iterator[HalfEdge] = _holesIncidentEdges.iterator.flatMap(_.traverseEdges)


    def vertices: Iterator[Vertex] = outsideVertices ++ holesVertices
    def edges: Iterator[HalfEdge] = borderEdges ++ holesEdges
    /** requires O(twin.holeEdges) */
    def isHole: Boolean = incidentEdge.exists(_.twin.isHoleHalfSide)
  }

  /**
    * @param _data    data associated with half-edge
    * @param origin   start
    * @param twin     opposite half-edge
    * @param leftFace face to its left
    * @param next     next half‐edge on the boundary of incidentFace
    * @param prev     previous half-edge
    */
  class RawHalfEdge[VertexData, HalfEdgeData, FaceData] private[dcel](
                                                                       private[dcel] var _data: HalfEdgeData,
                                                                       private[dcel] var _origin: RawVertex[VertexData, HalfEdgeData, FaceData],
                                                                       private[dcel] var _twin: RawHalfEdge[VertexData, HalfEdgeData, FaceData],
                                                                       private[dcel] var _leftFace: RawFace[VertexData, HalfEdgeData, FaceData],
                                                                       private[dcel] var _prev: RawHalfEdge[VertexData, HalfEdgeData, FaceData],
                                                                       private[dcel] var _next: RawHalfEdge[VertexData, HalfEdgeData, FaceData],
                                                                     ) {


    type HalfEdge = RawHalfEdge[VertexData, HalfEdgeData, FaceData]
    type Vertex = RawVertex[VertexData, HalfEdgeData, FaceData]
    type Face = RawFace[VertexData, HalfEdgeData, FaceData]

    def origin: Vertex = _origin
    def ending: Vertex = _next.origin

    def twin: HalfEdge = _twin


    def leftFace: Face = _leftFace
    def next: HalfEdge = _next
    def prev: HalfEdge = _prev
    /**
      * Is this halfEdge - edge of the hole from hole's owner's side
      * O(leftFace.holeEdges) */
    def isHoleHalfSide: Boolean = leftFace.holesIncidentEdges.nonEmpty && leftFace.holesEdges.contains(this)

    def dest: Vertex = twin.origin

    def data: HalfEdgeData = _data
    def data_=(value: HalfEdgeData): Unit = {
      _data = value
    }

    def nextAdjacent: HalfEdge = twin.next

    def traverseEdges: Iterator[HalfEdge] = new Iterator[HalfEdge] {
      var cur: HalfEdge = RawHalfEdge.this
      var first = true
      override def hasNext: Boolean = first || cur != RawHalfEdge.this
      override def next(): HalfEdge = {
        val res = cur
        cur = cur.next
        first = false
        res
      }
    }

    def traverseAllReachableEdges(bfs: Boolean = true): Iterator[HalfEdge] = new Iterator[HalfEdge] {
      val visited: mutable.Set[HalfEdge] = mutable.Set[HalfEdge](RawHalfEdge.this)
      val queue = new mutable.ArrayDeque[HalfEdge]()
      queue += RawHalfEdge.this

      @inline def addIfNotContains(h: HalfEdge): Unit = if (!visited.contains(h)) {
        visited += h
        queue.append(h)
      }

      override def hasNext: Boolean = queue.nonEmpty
      override def next(): HalfEdge = {
        val res = if (bfs) queue.removeHead() else queue.removeLast()
        addIfNotContains(res.next)
        addIfNotContains(res.prev)
        addIfNotContains(res.twin)
        res
      }
    }

    def allReachableFaces(bfs: Boolean = true): Iterator[Face] = traverseAllReachableEdges(bfs).map(_.leftFace).distinct

    def allReachableVertices(bfs: Boolean = true): Iterator[Vertex] = traverseAllReachableEdges(bfs).map(_.origin).distinct
  }


  class RawVertex[VertexData, HalfEdgeData, FaceData] private[dcel](
                                                                     private[this] var _data: VertexData,
                                                                     private[dcel] var _incidentEdge: Option[RawHalfEdge[VertexData, HalfEdgeData, FaceData]] = None
                                                                   ) {

    def incidentEdge: Option[RawHalfEdge[VertexData, HalfEdgeData, FaceData]] = _incidentEdge

    type HalfEdge = RawHalfEdge[VertexData, HalfEdgeData, FaceData]
    type Vertex = RawVertex[VertexData, HalfEdgeData, FaceData]
    type Face = RawFace[VertexData, HalfEdgeData, FaceData]

    def data: VertexData = _data
    def data_=(value: VertexData): Unit = {
      _data = value
    }

    def adjacentFaces(): Set[Face] = edgesWithOriginHere.map(_.leftFace).toSet

    def edgesWithOriginHere: Iterator[HalfEdge] =
      if (_incidentEdge.isEmpty) Iterator.empty
      else new Iterator[HalfEdge] {
        val start: HalfEdge = _incidentEdge.get
        var cur: HalfEdge = _incidentEdge.get
        var first: Boolean = true

        override def hasNext: Boolean = first || cur != start
        override def next(): HalfEdge = {
          val res = cur
          cur = cur.twin.next
          first = false
          res
        }
      }

    def edgesWithEndHere: Iterator[HalfEdge] = edgesWithOriginHere.map(_.twin)
  }
}

/** Double connected edge list */
class DCEL[VertexData, HalfEdgeData, FaceData](
                                                outerFaceData: FaceData
                                              ) {
  type Dcel = DCEL[VertexData, HalfEdgeData, FaceData]
  type HalfEdge = RawHalfEdge[VertexData, HalfEdgeData, FaceData]
  type Vertex = RawVertex[VertexData, HalfEdgeData, FaceData]
  type Face = RawFace[VertexData, HalfEdgeData, FaceData]

  val outerFace = new Face(outerFaceData, None, Set())

  val innerFaces: mutable.Set[Face] = mutable.Set[Face]()

  val halfEdges: mutable.Set[HalfEdge] = mutable.Set[HalfEdge]()

  val vertices: mutable.Set[Vertex] = mutable.Set[Vertex]()

  val onNewFace: Event[Face] = new EventImpl[Face]
  val onFaceDelete: Event[Face] = new EventImpl[Face]

  val onNewHalfEdge: Event[HalfEdge] = new EventImpl[HalfEdge]
  val onHalfEdgeRemoved: Event[HalfEdge] = new EventImpl[HalfEdge]
  val onEdgeSplit: Event[(HalfEdge, HalfEdge)] = new EventImpl[(HalfEdge, HalfEdge)]

  val onNewVertex: Event[Vertex] = new EventImpl[Vertex]
  val onVertexDelete: Event[Vertex] = new EventImpl[Vertex]


  def makeVertex(d: VertexData): Vertex = {
    val res = new Vertex(d, None)
    vertices += res
    onNewVertex(res)
    res
  }

  def makeFace(f: FaceData): Face = {
    val res = new Face(f, None, Set())
    innerFaces += res
    onNewFace(res)
    res
  }

  def makeFace(f: FaceData, edge: HalfEdge, redirectOldFaceTo: HalfEdge): Face = {
    val res = new Face(f, Some(edge), Set())
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

  def makeEdge(from: Vertex, to: Vertex, leftFace: Face, rightFace: Face, leftData: HalfEdgeData, rightData: HalfEdgeData,
               leftPrevOpt: Option[HalfEdge] = None, leftNextOpt: Option[HalfEdge] = None, rightPrevOpt: Option[HalfEdge] = None, rightNextOpt: Option[HalfEdge] = None
              ): HalfEdge = {
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
  def split(oldEdge: HalfEdge, at: VertexData, newLeftData: HalfEdgeData, newRightData: HalfEdgeData): Vertex = {
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

  /** Removes links from faces to this edge in it's twin, where possible changes link to next, */
  private def unparentEdge(e: HalfEdge) = {
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

  def collapseEdge(e: HalfEdge): Unit = {
    unparentEdge(e)
    e.prev._next = e.next
    e.next._prev = e.prev
    e.twin.prev._next = e.twin.next
    e.twin.next._prev = e.twin.prev
    halfEdges -= e
    halfEdges -= e.twin
    onHalfEdgeRemoved(e)
    onHalfEdgeRemoved(e.twin)
  }


  /** Ignores possibility of merging faces */
  def deleteEdgeUnsafe(e: HalfEdge): Unit = {
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


}
