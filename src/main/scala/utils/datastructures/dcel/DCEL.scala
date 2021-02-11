package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{RawFace, RawHalfEdge, RawVertex}
import utils.system.Event
import utils.system.Event.{Event, EventImpl}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
object DCEL{


  /**
   * @param _data        data associated with face
   * @param incidentEdge starting point to traverse in CCW order
   */
  class RawFace[VertexData, HalfEdgeData, FaceData] private[DCEL](
                            private[this] var _data: FaceData,
                            private[dcel] var _incidentEdge: Option[RawHalfEdge[VertexData, HalfEdgeData, FaceData]] = None
                          ) {
    type HalfEdge = RawHalfEdge[VertexData, HalfEdgeData, FaceData]
    type Vertex = RawVertex[VertexData, HalfEdgeData, FaceData]
    type Face = RawFace[VertexData, HalfEdgeData, FaceData]

    def data: FaceData = _data
    def data_=(value: FaceData): Unit = {
      _data = value
    }
    def incidentEdge: Option[HalfEdge] = _incidentEdge


    def vertices: Iterator[Vertex] = incidentEdge.map(_.traverseEdges.map(_.origin)).getOrElse(Iterator.empty)
    def edges: Iterator[HalfEdge] = incidentEdge.map(_.traverseEdges).getOrElse(Iterator.empty)
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
  }


  class RawVertex[VertexData, HalfEdgeData, FaceData] private[dcel](
                              private[this] var _data: VertexData,
                              private[dcel] var incidentEdge: Option[RawHalfEdge[VertexData, HalfEdgeData, FaceData]] = None
                            ) {
    type HalfEdge = RawHalfEdge[VertexData, HalfEdgeData, FaceData]
    type Vertex = RawVertex[VertexData, HalfEdgeData, FaceData]
    type Face = RawFace[VertexData, HalfEdgeData, FaceData]

    def data: VertexData = _data
    def data_=(value: VertexData): Unit = {
      _data = value
    }

    def adjacentFaces(): Set[Face] = edgesWithOriginHere.map(_.leftFace).toSet

    def edgesWithOriginHere: Iterator[HalfEdge] =
      if (incidentEdge.isEmpty) Iterator.empty
      else new Iterator[HalfEdge] {
        val start: HalfEdge = incidentEdge.get
        var cur: HalfEdge = incidentEdge.get
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
  type HalfEdge = RawHalfEdge[VertexData, HalfEdgeData, FaceData]
  type Vertex = RawVertex[VertexData, HalfEdgeData, FaceData]
  type Face = RawFace[VertexData, HalfEdgeData, FaceData]

  val outerFace = new Face(outerFaceData, None)

  val innerFaces: mutable.Set[Face] = mutable.Set[Face]()

  val halfEdges: mutable.Set[HalfEdge] = mutable.Set[HalfEdge]()

  val vertices: mutable.Set[Vertex] = mutable.Set[Vertex]()

  val onNewFace: Event[Face] = new EventImpl[Face]
  val onNewEdge: Event[HalfEdge] = new EventImpl[HalfEdge]
  val onNewVertex: Event[Vertex] = new EventImpl[Vertex]
  val onEdgeSplit: Event[(HalfEdge, HalfEdge)] = new EventImpl[(HalfEdge, HalfEdge)]
  val onEdgeCollapse: Event[HalfEdge] = new EventImpl[HalfEdge]


  def makeVertex(d: VertexData): Vertex = {
    val res = new Vertex(d, None)
    vertices += res
    onNewVertex(res)
    res
  }

  def makeFace(f: FaceData): Face = {
    val res = new Face(f, None)
    innerFaces += res
    onNewFace(res)
    res
  }

  def makeFace(f: FaceData, edge: HalfEdge): Face = {
    val res = new Face(f, Some(edge))
    innerFaces += res
    edge.traverseEdges.foreach(e => e._leftFace = res)
    onNewFace(res)
    res
  }

  def makeEdge(from: Vertex, to: Vertex, leftFace: Face, rightFace: Face, leftData: HalfEdgeData, rightData: HalfEdgeData): HalfEdge = {
    val main = new HalfEdge(leftData, from, null, leftFace, null, null)
    val twin = new HalfEdge(rightData, to, main, rightFace, null, null)
    halfEdges += main
    halfEdges += twin

    main._twin = twin

    val fPrev = from.edgesWithEndHere.find(_.leftFace == leftFace)
    val fNext = to.edgesWithOriginHere.find(_.leftFace == leftFace)
    val sPrev = to.edgesWithEndHere.find(_.leftFace == rightFace)
    val sNext = from.edgesWithOriginHere.find(_.leftFace == rightFace)

    fPrev match {
      case Some(prev) =>
        prev._next = main
        main._prev = prev
      case None =>
        main._prev = twin
    }

    fNext match {
      case Some(next) =>
        next._prev = main
        main._next = next
      case None =>
        main._next = twin
    }
    sPrev match {
      case Some(prev) =>
        twin._prev = prev
        prev._next = twin
      case None =>
        twin._prev = main
    }
    sNext match {
      case Some(next) =>
        twin._next = next
        next._prev = twin
      case None =>
        twin._next = main
    }

    if (from.incidentEdge.isEmpty) from.incidentEdge = Some(main)
    if (to.incidentEdge.isEmpty) to.incidentEdge = Some(twin)
    if (leftFace.incidentEdge.isEmpty) leftFace._incidentEdge = Some(main)
    if (rightFace.incidentEdge.isEmpty) rightFace._incidentEdge = Some(twin)
    onNewEdge(main)
    main
  }

  /** e and twin become shorter, creates new vertex and two half-edges */
  def split(oldEdge: HalfEdge, at: VertexData, newLeftData: HalfEdgeData, newRightData: HalfEdgeData): Vertex = {
    val res = makeVertex(at)
    onNewVertex(res)
    val newNext = new HalfEdge(newLeftData, res, null, oldEdge.leftFace, oldEdge, oldEdge._next)
    halfEdges += newNext
    oldEdge._next = newNext

    val newNextTwin = new HalfEdge(newRightData, oldEdge.dest, newNext, oldEdge.twin.leftFace, oldEdge.twin.prev, oldEdge.twin)
    halfEdges += newNextTwin
    oldEdge.twin._prev = newNextTwin

    res.incidentEdge = Some(newNext)
    newNext._twin = newNextTwin
//    onNewEdge(newNext) //todo enable
    onEdgeSplit((oldEdge, newNext))
    res
  }


  def collapseEdge(e: HalfEdge): Unit = {
    if (e.leftFace.incidentEdge.contains(e))
      e.leftFace._incidentEdge = Option.when(e.next != e)(e.next)
    if (e.twin.leftFace.incidentEdge.contains(e.twin))
      e.twin.leftFace._incidentEdge = Option.when(e.twin.next != e)(e.twin.next)
    if (e.origin.incidentEdge.contains(e))
      e.origin.incidentEdge = Option.when(e.nextAdjacent != e)(e.nextAdjacent)
    if (e.twin.origin.incidentEdge.contains(e.twin))
      e.twin.origin.incidentEdge = Option.when(e.twin.nextAdjacent != e.twin)(e.twin.nextAdjacent)
    e.prev._next = e.next
    e.next._prev = e.prev
    e.twin.prev._next = e.twin.next
    e.twin.next._prev = e.twin.prev
    halfEdges -= e
    halfEdges -= e.twin
    onEdgeCollapse(e)
  }


}
