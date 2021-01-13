package utils.datastructures.dcel

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/** Double connected edge list */
class DCEL[VertexData, HalfEdgeData, FaceData](
                                                outerFaceData: FaceData
                                              ) {

  class Vertex private[dcel](
                              private[this] var _data: VertexData,
                              private[dcel] var incidentEdge: Option[HalfEdge] = None
                            ) {
    def data: VertexData = _data
    def data_=(value: VertexData): Unit = {
      _data = value
    }

    def edgesWithOriginHere: Iterator[HalfEdge] =
      if (incidentEdge.isEmpty) Iterator.empty
      else new Iterator[HalfEdge] {
        val start: HalfEdge = incidentEdge.get
        var cur: HalfEdge = incidentEdge.get
        var first: Boolean = false

        override def hasNext: Boolean = first | cur != start
        override def next(): HalfEdge = {
          val res = cur
          cur = cur.twin.next
          first = false
          res
        }
      }

    def edgesWithEndHere: Iterator[HalfEdge] = edgesWithOriginHere.map(_.twin)
  }

  /**
   * @param _data    data associated with half-edge
   * @param origin   start
   * @param twin     opposite half-edge
   * @param leftFace face to its left
   * @param next     next half‐edge on the boundary of incidentFace
   * @param prev     previous half-edge
   */
  class HalfEdge private[dcel](
                                private[this] var _data: HalfEdgeData,
                                private[dcel] var origin: Vertex,
                                private[dcel] var twin: HalfEdge,
                                private[dcel] var leftFace: Face,
                                private[dcel] var next: HalfEdge,
                                private[dcel] var prev: HalfEdge,
                              ) {

    def dest: Vertex = twin.origin

    def data: HalfEdgeData = _data
    def data_=(value: HalfEdgeData): Unit = {
      _data = value
    }

    def nextAdjacent: HalfEdge = twin.next

    def traverseEdges: Iterator[HalfEdge] = new Iterator[HalfEdge] {
      var cur: HalfEdge = HalfEdge.this
      var first = false
      override def hasNext: Boolean = !first || cur != HalfEdge.this
      override def next(): HalfEdge = {
        val res = cur
        cur = cur.next
        first = false
        res
      }
    }
  }

  /**
   * @param _data        data associated with face
   * @param incidentEdge starting point to traverse in CCW order
   */
  class Face private[DCEL](
                            private[this] var _data: FaceData ,
                            private[dcel] var incidentEdge: Option[HalfEdge] = None
                          ) {
    def data: FaceData = _data
    def data_=(value: FaceData): Unit = {
      _data = value
    }

    def vertices:Iterator[Vertex] = incidentEdge.map(_.traverseEdges.map(_.origin)).getOrElse(Iterator.empty)
    def edges:Iterator[HalfEdge] = incidentEdge.map(_.traverseEdges).getOrElse(Iterator.empty)
  }

  val outerFace = new Face(outerFaceData, None)

  val innerFaces:mutable.Set[Face] = mutable.Set[Face]()

  val halfEdges:mutable.Set[HalfEdge] = mutable.Set[HalfEdge]()

  val vertices:mutable.Set[Vertex] = mutable.Set[Vertex]()

  def makeVertex(d: VertexData): Vertex = {
    val res = new Vertex(d, None)
    vertices += res
    res
  }

  def makeFace(f: FaceData): Face = {
    val res = new Face(f, None)
    innerFaces += res
    res
  }

  def makeEdge(from: Vertex, to: Vertex, leftFace: Face, rightFace: Face, leftData: HalfEdgeData, rightData: HalfEdgeData): HalfEdge = {
    val main = new HalfEdge(leftData, from, null, leftFace, null, null)
    val twin = new HalfEdge(rightData, to, main, rightFace, null, null)
    halfEdges += main
    halfEdges += twin

    main.twin = twin

    val fPrev = from.edgesWithEndHere.find(_.leftFace == leftFace)
    val fNext = to.edgesWithOriginHere.find(_.leftFace == leftFace)
    val sPrev = to.edgesWithEndHere.find(_.leftFace == rightFace)
    val sNext = from.edgesWithOriginHere.find(_.leftFace == rightFace)

    fPrev match {
      case Some(prev) =>
        prev.next = main
        main.prev = prev
      case None =>
        main.prev = twin
    }

    fNext match {
      case Some(next) =>
        next.prev = main
        main.next = next
      case None =>
        main.next = twin
    }
    sPrev match {
      case Some(prev) =>
        twin.prev = prev
        prev.next = twin
      case None =>
        twin.prev = main
    }
    sNext match {
      case Some(next) =>
        twin.next = next
        next.prev = twin
      case None =>
        twin.next = main
    }

    if (from.incidentEdge.isEmpty) from.incidentEdge = Some(main)
    if (to.incidentEdge.isEmpty) to.incidentEdge = Some(twin)
    if (leftFace.incidentEdge.isEmpty) leftFace.incidentEdge = Some(main)
    if (rightFace.incidentEdge.isEmpty) rightFace.incidentEdge = Some(twin)
    main
  }

  /** e and twin become shorter, creates new vertex and two half-edges */
  def split(e: HalfEdge, at: VertexData, newLeftData: HalfEdgeData, newRightData: HalfEdgeData): Vertex = {
    val res = new Vertex(at, None)
    val newNext = new HalfEdge(newLeftData, res, null, e.leftFace, e.next, e)
    halfEdges += newNext
    e.next = newNext

    val newNextTwin = new HalfEdge(newRightData, e.dest, newNext, e.twin.leftFace, e.twin, e.twin.prev)
    halfEdges += newNextTwin
    e.twin.prev = newNextTwin

    res.incidentEdge = Some(newNext)
    newNext.twin = newNextTwin
    res
  }


  def collapseEdge(e: HalfEdge): Unit = {
    if (e.leftFace.incidentEdge.contains(e))
      e.leftFace.incidentEdge = Option.when(e.next != e)(e.next)
    if (e.twin.leftFace.incidentEdge.contains(e.twin))
      e.twin.leftFace.incidentEdge = Option.when(e.twin.next != e)(e.twin.next)
    if (e.origin.incidentEdge.contains(e))
      e.origin.incidentEdge = Option.when(e.nextAdjacent != e)(e.nextAdjacent)
    if (e.twin.origin.incidentEdge.contains(e.twin))
      e.twin.origin.incidentEdge = Option.when(e.twin.nextAdjacent != e.twin)(e.twin.nextAdjacent)
    e.prev.next = e.next
    e.next.prev = e.prev
    e.twin.prev.next = e.twin.next
    e.twin.next.prev = e.twin.prev
    halfEdges -= e
    halfEdges -= e.twin
  }


}
