package utils.datastructures.dcel


/** Double connected edge list */
class DCEL[VertexData, HalfEdgeData, FaceData](
                                                outerFaceData: FaceData
                                              ) {

  class Vertex private[DCEL](
                              private[this] var _data: VertexData,
                              private[DCEL] var incidentEdge: Option[HalfEdge] = None
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
  class HalfEdge private[DCEL](
                                private[this] var _data: HalfEdgeData,
                                private[DCEL] var origin: Vertex,
                                private[DCEL] var twin: HalfEdge,
                                private[DCEL] var leftFace: Face,
                                private[DCEL] var next: HalfEdge,
                                private[DCEL] var prev: HalfEdge,
                              ) {

    def dest: Vertex = twin.origin

    def data: HalfEdgeData = _data
    def data_=(value: HalfEdgeData): Unit = {
      _data = value
    }


    def traverseEdges:Iterator[HalfEdge] = new Iterator[HalfEdge] {
      var cur:HalfEdge = HalfEdge.this
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
                            private[this] var _data: FaceData = null,
                            private[DCEL] var incidentEdge: Option[HalfEdge] = None
                          ) {
    def data: FaceData = _data
    def data_=(value: FaceData): Unit = {
      _data = value
    }
  }

  val outerFace = new Face(outerFaceData, None)

  def makeVertex(d: VertexData): Vertex = new Vertex(d, None)

  def makeFace(f:FaceData):Face = new Face(f, None)

  def makeEdge(from: Vertex, to: Vertex, leftFace: Face, rightFace: Face, leftData: HalfEdgeData, rightData: HalfEdgeData): HalfEdge = {
    val main = new HalfEdge(leftData, from, null, leftFace, null, null)
    val twin = new HalfEdge(rightData, to, main, rightFace, null, null)
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
      case None  =>
      twin.next = main
    }

    if (from.incidentEdge.isEmpty) from.incidentEdge = Some(main)
    if (to.incidentEdge.isEmpty) to.incidentEdge = Some(twin)
    if(leftFace.incidentEdge.isEmpty) leftFace.incidentEdge = Some(main)
    if(rightFace.incidentEdge.isEmpty) rightFace.incidentEdge = Some(twin)
    main
  }

  /**e and twin become shorter, creates new vertex and two half-edges */
  def split(e:HalfEdge, at:VertexData, newLeftData:HalfEdgeData, newRightData:HalfEdgeData):Vertex = {
    val res = new Vertex(at, None)
    val newNext = new HalfEdge(newLeftData, res, null, e.leftFace, e.next, e)
    e.next = newNext

    val newNextTwin = new HalfEdge(newRightData, e.dest, newNext, e.twin.leftFace, e.twin, e.twin.prev)
    e.twin.prev = newNextTwin

    res.incidentEdge = Some(newNext)
    newNext.twin = newNextTwin
    res
  }


}
