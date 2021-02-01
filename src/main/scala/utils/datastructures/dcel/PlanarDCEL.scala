package utils.datastructures.dcel

import utils.math.planar.{AngleCCWPlanar, AngleOps, PointIntersection, PolygonRegion, SegmentIntersection, SegmentPlanar, V2}
import utils.math._



class PlanarDCEL[VD, HED, FD](
                               outerFaceData: FD,
                               val extractor: VD => V2
                             ) extends DCEL[VD, HED, FD](outerFaceData) {

  implicit class PlanarVertex(f: Vertex) {
    def pos: V2 = extractor(f.data)
    /** If vertex has no connected edges return face containing vertex */
    def insideFace: Option[Face] = Option.when(f.incidentEdge.isEmpty)(faceAt(pos))
  }

  implicit class PlanarFace(f: Face) {
    def polygon: PolygonRegion = PolygonRegion(f.vertices.map(v => extractor(v.data)).toSeq)
  }

  implicit class PlanarEdge(e: HalfEdge) {
    def asSegment: SegmentPlanar = SegmentPlanar(extractor(e.origin.data), extractor(e.dest.data))
  }

  def faceAt(pos: V2): Face = innerFaces.find(_.polygon.contains(pos)).getOrElse(outerFace)

  /**
   *
   * @param poly
   * @param newVdProvider     maps new vertex position to VertexData
   * @param newEdProvider     newHalfEdges to its halfEdgeData's
   * @param splitEdgeListener calls on edge split, takes splitted edge as argument, arg.next is new edge with origin at split point, with HED copied from args. Provide new EdgeData if needed.
   * @param splitFaceListener calls on face split, takes egde that splits face as parameter, its leftFace is newly created face with twin.leftFace FD copied. Provide new FaceData if needed.
   */
  def cutPoly(poly: Seq[V2],
              newVdProvider: V2 => VD,
              newEdProvider: (Vertex, Vertex) => (HED, HED),
              newFdProvider: HalfEdge => FD,
             ): Seq[Vertex] = {
    var res: Seq[Vertex] = Seq()

    def getOrAddVertex(pos: V2): Vertex =
      vertices.find(_.pos ~= pos)
        .orElse(
          halfEdges.find(_.asSegment.contains(pos)).map {
            e =>
              val res = split(e, newVdProvider(pos), e.data, e.twin.data)

              res
          }
        ).getOrElse(makeVertex(newVdProvider(pos)))

    if (poly.size <= 1) return Seq()

    var toTraverse = poly.tail
    val startPosition = poly.head


    //    var currentPosition = startPosition
    var previous = getOrAddVertex(startPosition)

    res = res :+ previous

    def popNext(): Vertex = {
      val cur = previous.pos
      val end = toTraverse.head

      val toTest = if (previous.incidentEdge.isEmpty) {
        val face = faceAt(cur)
        face.edges
      } else {
        previous.edgesWithOriginHere
      }

      val path = SegmentPlanar(cur, end)
      val intersections = toTest.flatMap(t => t.asSegment.intersection(path))
        .filter {
          case PointIntersection(p) => !(p ~= cur)
          case _ => true
        }
      if (intersections.isEmpty) {
        toTraverse = toTraverse.tail
        getOrAddVertex(end)
      } else {
        val closestIntersection = intersections.map {
          case PointIntersection(p) => p
          case SegmentIntersection(SegmentPlanar(s, e)) =>
            if (s ~= cur) e
            else if (e ~= cur) s //todo check if intersection can return degenerate segments
            else if (e.distance(cur) < s.distance(cur)) e else s
        }.minBy(_.distance(cur))
        getOrAddVertex(closestIntersection)
      }
    }

    while (toTraverse.nonEmpty) {
      val current = popNext()

      res = res :+ current

      if (previous.incidentEdge.isEmpty && current.incidentEdge.isEmpty) {
        //no edges or faces, we inside some face
        val face = faceAt(previous.pos)
        val (ld, rd) = newEdProvider(previous, current)
        makeEdge(previous, current, face, face, ld, rd)
      } else if (previous.incidentEdge.nonEmpty && current.incidentEdge.isEmpty) {
        //we are going inside face
        val face = faceAt(current.pos)
        val (ld, rd) = newEdProvider(previous, current)
        makeEdge(previous, current, face, face, ld, rd)
      } else if (previous.incidentEdge.isEmpty && current.incidentEdge.nonEmpty) {
        //we are going to edge from inside face
        val face = faceAt(previous.pos)
        val (ld, rd) = newEdProvider(previous, current)
        makeEdge(previous, current, face, face, ld, rd)
      } else {
        //if vertices disconnected
        if(!previous.edgesWithOriginHere.exists(e => e.ending == current)) {
          //we are closing chain
          val prevFaces = previous.adjacentFaces()
          val currentFaces = current.adjacentFaces()

          val face = {
            val commonFaces = prevFaces & currentFaces
            if (commonFaces.isEmpty) {
              throw new Exception("Malformed DCEL, cant find face adjacent to both sides of chain.")
            } else if (commonFaces.size == 1) {
              commonFaces.head
            } else {
              val end = current.pos
              previous.edgesWithOriginHere.map(e => (e, e.prev.origin.pos, e.origin.pos, e.ending.pos)).find{
                case (e, v1, v2, v3) =>
                  val isCCWTurn = AngleOps.isCCW(v1, v2, v3)
                  val ox = v2 - v1
                  val oy = v3 - v2
                  val p = end - v2
                  val projX = ox ** p
                  val projY = oy ** p
                  (isCCWTurn && projX <=0  && projY >= 0) ||
                    (!isCCWTurn &&(projX >= 0 || projY >= 0))
              }.map(_._1.leftFace).get //todo check
            }
          }

          val (ld, rd) = newEdProvider(previous, current)
          val newEdge = makeEdge(previous, current, face, face, ld, rd)
          //if left and right different polys
          if (!newEdge.traverseEdges.contains(newEdge.twin)) {
            makeFace(newFdProvider(newEdge), newEdge)
          }
        }
      }
      previous = current
    }

    res
  }
}
