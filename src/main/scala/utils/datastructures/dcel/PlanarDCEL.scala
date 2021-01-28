package utils.datastructures.dcel

import utils.math.planar.{PointIntersection, PolygonRegion, SegmentIntersection, SegmentPlanar, V2}
import utils.math._

class PlanarDCEL[VD, HED, FD](
                               outerFaceData: FD,
                               extractor: VD => V2
                             ) extends DCEL[VD, HED, FD](outerFaceData) {

  implicit class PlanarVertex(f: Vertex) {
    def pos: V2 = extractor(f.data)
  }

  implicit class PlanarFace(f: Face) {
    def polygon: PolygonRegion = PolygonRegion(f.vertices.map(v => extractor(v.data)).toSeq)
  }

  implicit class PlanarEdge(e: HalfEdge) {
    def asSegment: SegmentPlanar = SegmentPlanar(extractor(e.origin.data), extractor(e.dest.data))
  }

  def faceAt(pos:V2):Face = innerFaces.find(_.polygon.contains(pos)).getOrElse(outerFace)

  def cutPoly(poly: Seq[V2],
              newVdProvider: V2 => VD,
              newEdgeDataProvider:(Vertex, Vertex, Face, Face) => (HED, HED),
              splitEdgeListener: HalfEdge => Unit = (x) => (),
              splitFaceListener: (Face, Face) => Unit = (x, y) => (),
             ): Unit = {
    def getOrAddVertex(pos: V2): Vertex =
      vertices.find(_.pos ~= pos)
        .orElse(
          halfEdges.find(_.asSegment.contains(pos)).map {
            e =>
              val res = split(e, newVdProvider(pos), e.data, e.twin.data)
              splitEdgeListener(e)
              res
          }
        ).getOrElse(makeVertex(newVdProvider(pos)))

    if (poly.size <= 1) return;

    var toTraverse = poly.tail
    val startPosition = poly.head


    //    var currentPosition = startPosition
    var previous = getOrAddVertex(startPosition)

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
      val currentVertex = popNext()
      if(previous.incidentEdge.isEmpty && currentVertex.incidentEdge.isEmpty){
        //no edges or faces, we inside some face
        val face = faceAt(previous.pos)
        val (ld, rd) = newEdgeDataProvider(previous, currentVertex, face, face)
        makeEdge(previous, currentVertex, face, face, ld, rd)
      } else if(previous.incidentEdge.nonEmpty && currentVertex.incidentEdge.isEmpty) {
        //we are going inside face
        val face = faceAt(currentVertex.pos)
        val (ld, rd) = newEdgeDataProvider(previous, currentVertex, face, face)
        makeEdge(previous, currentVertex, face, face, ld, rd)
      } else if(previous.incidentEdge.isEmpty && currentVertex.incidentEdge.nonEmpty){
        //we are going to edge from inside face
        val face = faceAt(previous.pos)
        val (ld, rd) = newEdgeDataProvider(previous, currentVertex, face, face)
        makeEdge(previous, currentVertex, face, face, ld, rd)
      } else {
        //we are connecting different points

      }


      previous = currentVertex
    }

  }
}
