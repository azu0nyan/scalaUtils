package utils.datastructures.dcel

import utils.math.planar.{PolygonRegion, SegmentPlanar, V2}

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


  def cutPoly(poly: Seq[V2],
              newVdProvider: V2 => VD,
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
    var currentVertex = getOrAddVertex(startPosition)

    def popNext():Vertex = {
      val end = toTraverse.head
      val cur = currentVertex.pos

      val toTest = if(currentVertex.incidentEdge.isEmpty) {
        val face = innerFaces.find(_.polygon.contains(currentVertex.pos)).getOrElse(outerFace)
        face.edges
      } else {
        currentVertex.edgesWithOriginHere
      }

      val path = SegmentPlanar(cur, end)



    }

    var previousVertex = getOrAddVertex()

    if(currentVertex.edgesWithOriginHere.isEmpty) {

    } else {

    }

   /* def popNext():(V2, Option[Face]) = {
      val seg = SegmentPlanar(currentPosition, toTraverse.head)
      if(currentVertex.incidentEdge.isEmpty){
        innerFaces.find(_.polygon.contains(currentPosition)).iterator.toSeq
      }
      val face =
      if(currentFace.)
    }*/

    while (toTraverse.nonEmpty){

    }

//    var currentFace:Option[Face] = outerFace
//    if(currentVertex.incidentEdge.isEmpty){
//      currentFace =
//    }

    //      var currentFace: Option[Face] = innerFaces.find(_.polygon.contains(currentVertex))
    //      var faceEdge: Option[HalfEdge] = currentFace.flatMap(_.edges.find(_.asSegment.contains(currentVertex)))
    //      var edgeVertex: Option[Vertex] = faceEdge.flatMap(e =>
    //        if (e.origin.pos ~= currentVertex) Some(e.origin)
    //        else if (e.dest.pos ~= currentVertex) Some(e.dest)
    //        else None)


  }
}
