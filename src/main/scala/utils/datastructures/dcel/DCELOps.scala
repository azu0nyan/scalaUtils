package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{RawFace, RawHalfEdge, RawVertex}

import scala.collection.mutable

object DCELOps {

  def apply[VertexData, HalfEdgeData, FaceData]( dcel: DCEL[VertexData, HalfEdgeData, FaceData]):DCELOps[VertexData, HalfEdgeData, FaceData] = new DCELOps(dcel)

  implicit  class DCELOps[VertexData, HalfEdgeData, FaceData](val dcel: DCEL[VertexData, HalfEdgeData, FaceData]) extends AnyVal {
    type Dcel = DCEL[VertexData, HalfEdgeData, FaceData]
    type HalfEdge = RawHalfEdge[VertexData, HalfEdgeData, FaceData]
    type Vertex = RawVertex[VertexData, HalfEdgeData, FaceData]
    type Face = RawFace[VertexData, HalfEdgeData, FaceData]

    def toChain(vs: Seq[Vertex]): Iterator[Option[HalfEdge]] = if (vs.size >= 2) vs.sliding(2).map {
      case List(o, e) => o.edgesWithOriginHere.find(_.ending == e)
    } else Iterator.empty


    def selectToTheLeft(hes: Seq[HalfEdge]): Iterator[Face] = new Iterator[Face] {
      val visitedFaces = new mutable.HashSet[Face]()
      val forbiddenEdges: Set[HalfEdge] = hes.toSet
      val faceQueue: mutable.Queue[Face] = mutable.Queue()

      hes.foreach(he => {
        if (!visitedFaces.contains(he.leftFace)) {
          faceQueue += he.leftFace
          visitedFaces += he.leftFace
        }
      })

      override def hasNext: Boolean = faceQueue.nonEmpty
      override def next(): Face = {
        val res = faceQueue.dequeue()

        res.edges
          .filter(he => !forbiddenEdges.contains(he))
          .map(x => x.leftFace.asInstanceOf[RawFace[VertexData,HalfEdgeData,FaceData]])
          .foreach { face =>
            if (!visitedFaces.contains(face)) {
              faceQueue += face
              visitedFaces += face
            }
          }

        res
      }
    }
  }
}
