package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{RawFace, RawHalfEdge, RawVertex}

import scala.collection.mutable

object DCELOps {

  def apply[VertexData, HalfEdgeData, FaceData](dcel: DCEL[VertexData, HalfEdgeData, FaceData]): DCELOps[VertexData, HalfEdgeData, FaceData] = new DCELOps(dcel)

  implicit class DCELOps[VertexData, HalfEdgeData, FaceData](val dcel: DCEL[VertexData, HalfEdgeData, FaceData]) extends AnyVal {
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
          .map(x => x.leftFace.asInstanceOf[RawFace[VertexData, HalfEdgeData, FaceData]])
          .foreach { face =>
            if (!visitedFaces.contains(face)) {
              faceQueue += face
              visitedFaces += face
            }
          }

        res
      }
    }

    /**
      * This procedure assumes that toMerge can't be neighbour and hole at the same time
      * routine:
      * - mergeFaceDatas called once before merge
      * - face data on main updated
      * - then toMerge rewritten as main
      * - then for every edge
      * - - halfEdgeDestructor called
      * - - halfEdge detached
      * */
    def mergeAdjancedFaces(main: Face, toMerge: Face,
                           mergeFaceDatas: (FaceData, FaceData) => FaceData,
                           halfEdgeDestructor: (HalfEdgeData, HalfEdgeData) => Unit): Boolean = {

      if (main == toMerge) false
      else {
        val commonBorderEdges = main.edges.filter(_.twin.leftFace == toMerge).toSeq
        if (commonBorderEdges.nonEmpty) { //we're simple neighbours
          val newFaceData = mergeFaceDatas(main.data, toMerge.data)
          main.data = newFaceData
          main._holesIncidentEdges = main._holesIncidentEdges.filter(_.twin.leftFace != toMerge) ++
            toMerge._holesIncidentEdges.filter(_.twin.leftFace != main)
          for(e <- toMerge.edges) e._leftFace = main
          for(e <- commonBorderEdges) {
            halfEdgeDestructor(e.data, e.twin.data)
            dcel.deleteEdgeUnsafe(e)
          }
          dcel.innerFaces -= toMerge
          dcel.onFaceDelete(toMerge)
          true
        } else false
      }
    }
  }




}
