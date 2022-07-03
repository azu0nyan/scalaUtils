package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{RawFace, RawHalfEdge, RawVertex}
import utils.math.planar.PolygonRegion

import scala.collection.mutable

object DCELOps {

  def apply[VertexData, HalfEdgeData, FaceData](dcel: DCEL[VertexData, HalfEdgeData, FaceData]): DCELOpsImplicit[VertexData, HalfEdgeData, FaceData] =
    new DCELOpsImplicit[VertexData, HalfEdgeData, FaceData](dcel)


  def selectToTheLeft[VD, HED, FD](hes: Seq[RawHalfEdge[VD, HED, FD]]): Iterator[RawFace[VD, HED, FD]] = new Iterator[RawFace[VD, HED, FD]] {
    type Dcel = DCEL[VD, HED, FD]
    type HalfEdge = RawHalfEdge[VD, HED, FD]
    type Vertex = RawVertex[VD, HED, FD]
    type Face = RawFace[VD, HED, FD]

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
        .map(x => x.leftFace.asInstanceOf[RawFace[VD, HED, FD]])
        .foreach { face =>
          if (!visitedFaces.contains(face)) {
            faceQueue += face
            visitedFaces += face
          }
        }

      res
    }
  }

  def toChain[VD, HED, FD](vs: Seq[RawVertex[VD, HED, FD]]): Iterator[Option[RawHalfEdge[VD, HED, FD]]] = if (vs.size >= 2) vs.sliding(2).map {
    case List(o, e) => o.edgesWithOriginHere.find(_.ending == e)
  } else Iterator.empty

  implicit class DCELOpsImplicit[VertexData, HalfEdgeData, FaceData](val dcel: DCEL[VertexData, HalfEdgeData, FaceData]) extends AnyVal {
    type Dcel = DCEL[VertexData, HalfEdgeData, FaceData]
    type HalfEdge = RawHalfEdge[VertexData, HalfEdgeData, FaceData]
    type Vertex = RawVertex[VertexData, HalfEdgeData, FaceData]
    type Face = RawFace[VertexData, HalfEdgeData, FaceData]




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
                           mergeFaceDatas: (FaceData, FaceData) => FaceData = (a, b) => a,
                           halfEdgeDestructor: (HalfEdgeData, HalfEdgeData) => Unit = (a, b) => ()): Boolean = {
      //todo remove hanging vertices
      if (main == toMerge) false
      else {
        val commonBorderEdges = main.edges.filter(_.twin.leftFace == toMerge).toSeq
        if (commonBorderEdges.nonEmpty) { //we're simple neighbours
          val isMergingWithHole = main.holesEdges.map(_.twin.leftFace).contains(toMerge)
          val isMainHole = toMerge.holesEdges.map(_.twin.leftFace).contains(main)

          val newFaceData = mergeFaceDatas(main.data, toMerge.data)
          main.data = newFaceData
          //          main._holesIncidentEdges = main._holesIncidentEdges.filter(_.twin.leftFace != toMerge) ++
          //            toMerge._holesIncidentEdges.filter(_.twin.leftFace != main)
          main._holesIncidentEdges = main._holesIncidentEdges ++ toMerge._holesIncidentEdges
          for (e <- toMerge.edges) e._leftFace = main
          if (isMainHole) main._incidentEdge = toMerge._incidentEdge

          dcel.innerFaces -= toMerge
          dcel.onFaceDelete(toMerge)

          for (e <- if (isMainHole) commonBorderEdges.map(_.twin) else commonBorderEdges) {
            halfEdgeDestructor(e.data, e.twin.data)
            dcel.deleteEdgeUnsafe(e)
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
              //              println(dcel.asInstanceOf[PlanarDCEL[VertexData, HalfEdgeData, FaceData]].pos(e._origin), dcel.asInstanceOf[PlanarDCEL[VertexData, HalfEdgeData, FaceData]].pos(e.ending))
              //              println(chainBroken, isMergingWithHole)
              if (chainBroken && !isMergingWithHole) { //if we created hole
                dcel match {
                  case p: PlanarDCEL[VertexData, HalfEdgeData, FaceData] => //ugly typecast
                    val nextPoly = PolygonRegion(next.traverseEdges.map(e => p.pos(e.origin)).toSeq)
                    val prevPoly = PolygonRegion(prev.traverseEdges.map(e => p.pos(e.origin)).toSeq)
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
  }


}
