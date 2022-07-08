package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{CantDoOpOnDCELException, MalformedDCELException, MultipleEdgesBetweenTwoVerticesException, RawFace, RawHalfEdge, RawVertex}
import utils.datastructures.dcel.PlanarDCEL._
import utils.math.planar.{AngleCCWPlanar, AngleOps, PointIntersection, Polygon, PolygonRegion, SegmentIntersection, SegmentPlanar, V2}
import utils.math._


object PlanarDCEL {
  implicit class PlanarVertex[VD, HED, FD](f: RawVertex[VD, HED, FD])(implicit extractor: VD => V2) {
    def position: V2 = extractor(f.data)

    /** If vertex has no connected edges return face containing vertex */
    def insideFace(implicit dcel: PlanarDCEL[VD, HED, FD]): Option[RawFace[VD, HED, FD]] = Option.when(f.incidentEdge.isEmpty)(dcel.faceAt(position))
  }

  implicit class PlanarFace[VD, HED, FD](f: RawFace[VD, HED, FD])(implicit extractor: VD => V2) {
    def polygon: PolygonRegion = PolygonRegion(f.outsideVertices.map(v => extractor(v.data)).toSeq)
  }

  implicit class PlanarEdge[VD, HED, FD](e: RawHalfEdge[VD, HED, FD])(implicit extractor: VD => V2) {
    def asSegment: SegmentPlanar = SegmentPlanar(extractor(e.origin.data), extractor(e.dest.data))

  }
}

class PlanarDCEL[VD, HED, FD](
                               outerFaceData: FD,
                               implicit val extractor: VD => V2
                             ) extends DCEL[VD, HED, FD](outerFaceData) {


  def pos(v: RawVertex[VD, HED, FD]): V2 = extractor(v.data)

  def seg(e: HalfEdge): SegmentPlanar = e.asSegment

  def outerContour(f: Face): PolygonRegion = PolygonRegion(f.outsideVertices.map(_.position).toSeq)

  def polygon(f: Face): Polygon = Polygon(outerContour(f) +: f.holesIncidentEdges.toSeq.map(h => PolygonRegion(h.traverseEdges.map(_.origin.position).toSeq)))


  def faceAt(pos: V2): Face = {
    innerFaces.find(f => f.polygon.contains(pos) &&
      !f.holesContours.exists(c => PolygonRegion(c.map(_.origin.position).toSeq).contains(pos))).getOrElse(outerFace)
  }

  /*innerFaces.filter(_._holes.isEmpty).find(_.polygon.contains(pos)).getOrElse {
  val cont = innerFaces.filter(_._holes.nonEmpty).filter(_.polygon.contains(pos))
  if (cont.isEmpty) outerFace
  else if (cont.size == 1) cont.head
  else cont.minBy(_.polygon.area)
}*/


  /**
    *
    * @param poly              in ccw order with Y-up
    * @param newVdProvider     maps new vertex position to VertexData
    * @param newEdProvider     newHalfEdges to its halfEdgeData's
    * @param splitEdgeListener calls on edge split, takes splitted edge as argument, arg.next is new edge with origin at split point, with HED copied from args. Provide new EdgeData if needed.
    * @param splitFaceListener calls on face split, takes egde that splits face as parameter, its leftFace is newly created face with twin.leftFace FD copied. Provide new FaceData if needed.
    */
  def cutPoly(poly: Seq[V2], newVdProvider: V2 => VD,
              newEdProvider: (Vertex, Vertex) => (HED, HED),
              splitEdProvider: (HalfEdge, V2) => (HED, HED),
              newFdProvider: HalfEdge => FD,
             ): Seq[Vertex] = cutChain(poly :+ poly.head, newVdProvider, newEdProvider, splitEdProvider, newFdProvider)


  def getOrAddVertex(pos: V2, newVdProvider: V2 => VD, splitEdProvider: (HalfEdge, V2) => (HED, HED)): Vertex = {
    val res = vertices.find(_.position ~= pos)
      .orElse(
        halfEdges.find(_.asSegment.contains(pos)).map {
          e =>
            val (l, r) = splitEdProvider(e, pos)
            val res = split(e, newVdProvider(pos), l, r)

            res
        }
      ).getOrElse(makeVertex(newVdProvider(pos)))
    res
  }

  def cutFromTo(from: V2, to: V2,
                newVdProvider: V2 => VD,
                newEdProvider: (Vertex, Vertex) => (HED, HED),
                splitEdProvider: (HalfEdge, V2) => (HED, HED),
                newFdProvider: HalfEdge => FD,
               ): Seq[Vertex] = cutChain(Seq(from, to), newVdProvider, newEdProvider, splitEdProvider, newFdProvider)

  def cutChain(poly: Seq[V2],
               newVdProvider: V2 => VD,
               newEdProvider: (Vertex, Vertex) => (HED, HED),
               splitEdProvider: (HalfEdge, V2) => (HED, HED),
               newFdProvider: HalfEdge => FD,
              ): Seq[Vertex] = {
    var res: Seq[Vertex] = Seq()


    if (poly.size <= 1) return Seq()

    var toTraverse = poly.tail
    val startPosition = poly.head


    //    var currentPosition = startPosition
    var previous = getOrAddVertex(startPosition, newVdProvider, splitEdProvider)

    res = res :+ previous

    def popNext(): Vertex = {
      val cur: V2 = previous.position
      val end: V2 = toTraverse.head

      val toTest: Seq[HalfEdge] = if (previous.incidentEdge.isEmpty) {
        val face = faceAt(cur)
        //        println(if (face == outerFace) "outer" else face.data.toString)
        //        println(face.edges.map(_.asSegment.start).toSeq)
        face.edges.toSeq
      } else {

        previous.edgesWithOriginHere.map(_.leftFace).distinct.flatMap(_.edges.toSeq).toSeq
      }

      val path = SegmentPlanar(cur, end)
      //      println(toTest.map(_.asSegment))
      val intersections = toTest.flatMap(t => t.asSegment.intersection(path))
        .filter {
          case PointIntersection(p) => !(p ~= cur)
          case _ => true
        }
      //      println(path)
      //      println(intersections)
      if (intersections.isEmpty) {
        toTraverse = toTraverse.tail
        getOrAddVertex(end, newVdProvider, splitEdProvider)
      } else {
        val closestIntersection = intersections.map {
          case PointIntersection(p) => p
          case SegmentIntersection(SegmentPlanar(s, e)) =>
            if (s ~= cur) e
            else if (e ~= cur) s
            else if (e.distance(cur) < s.distance(cur)) e else s
        }.minBy(_.distance(cur))

        if (closestIntersection ~= end) {
          toTraverse = toTraverse.tail
        }
        getOrAddVertex(closestIntersection, newVdProvider, splitEdProvider)
      }
    }

    while (toTraverse.nonEmpty) {
      val current = popNext()
      res = res :+ current

      val start = previous.position
      val end = current.position

      if (previous.incidentEdge.isEmpty && current.incidentEdge.isEmpty) {
        //                println("1")
        //no edges or faces, we inside some face
        val face = faceAt(previous.position)
        val (ld, rd) = newEdProvider(previous, current)
        val he = makeEdge(previous, current, face, face, ld, rd)
        face._holesIncidentEdges += he
      } else if (previous.incidentEdge.nonEmpty && current.incidentEdge.isEmpty) {
        //                println("2")
        //we are going inside face
        val face = faceAt(current.position)
        //
        //
        val prevEdge = previous.edgesWithEndHere.minBy { e =>
          val v1 = end - start
          val estart = e.origin.position
          val eend = e.ending.position
          val v2 = estart - eend
          //min ccw
          val res = AngleOps.ccwAngleFromTo(v1, v2)
          //          println(estart, eend, v1, v2, res)
          res
        }

        val (ld, rd) = newEdProvider(previous, current)
        makeEdge(previous, current, prevEdge.leftFace, prevEdge.leftFace, ld, rd,
          Some(prevEdge), None, None, Some(prevEdge.next),
        )
        //        val (ld, rd) = newEdProvider(previous, current)
        //        makeEdge(previous, current, face, face, ld, rd)
      } else if (previous.incidentEdge.isEmpty && current.incidentEdge.nonEmpty) {
        //                println("3")
        //we are going to edge from inside face
        val nextEdge = current.edgesWithOriginHere.minBy { e =>
          val v1 = start - end
          val estart = e.origin.position
          val eend = e.ending.position
          val v2 = eend - estart
          //min cw
          val res = -AngleOps.ccwAngleFromTo(v1, v2)
          //              println(estart, eend, v1, v2, res)
          res
        }

        val face = faceAt(previous.position)
        val (ld, rd) = newEdProvider(previous, current)
        makeEdge(previous, current, face, face, ld, rd, None, Some(nextEdge), Some(nextEdge.prev), None)
      } else {
        //                println("4")
        //if vertices disconnected
        if (!previous.edgesWithOriginHere.exists(e => e.ending == current)) {
          //we are closing chain
          val prevFaces = previous.adjacentFaces()
          val currentFaces = current.adjacentFaces()

          val commonFaces = prevFaces & currentFaces
          val (ld, rd) = newEdProvider(previous, current)

          if (commonFaces.isEmpty) {
            throw new Exception("Malformed DCEL, cant find face adjacent to both sides of chain.")
          } else /*if (commonFaces.size == 100) { //todo ??
            val face = commonFaces.head
            val newEdge = makeEdge(previous, current, face, face, ld, rd)
            //if left and right different polys
            if (!newEdge.traverseEdges.contains(newEdge.twin)) {
              makeFace(newFdProvider(newEdge), newEdge, newEdge.twin)
            }
          } else*/ {


            val nextEdge = current.edgesWithOriginHere.minBy { e =>
              val v1 = start - end
              val estart = e.origin.position
              val eend = e.ending.position
              val v2 = eend - estart
              //min cw
              val res = -AngleOps.ccwAngleFromTo(v1, v2)
              //              println(estart, eend, v1, v2, res)
              res
            }
            //            println(s"next ${nextEdge.data}")
            val prevEdge = previous.edgesWithEndHere.minBy { e =>
              val v1 = end - start
              val estart = e.origin.position
              val eend = e.ending.position
              val v2 = estart - eend
              //min ccw
              val res = AngleOps.ccwAngleFromTo(v1, v2)
              //              println(estart, eend, v1, v2, res)
              res
            }
            //            println(s"prev ${prevEdge.data}")


            val newEdge = makeEdge(previous, current, prevEdge.leftFace, nextEdge.leftFace, ld, rd,
              Some(prevEdge), Some(nextEdge), Some(nextEdge.prev), Some(prevEdge.next),
            )
            //if left and right different polys
            if (!newEdge.traverseEdges.contains(newEdge.twin)) {
              //if we cut part of outer we should start from inner edge
              val startFaceFrom = if (PolygonRegion(newEdge.traverseEdges.map(_.origin.position).toSeq).isCcw) newEdge else newEdge.twin

              //              println(s"Making new face starting from ${startFaceFrom.data}")
              val f = makeFace(newFdProvider(startFaceFrom), startFaceFrom, startFaceFrom.twin)
              val fPoly = f.polygon
              f.incidentEdge.flatMap(_.traverseAllReachableEdges().filter(_.isHoleHalfSide).nextOption()) match {
                case Some(holeSide) =>
                  //                  println(s"Face can contain holes ${holeSide.data} ${holeSide.leftFace.holes.map(_.data)}")
                  holeSide.leftFace.holesIncidentEdges.filter(h =>
                    h.traverseEdges.map(_.origin.position).forall(v => fPoly.classify(v) == PolygonRegion.INSIDE)
                  ).foreach {
                    hole =>
                      //                      println(s"Found hole ${hole.data}")
                      holeSide.leftFace._holesIncidentEdges -= hole
                      f._holesIncidentEdges += hole
                      //                      println(holeSide.leftFace._holes.map(_.data))
                      //                      println(f._holes.map(_.data))
                      //                      println("Overriding old parent")
                      hole.traverseEdges.foreach(he => he._leftFace = f)

                  }
                case None =>
                  throw new Exception("Malformed DCEL, cant find hole side")
              }

            } else {
              //connected something inside leftFace, probably  two holes
              if (newEdge.leftFace == newEdge.twin.leftFace) {
                //                println("Connecting holes")
                val leftFace = newEdge.leftFace
                val holes = leftFace._holesIncidentEdges.toSeq
                if (holes.nonEmpty) {
                  leftFace.borderEdges.find(he => holes.contains(he)) match {
                    //if hole become part of border
                    case Some(edge) => leftFace._holesIncidentEdges = leftFace._holesIncidentEdges.filter(_ != edge)
                    case None =>
                      var badHoles: Set[HalfEdge] = Set()

                      for (i <- 0 until holes.size; j <- (i + 1) until holes.size) {
                        if (holes(i).traverseEdges.contains(holes(j))) {
                          badHoles += holes(j)
                        }
                      }

                      leftFace._holesIncidentEdges = leftFace._holesIncidentEdges &~ badHoles
                  }
                }
              }
            }


          }

          /*val face = {
            val commonFaces = prevFaces & currentFaces
            if (commonFaces.isEmpty) {
              throw new Exception("Malformed DCEL, cant find face adjacent to both sides of chain.")
            } else if (commonFaces.size == 1) {
              commonFaces.head
            } else {
              val start = previous.pos
              val end = current.pos
              val v1 = start - end
              current.edgesWithOriginHere.minBy { e =>
                val estart = e.origin.pos
                val end = e.ending.pos
                val v2 = estart - end
                AngleOps.angleFromTo(v1, v2)

              }.leftFace
              /*.map(e => (e, e.prev.origin.pos, e.origin.pos, e.ending.pos)).find{
              case (e, v1, v2, v3) =>
                val isCCWTurn = AngleOps.isCCW(v1, v2, v3)
                val ox = v2 - v1
                val oy = v3 - v2
                val p = end - v2
                val projX = ox ** p
                val projY = oy ** p
                (isCCWTurn && projX <=0  && projY >= 0) ||
                  (!isCCWTurn &&(projX >= 0 || projY >= 0))
            }.map(_._1.leftFace).get */
              //todo check
            }
          }*/
        }
      }
      previous = current
    }

    res
  }

  /**
    * Merges two vertices, calls onVertexDelete(v2).
    * Can't merge vertices with edge between.
    * Assumes that v1 close or in the same place
    * Merge allowed only on planar DCEL's, since on usual DCEL winding order after merge undefined
    * //todo
    * */
  @deprecated("since unfinished") def mergeVertices(v1: Vertex, v2: Vertex): Unit =
    if (v1.incidentEdge.nonEmpty && v2.incidentEdge.nonEmpty) {
      //for(e <- v1.edgeTo(v2)) deleteEdge(e) //Don't do it like that,  since deleteEdge can delete face that shouldn't be deleted  after vertex merge
      val edgeBetween = v1.edgeTo(v2)
      if (edgeBetween.nonEmpty) {
        //maybe implement
        throw new CantDoOpOnDCELException("Cant merge vertices with edge betweem")
        //our merge can delete some face
        //        for(e1 <- v1.edgesWithOriginHere; e2 <- v2.edgesWithOriginHere if e1.ending == e2.ending) {
        //          if(e1.leftFace == outerFace && e2.leftFace == outerFace) throw new MalformedDCELException("")
        //        }
        //        deleteEdgeUnsafe(edgeBetween.get)     //Do not modifies faces
      }
      /*
           v1oPrev
              |           __
              |f1      f2 /|
              |          / v2eNext
             \|        /
             v1      v2
              |        |\
           v1o|         \
              |          \v2e
             \|           \

     */
      val (v1o, v2e) = (for (v1o <- v1.edgesWithOriginHere; v2e <- v2.edgesWithEndHere) yield (v1o, v2e)).minBy {
        case (v1o, v2e) => v1o.asSegment.body.angleCCW0to2PI(v2e.asSegment.reverse.body)
      }
      val v1oPrev = v1o.prev
      val v2eNext = v2e.next

      v1o._prev = v2e
      v2e._next = v1o

      v1oPrev._next = v2eNext
      v2eNext._prev = v1oPrev

      for (e <- v2.edgesWithOriginHere) e._origin = v1

      //fix broken polygon or create new

      onVertexRemoved(v2)
    } else if (v1.incidentEdge.nonEmpty) { //v2 - empty, nothing to merge
      onVertexRemoved(v2)
    } else if (v2.incidentEdge.nonEmpty) { // v1 empty, redirect edges with origin at v2
      v1._incidentEdge = v2.incidentEdge
      for (e <- v2.edgesWithOriginHere) e._origin = v1
      onVertexRemoved(v2)
    } else { //both empty
      onVertexRemoved(v2)
    }

  def insert[OVD, OHED, OFD](ot: DCEL[OVD, OHED, OFD],
                             mapVD: RawVertex[OVD, OHED, OFD] => VD,
                             mapHED: RawHalfEdge[OVD, OHED, OFD] => HED,
                             mapFD: RawFace[OVD, OHED, OFD] => FD): Unit = {

  }
}
