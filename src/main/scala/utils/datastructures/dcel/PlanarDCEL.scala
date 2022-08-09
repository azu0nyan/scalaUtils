package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{CantDoOpOnDCELException, DCELData, Face, HalfEdge, MalformedDCELException, MultipleEdgesBetweenTwoVerticesException, Vertex}
import utils.datastructures.dcel.PlanarDCEL._
import utils.math.planar.{AngleCCWPlanar, AngleOps, PointIntersection, Polygon, PolygonRegion, SegmentIntersection, SegmentPlanar, V2}
import utils.math._

import scala.collection.mutable


object PlanarDCEL {
  implicit class PlanarVertex[D <: DCELData](f: Vertex[D])(implicit extractor: D#VertexData => V2) {
    def position: V2 = extractor(f.data)

    /** If vertex has no connected edges return face containing vertex */
    def insideFace(implicit dcel: PlanarDCEL[D]): Option[Face[D]] = Option.when(f.incidentEdge.isEmpty)(dcel.faceAt(position))
  }

  implicit class PlanarFace[D <: DCELData](f: Face[D])(implicit extractor: D#VertexData => V2) {
    def polygon: PolygonRegion = PolygonRegion(f.outsideVertices.map(v => extractor(v.data)).toSeq)
  }

  implicit class PlanarEdge[D <: DCELData](e: HalfEdge[D])(implicit extractor: D#VertexData => V2) {
    def asSegment: SegmentPlanar = SegmentPlanar(extractor(e.origin.data), extractor(e.dest.data))

  }
}

class PlanarDCEL[D <: DCELData](
                                 outerFaceData: D#FaceData,
                                 implicit val extractor: D#VertexData => V2
                               ) extends DCEL[D](outerFaceData) {


  def position(v: Vertex[D]): V2 = extractor(v.data)

  def asSegment(e: HalfEdge[D]): SegmentPlanar = e.asSegment

  def outerContour(f: Face[D]): PolygonRegion = PolygonRegion(f.outsideVertices.map(_.position).toSeq)

  def asPolygon(f: Face[D]): Polygon = Polygon(outerContour(f) +: f.holesIncidentEdges.toSeq.map(h => PolygonRegion(h.traverseEdges.map(_.origin.position).toSeq)))

  def getEdge(from: V2, to: V2): Option[HalfEdge[D]] =
    for (f <- getVertex(from); to <- getVertex(to); e <- f.edgeTo(to)) yield e

  def getStraightEdgePath(from: V2, to: V2): Seq[HalfEdge[D]] = {
    val f = getVertex(from)
    val t = getVertex(to)
    if (f.isEmpty || t.isEmpty) Seq()
    else getStraightEdgePathVertex(f.get, t.get)
  }

  def getStraightEdgePathVertex(from: Vertex[D], to: Vertex[D]): Seq[HalfEdge[D]] =
    if (from == to) Seq()
    else {
      var res: Seq[HalfEdge[D]] = Seq()
      val dir = to.position - from.position
      var cur = from.edgesWithOriginHere.find(_.asSegment.body.sameDirection(dir))
      while (cur.nonEmpty && cur.get.ending != to) {
        res = res :+ cur.get
        cur = cur.get.ending.edgesWithOriginHere.find(_.asSegment.body.sameDirection(dir))
      }
      if (cur.nonEmpty && cur.get.ending == to) res :+ cur.get
      else Seq()
    }

  def closestVertexOpt(pos: V2): Option[Vertex[D]] = vertices.minByOption(_.position.distance(pos))

  def faceAt(pos: V2): Face[D] = {
    innerFaces.find(f => f.polygon.contains(pos) &&
      !f.holesContours.exists(c => PolygonRegion(c.map(_.origin.position).toSeq).contains(pos))).getOrElse(outerFace)
  }

  def getVertex(pos: V2): Option[Vertex[D]] = vertices.find(_.position ~= pos)

  def getOrAddVertex(pos: V2, newVdProvider: NewVertexDataProvider[D], splitEdProvider: SplitEdgeDataProvider[D]): Vertex[D] = {
    val res = vertices.find(_.position ~= pos)
      .orElse(
        halfEdges.find(_.asSegment.contains(pos)).map {
          e =>
            val (l, r) = splitEdProvider.splitEdgeData(e, pos)
            val res = split(e, newVdProvider.newVertexData(pos), l, r)

            res
        }
      ).getOrElse(makeVertex(newVdProvider.newVertexData(pos)))
    res
  }

  //  private def holeAreasOf(f:Face[D]):Seq[Face[D]] = f.holes.toSeq ++ f.holes.flatMap(h => h.holes.flatMap(holeAreasOf).toSeq)
  //  def nonHoleFaces:Seq[Face[D]] = holeAreasOf(outerFace)


  def holeNonHoleFaces: (Seq[Face[D]], Seq[Face[D]]) = {
    val odd: mutable.Buffer[Face[D]] = mutable.Buffer()
    val even: mutable.Buffer[Face[D]] = mutable.Buffer()
    var curWave = 0
    var curWaveFaces = Seq(outerFace)
    while (curWaveFaces.nonEmpty){
      val nextWave = curWaveFaces.flatMap(_.allHoleFaces)
      if(curWave % 2 == 0) even ++= nextWave
      else odd ++= nextWave
      curWaveFaces = nextWave
      curWave += 1
    }
    println((odd.toSeq, even.toSeq))
    (odd.toSeq, even.toSeq)
  }

  def nonHoleFaces:Seq[Face[D]] = holeNonHoleFaces._2

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
    * @param splitEdgeListener calls on edge split, takes splitted edge as argument, arg.next is new edge with origin at split point, with D#HalfEdgeData copied from args. Provide new EdgeData if needed.
    * @param splitFaceListener calls on face split, takes egde that splits face as parameter, its leftFace is newly created face with twin.leftFace D#FaceData copied. Provide new FaceData if needed.
    * @return Seq of vertices that connected by cutted edges, first and last vertices are equal
    */
  def cutPoly(poly: Seq[V2],
              dcelDataProvider: DCELDataProvider[D]
             ): Seq[Vertex[D]] = cutChain(poly :+ poly.head, dcelDataProvider)

  def cutFromTo(from: V2, to: V2, dcelDataProvider: DCELDataProvider[D]): Seq[Vertex[D]] =
    cutChain(Seq(from, to), dcelDataProvider)

  def cutChain(poly: Seq[V2], dcelDataProvider: DCELDataProvider[D]): Seq[Vertex[D]] = {
    var res: Seq[Vertex[D]] = Seq()


    if (poly.size <= 1) return Seq()

    var toTraverse = poly.tail
    val startPosition = poly.head


    //    var currentPosition = startPosition
    var previous = getOrAddVertex(startPosition, dcelDataProvider, dcelDataProvider)

    res = res :+ previous

    def popNext(): Vertex[D] = {
      val cur: V2 = previous.position
      val end: V2 = toTraverse.head

      val toTest: Seq[HalfEdge[D]] = if (previous.incidentEdge.isEmpty) {
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
        getOrAddVertex(end, dcelDataProvider, dcelDataProvider)
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
        getOrAddVertex(closestIntersection, dcelDataProvider, dcelDataProvider)
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
        val (ld, rd) = dcelDataProvider.newEdgeData(previous, current)
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

        val (ld, rd) = dcelDataProvider.newEdgeData(previous, current)
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
        val (ld, rd) = dcelDataProvider.newEdgeData(previous, current)
        makeEdge(previous, current, face, face, ld, rd, None, Some(nextEdge), Some(nextEdge.prev), None)
      } else {
        //                println("4")
        //if vertices disconnected
        if (!previous.edgesWithOriginHere.exists(e => e.ending == current)) {
          //we are closing chain
          val prevFaces = previous.adjacentFaces()
          val currentFaces = current.adjacentFaces()

          val commonFaces = prevFaces & currentFaces
          val (ld, rd) = dcelDataProvider.newEdgeData(previous, current)

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
              val f = makeFace(dcelDataProvider.newFaceData(startFaceFrom), startFaceFrom, startFaceFrom.twin)
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
                      var badHoles: Set[HalfEdge[D]] = Set()

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
    * Assumes that v1 close to v2 or in the same place. Assumes that DCEL correct, but can have vertices in the same place.
    * Merge allowed only on planar DCEL's, since on usual DCEL winding order after merge undefined
    * */
  def mergeVertices(v1: Vertex[D], v2: Vertex[D], newFaceDataProvider: NewFaceDataProvider[D]): Unit =
    if (v1.incidentEdge.nonEmpty && v2.incidentEdge.nonEmpty) {
      //for(e <- v1.edgeTo(v2)) deleteEdge(e) //Don't do it like that,  since deleteEdge can delete face that shouldn't be deleted  after vertex merge
      val edgeBetween = v1.edgeTo(v2)
      if (edgeBetween.nonEmpty) {
        //maybe implement
        throw new CantDoOpOnDCELException("Cant merge vertices with edge between.")
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
      for (e <- v2.edgesWithOriginHere) e._origin = v1 //before link rewrite, change's only position making DCEL temporary malformed
      //in v1 origin, in v2 ending
      val (v1o, v2e) = (for (v1o <- v1.edgesWithOriginHere; v2e <- v2.edgesWithEndHere) yield (v1o, v2e)).minBy {
        case (v1o, v2e) => v1o.asSegment.body.angleCCW0to2PI(v2e.asSegment.reverse.body)
      }
      val v1oPrev = v1o.prev
      val v2eNext = v2e.next
      if (v1o.leftFace != v2e.leftFace) throw new CantDoOpOnDCELException("Cant merge vertices, without edges adjanced to same face")
      val faceWeIn = v1o.leftFace

      def redirectEdges(): Unit = {
        v1o._prev = v2e
        v2e._next = v1o

        v1oPrev._next = v2eNext
        v2eNext._prev = v1oPrev
      }

      def mergeRemoveV1OV2E(): Unit = {
        v1o.twin._twin = v2e.twin
        v2e.twin._twin = v1o.twin
        halfEdges -= v1o
        halfEdges -= v2e
        onHalfEdgeRemoved(v1o)
        onHalfEdgeRemoved(v2e)
      }

      def mergeRemoveV1OPrevV2ENext(): Unit = {
        v1oPrev.twin._twin = v2eNext.twin
        v2eNext.twin._twin = v1oPrev.twin
        halfEdges -= v1oPrev
        halfEdges -= v2eNext
        onHalfEdgeRemoved(v1oPrev)
        onHalfEdgeRemoved(v2eNext)
      }

      //fix broken polygon or create new
      /* case 1: v2 in hole of v1o poly
      v1-----------*
      |  v2-----*  |
      |  |      |  |
      |  *______*  |
      *____________*
      case 2:
      v2-----------*
      |  v1-----*  |
      |  |      |  |
      |  *______*  |
      *____________*
      case 3:
      *-----v1      v2------*
      |      |      |       |
      *-----*       *-------*
      case 4: new face
      *-------v1    v2----*
      |        |     \    |
      |        |      \   |
      |        *      *   |
      |         \   /     |
      *----------*--------*
      case 5: no new face, merge edges
      *-------v1    v2------*
      |        \    |       |
      |         \   |       |
      |          \  |       |
      |           \ |       |
      *------------ *-------*
      case 6: delete face
      *----------*----------*
      |         /  \        |
      |        /    \       |
      |      v1      v2     |
      |        \    /       |
      |         \  /        |
      *----------*----------*

       */

      val holeInV1OOpt = v1o.leftFace.holeIncidentEdge(v2e)
      val holeInV2EOpt = v2e.leftFace.holeIncidentEdge(v1o)
      (holeInV1OOpt, holeInV2EOpt) match {
        case (Some(holeEdge), Some(sameEdge)) if holeEdge == sameEdge => //4,5, 6-impossible because v1o and v2e both on holes border??????
          if (v1o.next == v2e && v2eNext.next == v1oPrev) { //6
            throw new MalformedDCELException("Impossible")
          } else if (v1o.next == v2e) { //5
            if (v1.incidentEdge.contains(v1o) || v1.incidentEdge.contains(v2e)) v1._incidentEdge = Some(v1oPrev.twin)
            if (holeEdge == v1o || holeEdge == v2e) {
              v1o.leftFace._holesIncidentEdges -= holeEdge
              v1o.leftFace._holesIncidentEdges += v1oPrev
            }
            mergeRemoveV1OV2E()
          } else if (v2eNext.next == v1oPrev) { //5'
            if (v1.incidentEdge.contains(v1oPrev) || v1.incidentEdge.contains(v2eNext)) v1._incidentEdge = Some(v1oPrev.twin)
            if (holeEdge == v1oPrev || holeEdge == v2eNext) {
              v1o.leftFace._holesIncidentEdges -= holeEdge
              v1o.leftFace._holesIncidentEdges += v1o
            }
            mergeRemoveV1OPrevV2ENext()
          } else { //4
            //Edge chain splitted to two chain one CW(hole) one CCW(new face)
            val v1oFaceEdges = v1o.traverseEdges.takeWhile(_ != v2eNext).toSeq
            val v1oFacePoly = PolygonRegion(v1oFaceEdges.map(_.origin.position))
            if (v1oFacePoly.isCcw) {
              if (v1oFaceEdges.contains(holeEdge)) {
                faceWeIn._holesIncidentEdges -= holeEdge
                faceWeIn._holesIncidentEdges += v1oPrev
              }
              val newFd = newFaceDataProvider.newFaceData(v1o)
              val newFace = makeFace(newFd)
              newFace._incidentEdge = Some(v1o)
              for (e <- v1oFaceEdges) e._leftFace = newFace

              //holes in faceWeIn exists, so check isn't needed, maybe check if faceWeIn.holeIncidentEdges.size > 1
              val (inNewFace, inOldFace) = faceWeIn.holesIncidentEdges.partition(e => v1oFacePoly.containsInside(e.origin.position))
              newFace._holesIncidentEdges = inNewFace
              //fix leftFace for holeEdges
              for (ie <- inNewFace; e <- ie.traverseEdges) e._leftFace = newFace
              faceWeIn._holesIncidentEdges = inOldFace

            } else { //v2eNExtFacePoly should be CCW
              val v2eNextFaceEdges = v2eNext.traverseEdges.takeWhile(_ != v1o)
              if (v2eNextFaceEdges.contains(holeEdge)) {
                faceWeIn._holesIncidentEdges -= holeEdge
                faceWeIn._holesIncidentEdges += v1o
              }
              val newFd = newFaceDataProvider.newFaceData(v2eNext)
              val newFace = makeFace(newFd)
              for (e <- v2eNextFaceEdges) e._leftFace = newFace
              val v2eNextFacePoly = PolygonRegion(v2eNextFaceEdges.map(_.origin.position).toSeq)
              val (inNewFace, inOldFace) = faceWeIn.holesIncidentEdges.partition(e => v2eNextFacePoly.containsInside(e.origin.position))
              newFace._holesIncidentEdges = inNewFace
              //fix leftFace for holeEdges
              for (ie <- inNewFace; e <- ie.traverseEdges) e._leftFace = newFace
              faceWeIn._holesIncidentEdges = inOldFace
            }
          }

        case (Some(holeEdge), Some(otherEdge)) => //3
          v1o.leftFace._holesIncidentEdges -= otherEdge
        case (Some(holeInV1O), None) => //1
          v1o.leftFace._holesIncidentEdges -= holeInV1O
        case (None, Some(holeInV2E)) => //2
          v2e.leftFace._holesIncidentEdges -= holeInV2E
        case (None, None) => //4, 5, 6
          if (v1o.next == v2e && v2eNext.next == v1oPrev) { //6
            if (v1.incidentEdge.contains(v1o) || v1.incidentEdge.contains(v2e) ||
              v1.incidentEdge.contains(v1oPrev) || v1.incidentEdge.contains(v2eNext)) v1._incidentEdge = Some(v1oPrev.twin)
            mergeRemoveV1OV2E()
            mergeRemoveV1OPrevV2ENext()
            if (faceWeIn.holes.nonEmpty) {
              throw new CantDoOpOnDCELException("Some holes left in deleted face")
            }
            innerFaces -= faceWeIn
            onFaceRemoved(faceWeIn)
          } else if (v1o.next == v2e) { //5
            if (v1.incidentEdge.contains(v1o) || v1.incidentEdge.contains(v2e)) v1._incidentEdge = Some(v1oPrev.twin)
            mergeRemoveV1OV2E()
          } else if (v2eNext.next == v1oPrev) { //5'
            if (v1.incidentEdge.contains(v1oPrev) || v1.incidentEdge.contains(v2eNext)) v1._incidentEdge = Some(v1oPrev.twin)
            mergeRemoveV1OPrevV2ENext()
          } else { //4 Because it's not hole's border, we can use either v1o and v2eNext for new face
            val newFd = newFaceDataProvider.newFaceData(v1o)
            val newFaceEdges = v1o.traverseEdges.takeWhile(_ != v2eNext).toSeq
            faceWeIn._incidentEdge = Some(v2eNext) //faster overwrite than check if rewrite needed
            val newFace = makeFace(newFd)
            newFace._incidentEdge = Some(v1o)
            for (e <- newFaceEdges) e._leftFace = newFace
            if (faceWeIn._holesIncidentEdges.nonEmpty) {
              val newFacePoly = PolygonRegion(newFaceEdges.map(_.origin.position))
              val (inNewFace, inOldFace) = faceWeIn.holesIncidentEdges.partition(e => newFacePoly.containsInside(e.origin.position))
              faceWeIn._holesIncidentEdges = inOldFace
              newFace._holesIncidentEdges = inNewFace
              //fix leftFace for holeEdges
              for (ie <- inNewFace; e <- ie.traverseEdges) e._leftFace = newFace
            }
          }

      }

      redirectEdges() //doesn't needed if 6 but who cares

      vertices -= v2
      onVertexRemoved(v2)
    } else if (v1.incidentEdge.nonEmpty) { //v2 - empty, nothing to merge
      vertices -= v2
      onVertexRemoved(v2)
    } else if (v2.incidentEdge.nonEmpty) { // v1 empty, redirect edges with origin at v2
      v1._incidentEdge = v2.incidentEdge
      for (e <- v2.edgesWithOriginHere) e._origin = v1
      vertices -= v2
      onVertexRemoved(v2)
    } else { //both empty
      vertices -= v2
      onVertexRemoved(v2)
    }

  //todo face & heDataPrivider when scala 3
  //todo check
  /** Assumes that there is no intersections for adding edge, so makes no cuts */
  def connectVerticesUnsafe(from: Vertex[D], to: Vertex[D], dataProvider: DCELDataProvider[D]): HalfEdge[D] = {
    val (ld, rd) = dataProvider.newEdgeData(from, to)
    if (from.incidentEdge.isEmpty && to.incidentEdge.isEmpty) {
      val face = faceAt(from.position)
      val res = makeEdge(from, to, face, face, ld, rd)
      face._holesIncidentEdges += res
      res
    } else if (from.incidentEdge.nonEmpty && to.incidentEdge.isEmpty) {
      val face = faceAt(to.position)
      makeEdge(from, to, face, face, ld, rd)
    } else if (from.incidentEdge.isEmpty && to.incidentEdge.nonEmpty) {
      val face = faceAt(from.position)
      makeEdge(from, to, face, face, ld, rd)
    } else {
      val edgeDir = to.position - from.position
      //      val fromFaceEdge = from.edgesWithOriginHere.minBy(e => edgeDir.angleCCW0to2PI(e.asSegment.body))
      val fromFaceEdge = from.edgesWithOriginHere.minBy(e => e.asSegment.body.angleCCW0to2PI(edgeDir))
      val faceWeIn = fromFaceEdge.leftFace
      val toFaceEdge: HalfEdge[D] = to.edgesWithOriginHere.find(_.leftFace == faceWeIn).get

      val next = toFaceEdge
      val prev = fromFaceEdge.prev
      val twinPrev = toFaceEdge.prev
      val twinNext = fromFaceEdge

      val fromBorderContains = faceWeIn.borderEdges.contains(fromFaceEdge)
      val toBorderContains = faceWeIn.borderEdges.contains(toFaceEdge)


      if (fromBorderContains && toBorderContains) {
        val res = makeEdge(from, to, faceWeIn, faceWeIn, ld, rd)
        val newFace = makeFace(dataProvider.newFaceData(res.twin))
        val newFaceEdges = res.twin.traverseEdges.toSeq
        for (e <- newFaceEdges) e._leftFace = newFace
        if (newFaceEdges.contains(faceWeIn.incidentEdge.get)) faceWeIn._incidentEdge = Some(res)
        newFace._incidentEdge = Some(res.twin)
        //split holes
        if (faceWeIn._holesIncidentEdges.nonEmpty) {
          val newFacePoly = PolygonRegion(newFaceEdges.map(_.origin.position))
          val (inNewFace, inOldFace) = faceWeIn.holesIncidentEdges.partition(e => newFacePoly.containsInside(e.origin.position))
          faceWeIn._holesIncidentEdges = inOldFace
          newFace._holesIncidentEdges = inNewFace
        }
        res
      } else if (fromBorderContains && !toBorderContains) {
        // to - hole
        val res = makeEdge(from, to, faceWeIn, faceWeIn, ld, rd)
        for (e <- res.traverseEdges) {
          faceWeIn._holesIncidentEdges -= e
        }
        res

      } else if (!fromBorderContains && toBorderContains) {
        //from - hole
        val res = makeEdge(from, to, faceWeIn, faceWeIn, ld, rd)
        for (e <- res.traverseEdges) {
          faceWeIn._holesIncidentEdges -= e
        }
        res
      } else {
        //both - hole, we may connect two holes, or cut part from hole
        val fromHole = faceWeIn.holesIncidentEdges.find(_.traverseEdges.contains(fromFaceEdge)).get
        val toHole = faceWeIn.holesIncidentEdges.find(_.traverseEdges.contains(toFaceEdge)).get
        val res = makeEdge(from, to, faceWeIn, faceWeIn, ld, rd, Some(prev), Some(next), Some(twinPrev), Some(twinNext))

        if (fromHole == toHole) {
          val resFacePoly = PolygonRegion(res.traverseEdges.map(_.origin.position).toSeq)
          if (resFacePoly.isCcw) { //res is cut part and new poly
            val newFace = makeFace(dataProvider.newFaceData(res))
            newFace._incidentEdge = Some(res)
            for (e <- res.traverseEdges) {
              e._leftFace = newFace
              if (faceWeIn.holesIncidentEdges.contains(e)) {
                faceWeIn._holesIncidentEdges -= e
                faceWeIn._holesIncidentEdges += res.twin
              }
            }
            val newFacePoly = PolygonRegion(newFace.vertices.map(_.position).toSeq)
            val (inNewFace, inOldFace) = faceWeIn.holesIncidentEdges.partition(e => newFacePoly.containsInside(e.origin.position))
            faceWeIn._holesIncidentEdges = inOldFace
            newFace._holesIncidentEdges = inNewFace

          } else { //res.twin is cut part and new poly
            val newFace = makeFace(dataProvider.newFaceData(res.twin))
            newFace._incidentEdge = Some(res.twin)
            for (e <- res.twin.traverseEdges) {
              e._leftFace = newFace
              if (faceWeIn.holesIncidentEdges.contains(e)) {
                faceWeIn._holesIncidentEdges -= e
                faceWeIn._holesIncidentEdges += res
              }
            }
            val newFacePoly = PolygonRegion(newFace.vertices.map(_.position).toSeq)
            val (inNewFace, inOldFace) = faceWeIn.holesIncidentEdges.partition(e => newFacePoly.containsInside(e.origin.position))
            faceWeIn._holesIncidentEdges = inOldFace
            newFace._holesIncidentEdges = inNewFace
          }

          res
        } else {
          //val res = makeEdge(from, to, faceWeIn, faceWeIn, ld, rd, Some(prev), Some(next), Some(twinPrev), Some(twinNext))
          faceWeIn._holesIncidentEdges -= toHole
          res
        }
      }
    }
  }


  /** untested */
  def insert[O <: DCELData](ot: DCEL[O],
                            mapVertexData: Vertex[O] => D#VertexData,
                            mapHalfEdgeData: HalfEdge[O] => D#HalfEdgeData,
                            mapFaceData: Face[O] => D#FaceData,
                            mergeVerticesAtSamePosition: Boolean,
                            newFaceDataProvider: NewFaceDataProvider[D]): Unit = {
    val newFaces: mutable.Map[Face[O], Face[D]] = mutable.Map()
    val newHalfEdges: mutable.Map[HalfEdge[O], HalfEdge[D]] = mutable.Map()
    val newVertices: mutable.Map[Vertex[O], Vertex[D]] = mutable.Map()
    for (v <- ot.vertices) {
      val v1 = makeVertex(mapVertexData(v))
      newVertices += v -> v1
    }
    for (f <- ot.innerFaces) {
      val f1 = makeFace(mapFaceData(f))
      newFaces += f -> f1
    }
    for (he <- ot.halfEdges) {
      val edgeFace = if (he.leftFace == ot.outerFace) outerFace else newFaces(he.leftFace)
      val origin = newVertices(he._origin)
      val he1 = new HalfEdge[D](mapHalfEdgeData(he), origin, null, edgeFace, null, null)
      newHalfEdges += he -> he1
      if (origin.incidentEdge.isEmpty) {
        origin._incidentEdge = Some(he1)
      }
    }
    for (he <- ot.halfEdges) {
      val newHe = newHalfEdges(he)
      newHe._twin = newHalfEdges(he._twin)
      newHe._prev = newHalfEdges(he._prev)
      newHe._next = newHalfEdges(he._next)
      onNewHalfEdge(newHe)
    }
    for (outerEdge <- ot.outerFace._holesIncidentEdges) {
      outerFace._holesIncidentEdges += newHalfEdges(outerEdge)
    }
    if (mergeVerticesAtSamePosition) {
      for (v <- ot.vertices;
           nv = newVertices(v);
           toMerge <- getVertex(nv.position)) {
        mergeVertices(toMerge, nv, newFaceDataProvider)
      }
    }

  }

  def planarSanityCheck(): Unit = {
    for (e1 <- halfEdges; e2 <- halfEdges if e1 != e2 && e1.twin != e2) if (e1.asSegment.haveSegmentIntersection(e2.asSegment))
      throw new MalformedDCELException(s"$e1 intersects body of $e2 | ${e1.asSegment} ${e2.asSegment}")
    for (f <- innerFaces if f.vertices.size >= 3) if (!f.polygon.isCcw)
      throw new MalformedDCELException(s"$f not in ccw order")
    for (f <- innerFaces; h <- f.holesContours.toSeq.map(_.toSeq) if h.toSeq.size >= 3; p = PolygonRegion(h.map(_.origin.position).toSeq) if p.isCcw)
      throw new MalformedDCELException(s"Hole border in ccw order in $f ${h.toSeq.head}")
    for (h <- outerFace.holesContours.toSeq.map(_.toSeq) if h.size >= 3; p = PolygonRegion(h.map(_.origin.position).toSeq) if p.isCcw)
      throw new MalformedDCELException(s"Hole border in ccw order in outerFace ${h.toSeq.head}")
    if (outerFace.incidentEdge.nonEmpty)
      throw new MalformedDCELException(s"Outer face incidentEdge should be empty.")
  }
}
