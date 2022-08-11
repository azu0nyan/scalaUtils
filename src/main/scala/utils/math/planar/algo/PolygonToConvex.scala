package utils.math.planar.algo

import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge, Vertex}
import utils.datastructures.dcel.{DCELDataProvider, PlanarDCEL}
import utils.math.planar.{Polygon, PolygonRegion, SegmentPlanar, V2}
import utils.math.planar.algo.PolygonTriangulation.triangulateNoneHolesDCEL
import utils.math.planar.algo.polygonClipping.{PolygonClipping, PolygonClippingImplicits}

object PolygonToConvex {
  def toConvexPolygons(polygon: Seq[Seq[V2]]): Seq[Seq[V2]] = {
    type DATA = DCELData {
      type VertexData = V2
      type HalfEdgeData = Unit
      type FaceData = Unit
    }
    object provider extends DCELDataProvider[DATA] {
      override def splitEdgeData(edge: HalfEdge[DATA], data: V2): (Unit, Unit) = ((), ())
      override def newVertexData(v: V2): V2 = v
      override def newEdgeData(v1: Vertex[DATA], v2: Vertex[DATA]): (Unit, Unit) = ((), ())
      override def newFaceData(edge: HalfEdge[DATA]): Unit = ()
    }

    val d = new PlanarDCEL[DATA]((), x => x)
    for (p <- polygon) d.cutPoly(p, provider)

    for (h <- d.nonHoleFaces;
         f <- toConvexPolygonsDCEL(d, h, provider)) yield f.vertices.map(d.position).toSeq
  }

  def toConvexPolygonsDCEL[D <: DCELData](dcel: PlanarDCEL[D], face: Face[D], provider: DCELDataProvider[D]): Seq[Face[D]] = {
    val initialPoly = PolygonRegion(face.vertices.map(dcel.position).toSeq)
    if (initialPoly.isConvex) Seq(face)
    else {
      implicit def faceToRegion(f: Face[D]): PolygonRegion = PolygonRegion(f.vertices.map(dcel.position).toSeq)
      implicit def edgeToSegment(e: HalfEdge[D]): SegmentPlanar = SegmentPlanar(dcel.position(e.origin), dcel.position(e.ending))
      implicit def vertexToV2(e: Vertex[D]): V2 = dcel.position(e)

      val monotones: Seq[Face[D]] = PolygonTriangulation.monotonePartitionDCELFace(dcel, face, provider)
      val (convexMonotones, nonConvexMonotones): (Seq[Face[D]], Seq[Face[D]]) = monotones.partition(m => PolygonRegion(m.vertices.map(dcel.position).toSeq).isConvex)

      def canMergeToConvex(f1:Face[D], f2:Face[D]): Boolean = {
        /*val ie = f1.incidentEdge.get
        val edges = Iterator.iterate(ie)(e =>
          if(e.leftFace == f1)
            if(e.next.twin.leftFace == f2) e.next.twin.next
            else e.next
          else
            if(e.next.twin.leftFace == f1) e.next.twin.next
            else e.next
        ).takeWhile(_.next != ie).toSeq
        val vs:Seq[V2] = edges.map(_.origin).map(vertexToV2) :+ vertexToV2(edges.last.ending)
        val p = PolygonRegion(vs)
        p.isConvex*/
        val p1 = Polygon(Seq(PolygonRegion(f1.vertices.toSeq.map(vertexToV2))))
        val p2 = Polygon(Seq(PolygonRegion(f2.vertices.toSeq.map(vertexToV2))))

        val res = PolygonClipping.union(p1, p2 )
        res.regions.size == 1 && res.regions.head.isConvex

      }

      def tryMergeToConvex(s: Seq[Face[D]]): Seq[Face[D]] = if (s.size > 1) {
        var facesLeft: Set[Face[D]] = s.toSet
        var fixedFaces: Set[Face[D]] = Set()
        while (facesLeft.nonEmpty) {
          val cur = facesLeft.maxBy(_.area)
          cur.edges.toSeq.sortBy(-_.length).find(he => facesLeft.contains(he.twin.leftFace) && canMergeToConvex(cur, he.twin.leftFace)) match {
            case Some(he) =>
              facesLeft -= he.twin.leftFace
              dcel.mergeAdjancedFaces(cur, he.twin.leftFace, provider)
              //cur stays in facesLeft queue
            case None =>
              facesLeft -= cur
              fixedFaces += cur
          }
        }

        fixedFaces.toSeq
      } else s

      val res = for (nonConvex <- nonConvexMonotones;
                     tr <- tryMergeToConvex(PolygonTriangulation.triangulateMonotoneFace(dcel, nonConvex, provider))) yield tr

      res ++ convexMonotones
    }
  }

}
