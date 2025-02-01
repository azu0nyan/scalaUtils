package utils.math.planar.algo

import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge, Vertex}
import utils.datastructures.dcel.{DCELDataProvider, PlanarDCEL}
import utils.math.planar.algo.PolygonTriangulation.triangulateNoneHolesDCEL
import utils.math.planar.algo.polygonClipping.{PolygonClipping, PolygonClippingImplicits}
import utils.math.planar.{Polygon, PolygonRegion, SegmentPlanar, V2}

object PolygonToConvex {
  def toConvexPolygons(polygon: Seq[Seq[V2]]): Seq[Seq[V2]] = {
    object provider extends DCELDataProvider[V2, Unit, Unit] {
      override def splitEdgeData(edge: HalfEdge[V2, Unit, Unit], data: V2): (Unit, Unit) = ((), ())
      override def newVertexData(v: V2): V2 = v
      override def newEdgeData(v1: Vertex[V2, Unit, Unit], v2: Vertex[V2, Unit, Unit]): (Unit, Unit) = ((), ())
      override def newFaceData(edge: HalfEdge[V2, Unit, Unit]): Unit = ()
    }

    val d = new PlanarDCEL[V2, Unit, Unit]((), x => x)
    for (p <- polygon) d.cutPoly(p, provider)

    for (h <- d.nonHoleFaces;
         f <- toConvexPolygonsDCEL(d, h, provider)) yield f.vertices.map(d.position).toSeq
  }

  def toConvexPolygonsDCEL[VD, HD, FD](dcel: PlanarDCEL[VD, HD, FD], face: Face[VD, HD, FD], provider: DCELDataProvider[VD, HD, FD]): Seq[Face[VD, HD, FD]] = {
    val initialPoly = PolygonRegion(face.vertices.map(dcel.position).toSeq)
    if (initialPoly.isConvex) Seq(face)
    else {
      implicit def faceToRegion(f: Face[VD, HD, FD]): PolygonRegion = PolygonRegion(f.vertices.map(dcel.position).toSeq)
      implicit def edgeToSegment(e: HalfEdge[VD, HD, FD]): SegmentPlanar = SegmentPlanar(dcel.position(e.origin), dcel.position(e.ending))
      implicit def vertexToV2(e: Vertex[VD, HD, FD]): V2 = dcel.position(e)

      val monotones: Seq[Face[VD, HD, FD]] = PolygonTriangulation.monotonePartitionDCELFace(dcel, face, provider)
      val (convexMonotones, nonConvexMonotones): (Seq[Face[VD, HD, FD]], Seq[Face[VD, HD, FD]]) = monotones.partition(m => PolygonRegion(m.vertices.map(dcel.position).toSeq).isConvex)

      def canMergeToConvex(f1:Face[VD, HD, FD], f2:Face[VD, HD, FD]): Boolean = {
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

      def tryMergeToConvex(s: Seq[Face[VD, HD, FD]]): Seq[Face[VD, HD, FD]] = if (s.size > 1) {
        var facesLeft: Set[Face[VD, HD, FD]] = s.toSet
        var fixedFaces: Set[Face[VD, HD, FD]] = Set()
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
