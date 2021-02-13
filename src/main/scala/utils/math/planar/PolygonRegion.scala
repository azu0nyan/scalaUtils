package utils.math.planar

import utils.math.space.{Plane, V3}
import utils.math._
import utils.math.planar.PolygonRegion.PolygonRegionOps
import utils.math.planar.PolygonalChain.PolygonalChainOps
import utils.math.planar.PointList.PointList
import utils.math.planar.patch.Path.Path

object PolygonRegion {

  def from(p: Path, vertices: Int): PolygonRegion = PolygonRegion(p.toPoints(vertices))


  trait PolygonRegionOps[MYTYPE <: PolygonRegionOps[MYTYPE]] extends PolygonalChainOps[MYTYPE] {




    lazy  val center:V2 = vertices.reduce(_ + _) * ( 1d / vertices.size)



    final override def closed: Boolean = true

    def areaSign: Int = math.signum(areaSigned).toInt

    def areaSigned: Scalar = sides.map(fs => (fs.v1.x + fs.v2.x) * (fs.v1.y - fs.v2.y)).sum

    /**
     * todo tmp func
     *
     * @return
     */
    def isCw: Boolean = areaSigned > 0

    def area: Scalar = Math.abs(areaSigned)

    def distanceTo(point: V2): Scalar = if (contains(point)) 0 else distanceToFromSides(point)

    def contains(point: V2): Boolean = {
      if (vertices.length < 1) false
      else if (vertices.length == 1) vertices.head ~= point
      else if (vertices.length == 2) SegmentPlanar(vertices.head, vertices.last).collinear(point)
      else {
        val otherEnd: V2 = V2(aabb.max.x + 1000f, point.y)
        val toTest: SegmentPlanar = SegmentPlanar(point, otherEnd)
        var res: Int = 0
        var res2: Int = 0
        for (side <- sides) {
          // println(toTest + " " + side + " " + toTest.intersects(side))
          if (toTest.intersects(side)) {
            res2 = res2 + 1
            if (side.contains(point)) return true
            /*
             The issue is solved as follows: If the intersection point is a vertex of a tested polygon side,
             then the intersection counts only if the second vertex of the side lies below the ray.
            */
            if (
              (side.v1.y == point.y && side.v2.y < point.y)
                ||
                (side.v2.y == point.y && side.v1.y < point.y)
                ||
                (side.v1.y != point.y && side.v2.y != point.y)
            ) {
              res = res + 1
            }
          }
        }

        res % 2 == 1
      }
    }

    //todo  плохо работает для случаев когда ребро совпадает с границей side.v1.y == 0 && side.v2.y == 0
    def contains(s: SegmentPlanar): Boolean = {
      val translation: V2 = s.v1
      val toRotate: Scalar = -s.angleOX
      val len: Scalar = s.length
      //поворачиваем чтобы отрезок оказался на оси ох

      val transformed: MYTYPE = (this - translation).rotate(toRotate)
      //val sTransformed = Segment2(0f, V2(l, 0))
      var l: Int = 0
      var r: Int = 0
      for (side <- transformed.sides) {
        /*
           пересечение с оХ
           в случае касания в точке напр.(у1 = -1 у2 = 0 у3 = 1) только одно из ребер будет обработано
           в случае касания в точке напр.(у1 = -1 у2 = 0 у3 = -1) ниодно из ребер не будет обработано
           в случае касания в точке напр.(у1 = 1 у2 = 0 у3 = 1) оба ребра будут обработаны
           нужная четность соблюдена
         */
        if ((side.v1.y > 0) != (side.v2.y > 0)) {
          val intersection = side.v1.x + side.xProjSigned * (Math.abs(side.v1.y) / Math.abs(side.yProjSigned))
          //пересечение
          if (intersection ~= 0) l += 1
          else if (intersection ~= len) r += 1
          else if (intersection > 0 && intersection < len) return false
          else if (intersection <= 0) l += 1
          else if (intersection >= len) r += 1
        }
      }
      return r % 2 == 1 || l % 2 == 1
    }

    def contains(list: PointList): Boolean = !list.vertices.exists(contains)

    def contains(segs: Seq[SegmentPlanar]): Boolean = segs.forall(contains)

    def contains(poly: PolygonRegion): Boolean = aabb.intersects(poly.aabb) && contains(poly.sides)

    def notContainsOrIntersects(seg: SegmentPlanar): Boolean = (!contains(seg.v1) || !contains(seg.v2)) || !sides.exists(_.intersection(seg) match {
      case Some(value) => value match {
        case PointIntersection(p) => true
        case SegmentIntersection(s) => false
      }
      case None => false
    }
    )

    /** side to side intersections counts */
    def intersects(s: SegmentPlanar): Boolean = contains(s.v1) || contains(s.v2) || sides.exists(_.intersection(s).nonEmpty)

    /** side to side intersections counts */
    def intersects(ot: PolygonRegion): Boolean = ot.vertices.exists(contains) || ot.sides.exists(contains)

    /** worst case O(n^3) */
    def triangulation: Seq[TrianglePlanar] = {
      if (vertices.length < 3) return Seq()
      if (vertices.length == 3) return Seq(TrianglePlanar(vertices(0), vertices(1), vertices(2)))

      sideTriangles.find(tr => tr.nonDegenerate && tr.cw && contains(SegmentPlanar(tr.v3, tr.v1))) match {
        case Some(t) => t +: removeVertex(t.indices._2).triangulation
        case _ => Seq()
      }
    }

    /** call only if polygon contains `hole` */
    def cutHoleUnsafe(hole: PolygonRegion): MYTYPE = {
      val holeVs = if (areaSign == hole.areaSign) hole.vertices.reverse else hole.vertices
      val verticesToConnect = (for (
        (m, idm) <- vertices.iterator.zipWithIndex;
        (h, idh) <- holeVs.iterator.zipWithIndex
        if {
          val s = SegmentPlanar(m, h)
          hole.contains(s) && contains(s)
        }) yield (idm, idh))
        .nextOption().getOrElse((0, 0)) //combine by first vertices
      mergeVerticesWithAt(verticesToConnect._1, verticesToConnect._2, true, holeVs)
    }

    def matchSign(sign: Int): MYTYPE = if (sign == areaSign) replacePoints(vertices) else this.reverse

    // IN CW order
    def triangulationIndices: Seq[(Int, Int, Int)] = triangulation.map(t => (indexOf(t.v1), indexOf(t.v2), indexOf(t.v3)))

    def toTriangle: TrianglePlanar = TrianglePlanar(vertices(0), vertices(1), vertices(2))

    def toQuad: QuadPlanar = new QuadPlanar(vertices(0), vertices(3), vertices(1), vertices(2))

    def projectIn3d(p: V3, q: V3): Seq[V3] = vertices.map(v => (p * V3(v.x, v.x, v.x)) + q * V3(v.y, v.y, v.y))

    def projectOn(plane: Plane): Seq[V3] = vertices.map(v => plane.toWorldCords(v))

    def withCache: PolygonRegionWithCache = PolygonRegionWithCache(vertices)
  }


}


case class PolygonRegionWithCache(vertices: Seq[V2]) extends PolygonRegionOps[PolygonRegionWithCache] {

  override lazy val sides: Seq[SegmentPlanar] = super.sides

  override lazy val areaSigned: Scalar = super.areaSigned

  override lazy val area: Scalar = super.area

  override lazy val triangulation: Seq[TrianglePlanar] = super.triangulation

  // IN CW order
  override lazy val triangulationIndices: Seq[(Int, Int, Int)] = super.triangulationIndices


  override def replacePoints(vertices: Seq[V2]): PolygonRegionWithCache = PolygonRegionWithCache(vertices)
}


case class PolygonRegion(vertices: Seq[V2]) extends PolygonRegionOps[PolygonRegion] {



  override def replacePoints(vertices: Seq[V2]): PolygonRegion = PolygonRegion(vertices)
}

