package utils.math.planar

import utils.math.space.{Plane, V3}
import utils.math._
import utils.math.planar.PolygonRegion.PolygonRegionOps
import utils.math.planar.PolygonalChain.PolygonalChainOps
import utils.math.planar.PointList.PointList
import utils.math.planar.patch.Path.Path

object PolygonRegion {

  final val BORDER = 0
  final val OUTSIDE = -1
  final val INSIDE = 1

  def from(p: Path, vertices: Int): PolygonRegion = PolygonRegion(p.toPoints(vertices))


  trait PolygonRegionOps[MYTYPE <: PolygonRegionOps[MYTYPE]] extends PolygonalChainOps[MYTYPE] {


    lazy val center: V2 = vertices.reduce(_ + _) * (1d / vertices.size)

    def offsetInside(value: Scalar): PolygonRegion = PolygonRegion(
      (vertices.last +: vertices :+ vertices.head).sliding(3).map {
        case Seq(p, c, n) => AngleOps.offsetLeft(p, c, n, value)
      }.toSeq
    )

    final override def closed: Boolean = true

    def areaSign: Int = math.signum(areaSigned).toInt

    def areaSigned: Scalar = sides.map(fs => (fs.v1.x + fs.v2.x) * (fs.v1.y - fs.v2.y)).sum / 2.0

    /**
      * todo tmp func
      *
      * @return
      */
    def isCw: Boolean = areaSigned > 0
    def isCcw: Boolean = areaSigned < 0

    def area: Scalar = Math.abs(areaSigned)

    def isConvex: Boolean = sideAngles.forall { case AngleCCWPlanar(l, c, r) => AngleOps.turnAngleCCW02PI(l, c, r) ~<= PI}

    def distanceTo(point: V2): Scalar = if (contains(point)) 0 else distanceToFromSides(point)

    //    def


    def notContains(p: V2): Boolean = classify(p) == OUTSIDE
    def onBorder(p: V2): Boolean = classify(p) == BORDER
    def containsInside(p: V2): Boolean = classify(p) == INSIDE
    def contains(p: V2): Boolean = classify(p) >= 0

    /**
      *
      * @param p
      * BORDER = 0
      * OUTSIDE = -1
      * INSIDE = 1
      * @return
      */
    def classify(p: V2): Int = if (vertices.isEmpty) OUTSIDE
    else if (vertices.length == 1) if (vertices.head ~= p) BORDER else OUTSIDE
    else if (vertices.length == 2) if (SegmentPlanar(vertices.head, vertices.last).contains(p)) BORDER else OUTSIDE
    else {
      var res = 0
      for (side <- sides) {

        val v1 = side.v1
        val v2 = side.v2
        //side not horizontal and we can intersect it
        //        if((v1.y ~> p.y) != (v2.y ~> p.y)){
        if (v1.y !~= v2.y) {
          if ((v1.y ~<= p.y) && (p.y ~<= v2.y) || (v2.y ~<= p.y) && (p.y ~<= v1.y)) {
            //intersection x
            //no division by 0 since
            val cx = v1.x + (p.y - v1.y) * (v2.x - v1.x) / (v2.y - v1.y)
            if (cx ~= p.x) return BORDER
            //we intersects
            if (cx > p.x) {
              //simple intersection
              if ((v1.y !~= p.y) && (v2.y !~= p.y)) {
                res += 1
                //side touches ray, other point higher
              } else if ((v1.y ~= p.y) && v2.y > p.y || (v2.y ~= p.y) && v1.y > p.y) {
                res += 1
              }
              //side touching ray other point lower
              /*else if((v2.y ~= p.y) && v1.y > p.y)*/
            }
          }
        } else if ((v1.y ~= p.y) && (min(v1.x, v2.x) ~<= p.x) && (p.x ~<= max(v1.x, v2.x))) return BORDER
      }
      if (res % 2 == 1) INSIDE else OUTSIDE
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

    /** -1 for ccw with Y-up, 1 for cw */
    def matchSign(sign: Int): MYTYPE = if (sign == areaSign) replacePoints(vertices) else this.reverse

    // IN CW order
    def triangulationIndices: Seq[(Int, Int, Int)] = triangulation.map(t => (indexOf(t.v1), indexOf(t.v2), indexOf(t.v3)))

    def toTriangle: TrianglePlanar = TrianglePlanar(vertices(0), vertices(1), vertices(2))

    def toQuad: QuadPlanar = new QuadPlanar(vertices(0), vertices(3), vertices(1), vertices(2))

    def projectIn3d(p: V3, q: V3): Seq[V3] = vertices.map(v => (p * V3(v.x, v.x, v.x)) + q * V3(v.y, v.y, v.y))

    def projectOn(plane: Plane): Seq[V3] = vertices.map(v => plane.toWorldCords(v))

    def withCache: PolygonRegionWithCache = PolygonRegionWithCache(vertices)

    def intersectsOrContainsCircle(center: V2, rad: Scalar): Boolean = contains(center) || sides.map(_.distanceTo(center)).minOption.getOrElse(0d) < rad
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

  def asPolygon: Polygon = Polygon(Seq(this))

  override def replacePoints(vertices: Seq[V2]): PolygonRegion = PolygonRegion(vertices)
}

