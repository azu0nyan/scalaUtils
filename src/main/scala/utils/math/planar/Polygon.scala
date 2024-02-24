package utils.math.planar

import java.util.GregorianCalendar
import utils.datastructures.spatial.AARectangle
import utils.math.Scalar
import utils.math.planar.Polygon.PolygonOps
import utils.math.planar.patch.Path
import utils.math.planar.patch.Path.Path


object Polygon {

  val empty: Polygon = {
    val s: Seq[PolygonRegion] = Seq()
    Polygon(s)
  }

  def from(p: Path, pointsCount: Int): Polygon = Polygon(Seq(PolygonRegion(p.toPoints(pointsCount))))

  def from(s: Seq[V2]): Polygon = Polygon(Seq(PolygonRegion(s)))

  //  def from(s:Seq[Path]):Polygon = Polygon.from(Path.toPoints(s, 10, false))

  def from(s: PolygonRegion): Polygon = Polygon(Seq(s))

  def getCommonParent(regions: Iterable[PolygonRegion]): Option[PolygonRegion] = regions.find(r => regions.forall(c => r.contains(c)))

  type OuterAndInnerPair = (PolygonRegion, PolygonRegion)

  def getContainingPairs(regions: Iterable[PolygonRegion]): Iterator[OuterAndInnerPair] =
    regions.toSeq.combinations(2).flatMap(p =>
      if (p(0).contains(p(1))) {
        Some(p(0), p(1))
      } else if (p(1).contains(p(0))) {
        Some(p(1), p(0))
      } else {
        None
      }
    )

  /**
   *
   * @param regions non intersecting by sides regions
   * @param outerSign -1 for ccw with Y-up, 1 for cw
   * @return
   */

  def fixOrdersForHoles(regions: Seq[PolygonRegion], outerSign: Int): Seq[PolygonRegion] =
    try {
      fixOrdersForHolesRec(regions.sortBy(-_.area), outerSign)
    } catch {
      case ex: RuntimeException => ex.printStackTrace()
        regions
    }

  private def fixOrdersForHolesRec(sorted: Seq[PolygonRegion], outerSign: Int): Seq[PolygonRegion] = sorted match {
    case Nil => Nil
    case outer :: Nil => Seq(outer.matchSign(outerSign))
    case outer :: rest =>
      val (contained, notContained) = rest.partition(p => outer.contains(p))
      Seq(outer.matchSign(outerSign)) ++ fixOrdersForHoles(contained, outerSign * -1) ++ fixOrdersForHolesRec(notContained, outerSign)
  }

  case class NestedPolygon(outer: PolygonRegion, holes: Seq[NestedPolygon]) {
    def area: Scalar = outer.area - holes.map(_.area).sum
  }

  /**Split nested polygon to seq of container-hole-container-hole-etc*/
  def partitionNested(nested: Seq[NestedPolygon]): Seq[Seq[PolygonRegion]] = {
    val curStep: Seq[PolygonRegion] = nested.map(_.outer)
    val nextStep: Seq[NestedPolygon] = nested.flatMap(_.holes)
    if(nextStep.nonEmpty ) curStep +: partitionNested(nextStep)
    else Seq(curStep)
  }

  def toNestedPolygons(regs: Seq[PolygonRegion]): Seq[NestedPolygon] = {
    toNestedPolygonsRec(regs.sortBy(-_.area))
  }

  def toNestedPolygonsRec(sorted: Seq[PolygonRegion]): Seq[NestedPolygon] = sorted match {
    case Nil => Nil
    case outer :: Nil => Seq(NestedPolygon(outer, Seq()))
    case outer :: rest =>
      val (contained, notContained) = rest.partition(p => outer.contains(p))
      val holes = toNestedPolygonsRec(contained)
      val other = toNestedPolygonsRec(notContained)
      NestedPolygon(outer, holes) +: other
  }

  def toRegionsAndHoles(regions: Seq[PolygonRegion]): Seq[(PolygonRegion, Seq[PolygonRegion])] = {
    toNestedPolygons(regions).flatMap(toRegionsAndHolesNested)
  }

  def toRegionsAndHolesNested(regions: Seq[NestedPolygon]): Seq[(PolygonRegion, Seq[PolygonRegion])] = {
    regions.flatMap(toRegionsAndHolesNested)
  }

  def toRegionsAndHolesNested(nestedPolygon: NestedPolygon): Seq[(PolygonRegion, Seq[PolygonRegion])] =
    (nestedPolygon.outer.matchSign(1), nestedPolygon.holes.map(_.outer.matchSign(-1))) +: nestedPolygon.holes.flatMap(hole => toRegionsAndHolesNested(hole.holes))

  private def toRegionsAndHolesRec(sorted: Seq[PolygonRegion]): Seq[(PolygonRegion, Seq[PolygonRegion])] = sorted match {
    case Nil => Nil
    case outer :: Nil => Seq((outer, Nil))
    case outer :: rest =>
      val (outerHoles, others) = rest.partition(p => p.areaSign < 0 && outer.contains(p))
      (outer, outerHoles) +: toRegionsAndHolesRec(others)
  }

  trait Polygonable {
    def toPolygon: Polygon
  }

  trait PolygonOps[MYTYPE <: PolygonOps[MYTYPE]] extends TransformablePlanar[MYTYPE] {

    def regions: Seq[PolygonRegion]

    def aabb: AARectangle = regions.map(_.aabb).reduceOption((b1, b2) => b1.combine(b2)).getOrElse(AARectangle.ZERO_EMPTY)
    /**
     *
     */
    def intersectingRegions: Iterator[(PolygonRegion, PolygonRegion)] = regions.combinations(2).filter(p => p(0).intersects(p(1))).map(p => (p(0), p(1)))

    /*true if no overlapping 'hole' regions */
    lazy val noHoles: Boolean = getContainingPairs(regions).isEmpty


    def combineWith(ot: Polygon): Polygon = Polygon(regions ++ ot.regions)

    /** working correct only if regions sides ont intersected */
    /* def cutHoles: Polygon = {
       if (regions.size > 1) {
         //  PolygonWithoutHoles(regions)
         var res: Seq[PolygonRegion] = Seq()
         var toCheck = regions.sortBy(_.area)
         while (toCheck.nonEmpty) {
           var outer = toCheck.head //biggest region
           //regions biggest contain//regions to check in next step
           var (contained, skipped) = toCheck.tail.partition(outer.contains)
           while (contained.nonEmpty) {
             //find pair of vertices whose connection not intersect any sides, will be found biggest(and outermost hole) sine they are sorted by area
             val (cutRegion, idiv, idov) = (for (
               reg <- contained;
               (innerV, idiv) <- reg.vertices.iterator.zipWithIndex;
               (outerV, idov) <- outer.vertices.iterator.zipWithIndex;
               seg <- Some(SegmentPlanar(outerV, innerV))
               if contained.forall(_.notContainsOrIntersects(seg))
             ) yield (reg, idiv, idov))
               .headOption.getOrElse((contained.head, 0, 0)) //shouldn't happened if input correct

             outer = outer.mergeVerticesWithAt(idov, idiv, true, if (cutRegion.areaSign == outer.areaSign) outer.reverse.vertices else outer.vertices)

             //remove cutted hole and
             val part2 = contained.filter(_ != cutRegion).partition(outer.contains)
             contained = contained ++ part2._1
             skipped = skipped ++ part2._2
           }

           res = outer +: res
           toCheck = skipped
         }

         Polygon(res)
       } else {
         Polygon(regions)
       }
     }*/
  }

}

/**
 * Y-up coordinates system
 * outermost CCW, holes CW, can be CUT to eliminate holes
 */
case class Polygon(override val regions: Seq[PolygonRegion]) extends PolygonOps[Polygon] {
  def vertices:Seq[V2] = regions.flatMap(_.vertices)

  def addRegion(region: PolygonRegion): Polygon = Polygon(region +: regions)

  lazy val (containers, holes) = regions.partition(_.isCcw)
  //todo check usage

  def area: Scalar = containers.map(_.area).sum - holes.map(_.area).sum

  def contains(point: V2): Boolean = containers.count(_.contains(point)) > holes.count(_.containsInside(point))

  def reverse: Polygon = Polygon(regions.map(_.reverse))

  override def map(f: V2 => V2): Polygon = Polygon(regions.map(_.map(f)))

  def sides: Seq[SegmentPlanar] = regions.flatMap(_.sides)

  def asSeq:Seq[Seq[V2]] = regions.map(_.vertices)
  
  def average: V2 = vertices.reduce(_ + _) / vertices.size

}

case class PolygonWithoutHoles(override val regions: Seq[PolygonRegion]) extends PolygonOps[PolygonWithoutHoles] {

  override def map(f: V2 => V2): PolygonWithoutHoles = PolygonWithoutHoles(regions.map(_.map(f)))
}


