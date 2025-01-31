package utils.math.planar.algo.polygonClipping

import utils.math.SMALL_NUMBER

object PolyBool {

  case class PolySegmentsSoup(segments: Iterable[Segment], inverted: Boolean)

  def segments(poly: Poly)(implicit epsilon: Epsilon): PolySegmentsSoup = {
    val i = new Intersecter(true)
    poly.regions.filter(_.nonEmpty).foreach(i.addRegion)
    return PolySegmentsSoup(i.calculate(poly.inverted), poly.inverted)
  }

  case class CombineResult(segments: Iterable[Segment], inverted1: Boolean, inverted2: Boolean)

  def combine(segments1: PolySegmentsSoup, segments2: PolySegmentsSoup)(implicit epsilon: Epsilon): CombineResult = {
    var i3 = new Intersecter(false);
    // performing combination of polygons, so only deal with already-processed segments
    // segmentsX come from the self-intersection API, or this API
    // invertedX is whether we treat that list of segments as an inverted polygon or not
    // returns segments that can be used for further operations
    segments1.segments.map(s => s.segmentCopy(s.start, s.end)).foreach(i3.eventAddSegment(_, true))
    segments2.segments.map(s => s.segmentCopy(s.start, s.end)).foreach(i3.eventAddSegment(_, false))
    val intersected = i3.calculate(segments1.inverted, segments2.inverted);
    return CombineResult(intersected, segments1.inverted, segments2.inverted)
  }

  def selectUnion(combined: CombineResult): PolySegmentsSoup = new PolySegmentsSoup(
    Selector.union(combined.segments),
    combined.inverted1 || combined.inverted2
  )


  def selectIntersecttion(combined: CombineResult): PolySegmentsSoup = new PolySegmentsSoup(
    Selector.intersect(combined.segments),
    combined.inverted1 && combined.inverted2
  )


  def selectDifference(combined: CombineResult): PolySegmentsSoup = new PolySegmentsSoup(
    Selector.difference(combined.segments),
    combined.inverted1 && !combined.inverted2
  )

  def selectDifferenceRev(combined: CombineResult): PolySegmentsSoup = new PolySegmentsSoup(
    Selector.differenceRev(combined.segments),
    !combined.inverted1 && combined.inverted2
  )

  def selectXor(combined: CombineResult): PolySegmentsSoup = new PolySegmentsSoup(
    Selector.xor(combined.segments),
    combined.inverted1 != combined.inverted2
  )

  // helper functions for common operations
  def union(poly1: Poly, poly2: Poly)(implicit epsilon: Epsilon): Poly = {
    return operate(poly1, poly2, PolyBool.selectUnion);
  }

  def intersect(poly1: Poly, poly2: Poly)(implicit epsilon: Epsilon): Poly = {
    return operate(poly1, poly2, PolyBool.selectIntersecttion);
  }

  def difference(poly1: Poly, poly2: Poly)(implicit epsilon: Epsilon): Poly = {
    return operate(poly1, poly2, PolyBool.selectDifference);
  }

  def differenceRev(poly1: Poly, poly2: Poly)(implicit epsilon: Epsilon): Poly = {
    return operate(poly1, poly2, PolyBool.selectDifferenceRev);
  }

  def xor(poly1: Poly, poly2: Poly)(implicit epsilon: Epsilon): Poly = {
    return operate(poly1, poly2, PolyBool.selectXor);
  }

  def polygon(segments:PolySegmentsSoup)(implicit epsilon: Epsilon) :Poly= {

    new Poly(
      Chainer(segments.segments),
      segments.inverted
    );
  }
  def operate(poly1: Poly, poly2: Poly, selector: CombineResult => PolySegmentsSoup)(implicit epsilon: Epsilon): Poly = {
    val seg1 = PolyBool.segments(poly1);
    val seg2 = PolyBool.segments(poly2);
    val comb = PolyBool.combine(seg1, seg2);
    val seg3 = selector(comb);
    return PolyBool.polygon(seg3);
  }

}
