package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.DCELData
import utils.datastructures.dcel.PlanarDCELCutPipeline._
import utils.math.planar.{SegmentPlanar, V2}

trait PlanarDCELCutPipelineSelectors {

  //Selectors
  def SelectFacesByLabel[VD, HD, FD, VL, HL, FL](label: FL): FaceSelector[VD, HD, FD, VL, HL, FL] =
    ctx => ctx.labelToFace.getOrElse(label, Set()).toSeq
  def SelectEdgesByLabel[VD, HD, FD, VL, HL, FL](label: HL): EdgeSelector[VD, HD, FD, VL, HL, FL] =
    ctx => ctx.labelToHalfEdge.getOrElse(label, Set()).toSeq
  def SelectVerticesByLabel[VD, HD, FD, VL, HL, FL](label: VL): VertexSelector[VD, HD, FD, VL, HL, FL] =
    ctx => ctx.labelToVertex.getOrElse(label, Set()).toSeq
  def SelectEdgesBetween[VD, HD, FD, VL, HL, FL](label1: FL, label2: FL): EdgeSelector[VD, HD, FD, VL, HL, FL] =
    ctx => (SelectFaceEdges(label1)(ctx).toSet & SelectFaceEdges(label2)(ctx).map(_.twin).toSet).toSeq
  def SelectFaceEdges[VD, HD, FD, VL, HL, FL](label: FL): EdgeSelector[VD, HD, FD, VL, HL, FL] =
    ctx => SelectFacesByLabel(label)(ctx).flatMap(_.edges)
  def SelectEdgesInSegment[VD, HD, FD, VL, HL, FL](seg: SegmentPlanar): EdgeSelector[VD, HD, FD, VL, HL, FL] =
    ctx => ctx.halfEdgesProduced.filter(e => seg.containsSegment(ctx.dcel.asSegment(e))).toSeq
  def SelectEdgesEqualToSegmentAndSameDirection[VD, HD, FD, VL, HL, FL](seg: SegmentPlanar): EdgeSelector[VD, HD, FD, VL, HL, FL] =
    ctx => ctx.halfEdgesProduced.filter(e => (ctx.dcel.position(e.origin) ~= seg.start) && (ctx.dcel.position(e.ending) ~= seg.end)).toSeq
  def SelectEdgesContainsPoint[VD, HD, FD, VL, HL, FL](p: V2): EdgeSelector[VD, HD, FD, VL, HL, FL] =
    ctx => ctx.halfEdgesProduced.filter(e => ctx.dcel.asSegment(e).contains(p)).toSeq
  def SelectOneEdge[VD, HD, FD, VL, HL, FL](s: EdgeSelector[VD, HD, FD, VL, HL, FL]): SingleEdgeSelector[VD, HD, FD, VL, HL, FL] =
    ctx => s.apply(ctx).headOption


}
