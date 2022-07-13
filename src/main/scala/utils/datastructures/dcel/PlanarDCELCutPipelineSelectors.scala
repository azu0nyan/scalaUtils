package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.DCELData
import utils.datastructures.dcel.PlanarDCELCutPipeline._
import utils.math.planar.{SegmentPlanar, V2}

trait PlanarDCELCutPipelineSelectors {

  //Selectors
  def SelectFacesByLabel[D <: DCELData, L <: Labels](label: L#FaceLabel): FaceSelector[D, L] =
    ctx => ctx.areaLabels.getOrElse(label, Set()).toSeq
  def SelectEdgesByLabel[D <: DCELData, L <: Labels](label: L#HalfEdgeLabel): EdgeSelector[D, L] =
    ctx => ctx.edgesLabels.getOrElse(label, Set()).toSeq
  def SelectVerticesByLabel[D <: DCELData, L <: Labels](label: L#VertexLabel): VertexSelector[D, L] =
    ctx => ctx.vertexLabels.getOrElse(label, Set()).toSeq
  def SelectEdgesBetween[D <: DCELData, L <: Labels](label1:L#FaceLabel, label2:L#FaceLabel) : EdgeSelector[D, L] =
    ctx => (SelectFaceEdges(label1)(ctx).toSet & SelectFaceEdges(label2)(ctx).map(_.twin).toSet).toSeq
  def SelectFaceEdges[D <: DCELData, L <: Labels](label: L#FaceLabel) : EdgeSelector[D, L] =
    ctx => SelectFacesByLabel(label)(ctx).flatMap(_.edges)
  def SelectEdgesInSegment[D <: DCELData, L <: Labels](seg:SegmentPlanar) : EdgeSelector[D, L] =
    ctx => ctx.edgesProduced.filter(e => seg.containsSegment(ctx.dcel.asSegment(e)))
  def SelectEdgesContainsPoint[D <: DCELData, L <: Labels](p:V2) : EdgeSelector[D, L] =
    ctx => ctx.edgesProduced.filter(e => ctx.dcel.asSegment(e).contains(p))
  def SelectOneEdge[D <: DCELData, L <: Labels](s: EdgeSelector[D, L]): SingleEdgeSelector[D, L] =
    ctx => s.apply(ctx).headOption


}
