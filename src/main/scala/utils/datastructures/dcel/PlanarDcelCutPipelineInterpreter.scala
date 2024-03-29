package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL._
import utils.datastructures.dcel.PlanarDCELCutPipeline._

import scala.annotation.tailrec
import scala.collection.mutable

object PlanarDcelCutPipelineInterpreter {



  def cutPipeline[D <: DCELData, L <: Labels](at: PlanarDCEL[D],
                                              provider: DCELDataProvider[D],
                                              pipeline: PlanarDCELCutPipeline[D, L]): CuttingContext[D, L] = {
    val context = CuttingContext[D, L](at, provider)
    cutPipelineWithContext(pipeline, context)
  }

  def cutPipelineWithContext[D <: DCELData, L <: Labels](
                                                          pipeline: PlanarDCELCutPipeline[D, L],
                                                          context: CuttingContext[D, L]): CuttingContext[D, L] = {
    pipeline match {
      case ProducePipeline(producer) => cutPipelineWithContext(producer(context), context)
      case TransformContext(transformer) => transformer(context)
      case StepsSeq(steps@_*) =>
        steps.foldLeft(context) { case (context, step) =>
          cutPipelineWithContext(step, context)
        }
      case If(pred, doThen) =>
        if (pred(context)) cutPipelineWithContext(doThen, context)
        else context
      case IfElse(pred, doThen, doElse) =>
        if (pred(context)) cutPipelineWithContext(doThen, context)
        else cutPipelineWithContext(doElse, context)
      case Empty() =>
        context
      case Do(code) =>
        PlanarDCELCutOps.withContextChangesTracking[D, L](c => {code.apply(c);c}, context)
      case ForEachFace(selector, doWith) =>
        PlanarDCELCutOps.withContextChangesTracking[D, L](c => {for (f <- selector(c)) doWith(f); c}, context)
      case ForEachEdge(selector, doWith) =>
        PlanarDCELCutOps.withContextChangesTracking[D, L](c => {for (he <- selector(c)) doWith(he); c}, context)
      case ForEachVertex(selector, doWith) =>
        PlanarDCELCutOps.withContextChangesTracking[D, L](c => {for (v <- selector(c)) doWith(v); c}, context)
      case t: TraceSegmentAtAngle[D, L] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.traceSegmentAtAngle(t, _), context)
      case c: CutPoly[D, L] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.cutPoly(c, _), context)
      case c: CutChain[D, L] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.cutChain(c, _), context)
      case m: MergeFaces[D, L] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.mergeFaces(m, _), context)
      case c: ConnectVertices[D, L] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.connectVertices(c, _), context)
    }
  }
}
