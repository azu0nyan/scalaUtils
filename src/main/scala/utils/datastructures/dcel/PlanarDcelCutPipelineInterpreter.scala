package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.DCELData
import utils.datastructures.dcel.PlanarDCELCutPipeline._

import scala.annotation.tailrec

object PlanarDcelCutPipelineInterpreter {
  def cutPipeline[D <: DCELData, L <: Labels](at: PlanarDCEL[D],
                                              provider: DCELDataProvider[D],
                                              pipeline: PlanarDCELCutPipeline[D, L]): CuttingContext[D, L] = {
    val context = CuttingContext(at, provider)
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
        code.apply(context)
        context
      case ForEachFace(selector, doWith) =>
        for (f <- selector(context)) doWith(f)
        context
      case ForEachEdge(selector, doWith) =>
        for (he <- selector(context)) doWith(he)
        context
      case ForEachVertex(selector, doWith) =>
        for (v <- selector(context)) doWith(v)
        context
      case t: TraceSegmentAtAngle[D, L] =>
        PlanarDCELCutOps.traceSegmentAtAngle(t, context)
      case c: CutPoly[D, L] =>
        PlanarDCELCutOps.cutPoly(c, context)
      case c: CutChain[D, L] =>
        PlanarDCELCutOps.cutChain(c, context)
      case m: MergeFaces[D, L] =>
        PlanarDCELCutOps.mergeFaces(m, context)
      case c: ConnectVertices[D, L] =>
        PlanarDCELCutOps.connectVertices(c, context)
    }
  }
}
