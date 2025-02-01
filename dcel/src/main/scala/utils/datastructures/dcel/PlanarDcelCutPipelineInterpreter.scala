package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL._
import utils.datastructures.dcel.PlanarDCELCutPipeline._

import scala.annotation.tailrec
import scala.collection.mutable

object PlanarDcelCutPipelineInterpreter {



  def cutPipeline[VD, HD, FD, VL, HL, FL](at: PlanarDCEL[VD, HD, FD],
                                              provider: DCELDataProvider[VD, HD, FD],
                                              pipeline: PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]): CuttingContext[VD, HD, FD, VL, HL, FL] = {
    val context = CuttingContext[VD, HD, FD, VL, HL, FL](at, provider)
    cutPipelineWithContext(pipeline, context)
  }

  def cutPipelineWithContext[VD, HD, FD, VL, HL, FL](
                                                          pipeline: PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL],
                                                          context: CuttingContext[VD, HD, FD, VL, HL, FL]): CuttingContext[VD, HD, FD, VL, HL, FL] = {
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
        PlanarDCELCutOps.withContextChangesTracking[VD, HD, FD, VL, HL, FL](c => {code.apply(c);c}, context)
      case ForEachFace(selector, doWith) =>
        PlanarDCELCutOps.withContextChangesTracking[VD, HD, FD, VL, HL, FL](c => {for (f <- selector(c)) doWith(f); c}, context)
      case ForEachEdge(selector, doWith) =>
        PlanarDCELCutOps.withContextChangesTracking[VD, HD, FD, VL, HL, FL](c => {for (he <- selector(c)) doWith(he); c}, context)
      case ForEachVertex(selector, doWith) =>
        PlanarDCELCutOps.withContextChangesTracking[VD, HD, FD, VL, HL, FL](c => {for (v <- selector(c)) doWith(v); c}, context)
      case t: TraceSegmentAtAngle[VD, HD, FD, VL, HL, FL] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.traceSegmentAtAngle(t, _), context)
      case c: CutPoly[VD, HD, FD, VL, HL, FL] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.cutPoly(c, _), context)
      case c: CutChain[VD, HD, FD, VL, HL, FL] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.cutChain(c, _), context)
      case m: MergeFaces[VD, HD, FD, VL, HL, FL] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.mergeFaces(m, _), context)
      case c: ConnectVertices[VD, HD, FD, VL, HL, FL] =>
        PlanarDCELCutOps.withContextChangesTracking(PlanarDCELCutOps.connectVertices(c, _), context)
    }
  }
}
