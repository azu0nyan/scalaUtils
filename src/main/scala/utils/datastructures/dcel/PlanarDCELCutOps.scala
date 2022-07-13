package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{DCELData, HalfEdge, MalformedDCELException}
import utils.datastructures.dcel.PlanarDCELCutPipeline.{ConnectVertices, CutChain, CutPoly, CuttingContext, Labels, MergeFaces, TraceSegmentAtAngle}
import utils.math.planar.{PointIntersection, SegmentIntersection, SegmentPlanar}
import utils.math.WithAlmostEquals

import scala.collection.mutable

object PlanarDCELCutOps {
  /*                     rayEndOrHitPoint
                        __
                        / \
             lArea     /   rArea
             lEdge    /    rEdge == lEdge.twin
                     /     angleCCW from (at, ending) to (at, rayEndOrHitPoint)
       fHalf        /      sHalf
   *---------------*------------------->
   origin         at              ending

   at = origin + (ending - origin) * ratio
    */
  //todo cound produced
  def traceSegmentAtAngle[D <: DCELData, L <: Labels](t: TraceSegmentAtAngle[D, L], context: CuttingContext[D, L]): CuttingContext[D, L] = {
    t match {
      case TraceSegmentAtAngle(edgeSelector, ratioFromOrigin, maxLength, angleCCW,
      lFaceLabel, rFaceLabel, lEdgeLabel, rEdgeLabel, fHalfLabel, sHalfLabel, atLabel, rayEndOrHitPointLabel) =>
        var ctx = context
        for (edge <- edgeSelector(context)) {
          val edgeSeg = context.dcel.asSegment(edge)
          val at = edgeSeg.sampleAt(ratioFromOrigin)
          val dir = edgeSeg.body.normalize.rotate(angleCCW)
          val potentialEnd = at + dir * maxLength
          val raySeg = SegmentPlanar(at, potentialEnd)

          val rayEndOrHitPoint = ctx.dcel.halfEdges.flatMap(he => ctx.dcel.asSegment(he).intersection(raySeg)) flatMap {
            case PointIntersection(p) => Seq(p)
            case SegmentIntersection(SegmentPlanar(a, b)) => Seq(a, b)
          } filter {
            x => x.distance(at) !~= 0d
          } minByOption {
            x => x.distance(at)
          } getOrElse potentialEnd

          val path = ctx.dcel.cutFromTo(at, rayEndOrHitPoint, context.provider)
          //if(path.size < 2) throw new MalformedDCELException() //todo
          val start = path(0)
          val end = path(1)
          val lEdge = start.edgeTo(end).get
          val rEdge = lEdge.twin

          val newVertexLabels =
            atLabel.map(l => (l, lEdge.origin)).toSeq ++
              rayEndOrHitPointLabel.map(l => (l, lEdge.ending)).toSeq
          val newFaceLabels =
            lFaceLabel.map(l => (l, lEdge.leftFace)).toSeq ++
              rFaceLabel.map(l => (l, rEdge.leftFace)).toSeq
          val newEdgeLabels =
            fHalfLabel.map(l => (l, lEdge.prev)).toSeq ++
              sHalfLabel.map(l => (l, rEdge.next)).toSeq ++
              lEdgeLabel.map(l => (l, lEdge)).toSeq ++
              rEdgeLabel.map(l => (l, rEdge)).toSeq

          ctx = ctx
            .addFaceLabels(newFaceLabels)
            .addVertexLabels(newVertexLabels)
            .addEdgeLabels(newEdgeLabels)
        }
        ctx
    }
  }

  def cutPoly[D <: DCELData, L <: Labels](c: CutPoly[D, L], context: CuttingContext[D, L]): CuttingContext[D, L] =
    c match {
      case CutPoly(poly, insideLabel, edgeLabels, edgeTwinLabels, vertexLabels) => {
        val newEdges = mutable.Buffer[HalfEdge[D]]()

        //todo generify these subscriprions for vertives and areas
        val list = context.dcel.onNewHalfEdge.subscribe(newEdges.addOne)
        val result = context.dcel.cutPoly(poly.vertices, context.provider)
        context.dcel.onNewHalfEdge.unSubscribe(list)

        if (result.nonEmpty) {
          val toLabelEdges = DCELOps.toChain(result)
          val toAddLabelAreas = DCELOps.selectToTheLeft(toLabelEdges.toSeq)//result.flatMap(edges => edges.headOption.map(_.leftFace.data))
          val toAddAreaLabels = c.insideLabel.toSeq.flatMap(l => toAddLabelAreas.map(a => (l, a)))

          val segmentAndLabel = (c.poly.vertices :+ c.poly.vertices.head).sliding(2)
            .map { case Seq(f, s) => SegmentPlanar(f, s) }
            .zip(c.edgeLabels).toSeq
          //label edges
          val toAddEdgeLabels = toLabelEdges.flatMap(e =>
            segmentAndLabel
              .find { case (s, l) => s.containsSegment(context.dcel.asSegment(e))}
              .map { case (_, l) => (l, e) })

          context
            .addEdges(newEdges)
            .addFaces(toAddLabelAreas.toSeq)
            .addFaceLabels(toAddAreaLabels)
            .addEdgeLabels(toAddEdgeLabels.toSeq)
        } else context
      }
    }

  def cutChain[D <: DCELData, L <: Labels](c: CutChain[D, L], context: CuttingContext[D, L]): CuttingContext[D, L] = {
    ???
  }

  def mergeFaces[D <: DCELData, L <: Labels](m: MergeFaces[D, L], context: CuttingContext[D, L]): CuttingContext[D, L] = {
    m match {
      case MergeFaces(mainFaceSelector, toMereFaceSelector, resultingFaceLabel) =>
        ???
    }
  }

  def connectVertices[D <: DCELData, L <: Labels](c: ConnectVertices[D, L], context: CuttingContext[D, L]): CuttingContext[D, L] = {
    c match {
      case ConnectVertices(mainVertexSelector, toConnectVertexSelector, edgeLabel, twinEdgeLabel) =>
        ???
    }
  }



}
