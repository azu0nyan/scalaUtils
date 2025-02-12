package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{Face, HalfEdge, MalformedDCELException, Vertex}
import utils.datastructures.dcel.PlanarDCELCutPipeline.{ConnectVertices, CutChain, CutPoly, CuttingContext, Labels, MergeFaces, TraceSegmentAtAngle}
import utils.math.planar.{PointIntersection, SegmentIntersection, SegmentPlanar, V2}
import utils.math.{WithAlmostEquals, compare}

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
  def traceSegmentAtAngle[VD, HD, FD, VL, HL, FL](
                                                   t: TraceSegmentAtAngle[VD, HD, FD, VL, HL, FL],
                                                   context: CuttingContext[VD, HD, FD, VL, HL, FL]): CuttingContext[VD, HD, FD, VL, HL, FL] = {
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
          //todo fix for collinear Segments begining at 'at'
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

  /** because during cut one input edge can result in several edges,
   * we need to group input edge with list of vertices that edge produced,
   * return groupped edges in same order as input edges */
  def group[VD, HD, FD](originalVertices: Seq[V2], result: Seq[Vertex[VD, HD, FD]], ex: Vertex[VD, HD, FD] => V2, circullar: Boolean): Seq[Seq[Vertex[VD, HD, FD]]] = {
    val grouped: mutable.Buffer[Seq[Vertex[VD, HD, FD]]] = mutable.Buffer()
    var curGroup: Seq[Vertex[VD, HD, FD]] = Seq()
    var resultLeft = result
    var originalLeft = originalVertices.tail //skip first
    while (resultLeft.nonEmpty && originalLeft.nonEmpty) {
      val cur = resultLeft.head //poll
      resultLeft = resultLeft.tail

      if (ex(cur) ~= originalLeft.head) { //new group
        grouped += curGroup
        curGroup = Seq(cur)
        originalLeft = originalLeft.tail
      } else {
        curGroup = curGroup :+ cur
      }
    }
    grouped += curGroup ++ resultLeft

    //add first vertes of group to previous to complete edges in group
    if (circullar) {
      for (i <- grouped.indices) {
        grouped(i) = grouped(i) :+ grouped((i + 1) % grouped.size).head
      }
    } else {
      for (i <- grouped.indices.dropRight(1)) {
        grouped(i) = grouped(i) :+ grouped(i + 1).head
      }
    }

    grouped.toSeq
  }

  def cutPoly[VD, HD, FD, VL, HL, FL](
                                       c: CutPoly[VD, HD, FD, VL, HL, FL],
                                       context: CuttingContext[VD, HD, FD, VL, HL, FL]
                                     ): CuttingContext[VD, HD, FD, VL, HL, FL] =
    c match {
      case CutPoly(poly, insideLabel, edgeLabels, edgeTwinLabels, vertexLabels) =>
        val result = context.dcel.cutPoly(poly.vertices, context.provider)

        if (result.nonEmpty) {
          var ctx = context
          val resultEdges = DCELOps.toChain(result) //toChainCircullar isn't needed because result contains end duplicate

          if (insideLabel.nonEmpty) {
            val toLabelFaces = DCELOps.selectToTheLeft(resultEdges.toSeq).toSeq
            ctx = ctx.addFaceLabels(toLabelFaces.map(f => (insideLabel.get, f)))
          }

          if (edgeLabels.nonEmpty || edgeTwinLabels.nonEmpty || vertexLabels.nonEmpty) {
            //edge can be splitted during cut so group resulting vertices by edges
            val grouped = group(poly.vertices, result.dropRight(1), context.dcel.position, true)
            //getStraightEdgePathVertex allow working with self intersected polygons
            val edgeLabelSeq = for ((label, g) <- edgeLabels.zip(grouped);
                                    Seq(start, end) <- g.sliding(2);
                                    toLabel <- ctx.dcel.getStraightEdgePathVertex(start, end)) yield (label, toLabel)
            val twinLabelsSeq = for ((label, g) <- edgeTwinLabels.zip(grouped);
                                     Seq(start, end) <- g.sliding(2);
                                     toLabel <- ctx.dcel.getStraightEdgePathVertex(end, start)) yield (label, toLabel)
            val vertexLabelSeq = for ((label, g) <- vertexLabels.zip(grouped);
                                      toLabel <- g.headOption) yield (label, toLabel)
            ctx = ctx
              .addEdgeLabels(edgeLabelSeq ++ twinLabelsSeq)
              .addVertexLabels(vertexLabelSeq)
          }
          ctx
        } else context
    }

  def cutChain[VD, HD, FD, VL, HL, FL](
                                        c: CutChain[VD, HD, FD, VL, HL, FL],
                                        context: CuttingContext[VD, HD, FD, VL, HL, FL]
                                      ): CuttingContext[VD, HD, FD, VL, HL, FL] =
    c match {
      case CutChain(chain, edgeLabels, edgeTwinLabels, vertexLabels) =>
        val result = context.dcel.cutChain(chain, context.provider)
        if (result.nonEmpty) {
          var ctx = context

          if (edgeLabels.nonEmpty || edgeTwinLabels.nonEmpty || vertexLabels.nonEmpty) {
            val grouped = group(chain, result, context.dcel.position, false)
            //getStraightEdgePathVertex allow working with self intersected chains
            val edgeLabelSeq = for ((label, g) <- edgeLabels.zip(grouped);
                                    Seq(start, end) <- g.sliding(2);
                                    toLabel <- ctx.dcel.getStraightEdgePathVertex(start, end)) yield (label, toLabel)
            val twinLabelsSeq = for ((label, g) <- edgeTwinLabels.zip(grouped);
                                     Seq(start, end) <- g.sliding(2);
                                     toLabel <- ctx.dcel.getStraightEdgePathVertex(end, start)) yield (label, toLabel)
            val vertexLabelSeq = for ((label, g) <- vertexLabels.zip(grouped);
                                      toLabel <- g.headOption) yield (label, toLabel)
            ctx = ctx
              .addEdgeLabels(edgeLabelSeq ++ twinLabelsSeq)
              .addVertexLabels(vertexLabelSeq)
          }
          ctx
        } else context


    }

  def mergeFaces[VD, HD, FD, VL, HL, FL](
                                          m: MergeFaces[VD, HD, FD, VL, HL, FL],
                                          context: CuttingContext[VD, HD, FD, VL, HL, FL]
                                        ): CuttingContext[VD, HD, FD, VL, HL, FL] = {
    m match {
      case MergeFaces(mainFaceSelector, toMereFaceSelector, resultingFaceLabel) =>
        val main = mainFaceSelector(context)
        //no adjacency check
        for (main <- mainFaceSelector(context);
             toMerge <- toMereFaceSelector(context)) context.dcel.mergeAdjancedFaces(main, toMerge, context.provider)

        if (main.nonEmpty && resultingFaceLabel.nonEmpty) context.addFaceLabels(Seq((resultingFaceLabel.get, main.get)))
        else context
    }
  }

  def connectVertices[VD, HD, FD, VL, HL, FL](
                                               c: ConnectVertices[VD, HD, FD, VL, HL, FL],
                                               context: CuttingContext[VD, HD, FD, VL, HL, FL]
                                             ): CuttingContext[VD, HD, FD, VL, HL, FL] = {
    c match { //todo vertex labels
      case ConnectVertices(centerVertexSelector, otherVertexSelector, edgeLabel, twinEdgeLabel) =>
        val edgeLabels: Seq[(HL, HalfEdge[VD, HD, FD])] =
          for (center <- centerVertexSelector(context).toSeq;
               other <- otherVertexSelector(context);
               res = context.dcel.cutFromTo(context.dcel.position(center), context.dcel.position(other), context.provider);
               Seq(s, e) <- res.sliding(2);
               edge <- s.edgeTo(e).toSeq;
               el <- edgeLabel.map(el => (el, edge)).toSeq ++ twinEdgeLabel.map(twl => (twl, edge.twin)).toSeq
               ) yield el

        context.addEdgeLabels(edgeLabels)
    }
  }


  /** Tracks created and deleted dcel parts during DCEL operation,
   * some parts can exist in both 'new' and 'deleted' lists.
   * */
  def withContextChangesTracking[VD, HD, FD, VL, HL, FL](op: CuttingContext[VD, HD, FD, VL, HL, FL] => CuttingContext[VD, HD, FD, VL, HL, FL],
                                                         context: CuttingContext[VD, HD, FD, VL, HL, FL]): CuttingContext[VD, HD, FD, VL, HL, FL] = {
    val newVertices: mutable.Set[Vertex[VD, HD, FD]] = mutable.Set[Vertex[VD, HD, FD]]()
    val newHalfEdges: mutable.Set[HalfEdge[VD, HD, FD]] = mutable.Set[HalfEdge[VD, HD, FD]]()
    val newFaces: mutable.Set[Face[VD, HD, FD]] = mutable.Set[Face[VD, HD, FD]]()

    val removedVertices: mutable.Set[Vertex[VD, HD, FD]] = mutable.Set[Vertex[VD, HD, FD]]()
    val removedHalfEdges: mutable.Set[HalfEdge[VD, HD, FD]] = mutable.Set[HalfEdge[VD, HD, FD]]()
    val removedFaces: mutable.Set[Face[VD, HD, FD]] = mutable.Set[Face[VD, HD, FD]]()


    val dcel = context.dcel
    val nvl = dcel.onNewVertex.subscribe(newVertices.addOne)
    val nhel = dcel.onNewHalfEdge.subscribe(newHalfEdges.addOne)
    val nf = dcel.onNewFace.subscribe(newFaces.addOne)

    val rvl = dcel.onVertexRemoved.subscribe(removedVertices.addOne)
    val rhel = dcel.onHalfEdgeRemoved.subscribe(removedHalfEdges.addOne)
    val rfl = dcel.onFaceRemoved.subscribe(removedFaces.addOne)

    val result: CuttingContext[VD, HD, FD, VL, HL, FL] = op(context)

    dcel.onNewVertex.unSubscribe(nvl)
    dcel.onNewHalfEdge.unSubscribe(nhel)
    dcel.onNewFace.unSubscribe(nf)

    dcel.onVertexRemoved.subscribe(rvl)
    dcel.onHalfEdgeRemoved.subscribe(rhel)
    dcel.onFaceRemoved.subscribe(rfl)

    result
      .addFaces(newFaces)
      .addHalfEdges(newHalfEdges)
      .addVertices(newVertices)
      .removeFaces(removedFaces)
      .removeHalfEdges(removedHalfEdges)
      .removeVertices(removedVertices)
  }


}
