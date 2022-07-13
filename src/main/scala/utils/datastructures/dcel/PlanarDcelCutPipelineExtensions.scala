package utils.datastructures.dcel

trait PlanarDcelCutPipelineExtensions {



  /*
   //use enum when scala3
   sealed trait OffsetFaceType
   case class AngleBisectors(offsetValue: Scalar) extends OffsetFaceType
   case class OffsetSidesAlongNormal(offsetValue: Scalar) extends OffsetFaceType
   case class OffsetFace[D <: DCELData, L <: Labels](faceSelector: FaceSelector[D, L],
                                                     offsetType: OffsetFaceType,
                                                     insideFaceLabel: Option[L#FaceLabel] = None
                                                    ) extends PlanarDCELCutPipeline[D, L]

   sealed trait OffsetEdgeType
   case class SimpleOffsetAlongNormal(offsetValue: Scalar) extends OffsetEdgeType
   case class OffsetAlongNormalAndTraceToFaceBorder(offsetValue: Scalar) extends OffsetFaceType
   case class OffsetEdge[D <: DCELData, L <: Labels](edgeSelector: EdgeSelector[D, L],
                                                     offsetEdgeType: OffsetEdgeType,
                                                     edgeLabel: Option[L#HalfEdgeLabel] = None,
                                                     twinEdgeLabel: Option[L#HalfEdgeLabel] = None) extends PlanarDCELCutPipeline[D, L]

   case class TracePath(path: Path, subdivisions: Int)*/

/*

  def splitAtRatios(selector: EdgeSelector, ratios: Seq[Scalar]): AreaFillingPipeline = {
    val randomSuffix = new Random().nextLong().toHexString
    def labelFromId(id: Int): EdgeLabel = id.toString + randomSuffix
    /*
    s = r1 + r2 + r3
          r1        r2             r3                      rn
    |---------|----------------|------------|-..........-|--------|
             split_1
     split_1 = r1 / s
     split_2 = r2 / (s - r1)
     ...
     split_i = ri / (s - sum(r1..r_{i-1}))

     */

    val fullSum = ratios.sum
    val recalculatedRatios = ratios
      .dropRight(1)
      .foldLeft((fullSum, Seq[Scalar]())) {
        case ((sum, res), cur) => (sum - cur, res :+ cur / sum)
      }._2

    val markFirstEdges: AreaFillingPipeline = TransformContext(ctx => ctx.addEdgeLabels(selector(ctx).map(e => ("0" + randomSuffix, e))))
    val traceSegments: Seq[AreaFillingPipeline] = recalculatedRatios.zipWithIndex.map {
      case (ratio, i) =>
        val selectLabel = labelFromId(i)
        val nextEdgeLabel = labelFromId(i + 1)
        TraceSegmentAtAngle(ctx => ctx.edgesLabels(selectLabel).toSeq, ratio, 10000, HALF_PI, rEdgeLabel = Some(nextEdgeLabel))
    }
    val steps: Seq[AreaFillingPipeline] = markFirstEdges +: traceSegments
    StepsSeq(steps: _*)
  }
*/

/*


  def CutPolyAtCenter(poly: PolygonRegion, insideLabel: Option[AreaLabel] = None, edgeLabels:Seq[EdgeLabel] = Seq()): AreaFillingPipeline =
    ProducePipeline(ctx =>
      CutPoly(poly.map(x => x + ctx.area.outerPoly.center), insideLabel, edgeLabels)
    )
  /*
           *<-----*
           |     /\
           |     |
          \/     |
          *--*-->*     pEdge


  <----------*----------- edgeSeg
            center

   */
  def CutPolyOnEdge(selector: EdgeSelector, poly: PolygonRegion, insideLabel: Option[AreaLabel] = None, edgeLabels:Seq[EdgeLabel] = Seq()): AreaFillingPipeline =
    ProducePipeline { ctx =>
      StepsSeq(
        (for( edge <- selector(ctx)) yield {
          val edgeSeg = edge.asSegment
          val pEdge = poly.sides.head
          val translation  =  edgeSeg.center - pEdge.center
          val rotation = AngleOps.angleFromTo(pEdge.body, edgeSeg.body.opposite)//todo why not edgeSeg.body.opposite
          CutPoly(poly.map(v => v + translation).rotate(rotation, edgeSeg.center), insideLabel, edgeLabels)
        }
          ):_*)
    }
*/

}
